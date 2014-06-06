-module(trace_info).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("elmon/include/elmon.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_event callbacks
-export([
    init/1,
    terminate/2,
    handle_info/2
    ]).

-export([
    test_crash/0,
    test_sleep_tracing/0,
    test_recursive/0
]).

-define(SERVER, ?MODULE).

-record(state, {
    trace_target,
    call_records
}).

start_link(TraceTargets) ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, TraceTargets, []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init(TraceTargets) ->
    erlang:process_flag(trap_exit, true),
    toggle_trace(true),

    lists:foreach(
        fun({Module, _, _}) ->
            case code:is_sticky(Module) of
                true -> ok;
                false ->
                    code:load_file(Module)
            end
        end,
        TraceTargets
    ),

    MatchSpec = dbg:fun2ms(fun(_)->
        exception_trace()
    end),
    toggle_trace_pattern(MatchSpec, TraceTargets),
    {ok, #state{
        trace_target = TraceTargets,
        call_records = ets:new(call_records, [private])}
    }.

terminate(
    _Reason,
    #state{
        trace_target = TraceTargets,
        call_records = CallRecords
    }
) ->
    ets:delete(CallRecords),
    toggle_trace_pattern(false, TraceTargets),
    toggle_trace(false),
    ok.

handle_info(
    {trace_ts, Pid, call, {Mod, Fun, ArgList}, Timestamp},
    #state{call_records = CallRecords} = State
) ->
    Key = {Pid, {Mod, Fun, length(ArgList)}},
    true = ets:insert_new(CallRecords, {Key, Timestamp}),
    {noreply, State};
handle_info(
    {trace_ts, Pid, return_from, MFA,
        ReturnValue, FinishTimestamp},
    #state{call_records = CallRecords} = State
) ->
    handle_finish(
        Pid, MFA, FinishTimestamp,
        #return_value{value = ReturnValue},
        CallRecords
    ),
    {noreply, State};
handle_info(
    {trace_ts, Pid, exception_from, MFA,
        ErrorInfo, FinishTimestamp},
    #state{call_records = CallRecords} = State
) ->
    handle_finish(
        Pid, MFA, FinishTimestamp,
        #exception{description = ErrorInfo},
        CallRecords),
    {noreply, State};
handle_info({'EXIT', _, Reason}, State) ->
    {stop, Reason, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


handle_finish(Pid, {Mod, Fun, Arity}, FinishTimestamp, Return, CallRecords) ->
    Key = {Pid, {Mod, Fun, Arity}},
    StartTimestamp = ets:lookup_element(CallRecords, Key, 2),
    ets:delete(CallRecords, Key),
    MonitorInfo = {
        Return,
        timer:now_diff(FinishTimestamp, StartTimestamp)
    },
    reporter:notify(MonitorInfo).


toggle_trace(Enabled) ->
    0 = erlang:trace(new, Enabled, [call, timestamp]).

toggle_trace_pattern(MatchSpec, TraceTargets) ->
    lists:foreach(
        fun(MFA) ->
            1 = erlang:trace_pattern(MFA, MatchSpec, [global])
        end,
        TraceTargets
    ).

%%%===================================================================
%%% Tests
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

-define(MSG_WAIT_TIMEOUT, 100).

cleanup_test() ->
    TracedFunction = {timer, sleep, 1},
    {ok, TracePid} = start_link([TracedFunction]),
    {ok, TestPid} = gen_event:start_link(),

    ?assertEqual(
        {tracer, TracePid},
        erlang:trace_info(TestPid, tracer)
    ),
    ?assertEqual(
        {traced, global},
        erlang:trace_info(TracedFunction, traced)
    ),

    unlink(TracePid),
    exit(TracePid, stop),

    timer:sleep(?MSG_WAIT_TIMEOUT),

    ?assertEqual(
        {tracer, []},
        erlang:trace_info(TestPid, tracer)
    ),
    ?assertEqual(
        {traced, false},
        erlang:trace_info(TracedFunction, traced)
    ).


trace_test_() ->
    {
        setup,
        fun() ->
            reporter:start_link(),
            msg_accumulator:start(),
            start_link([
                {msg_accumulator, sleep, 1},
                {msg_accumulator,crashing_function, 0},
                {msg_accumulator, recursive_sleep, 1}
            ])
        end,
        fun({ok, TracePid}) ->
            unlink(TracePid),
            exit(TracePid, stop),
            ReporterPid = whereis(reporter),
            unlink(ReporterPid),
            exit(ReporterPid, stop)
        end,
        fun(_) ->
            [
                fun test_sleep_tracing/0,
                fun test_crash/0,
                fun test_recursive/0
            ]
        end
    }.

test_crash() ->
    spawn(msg_accumulator, crashing_function, []),
    timer:sleep(?MSG_WAIT_TIMEOUT),
    ?assertMatch(
        {value,{#exception{description = {error,{nocatch,aborted}}},_}},
        msg_accumulator:get_message()
    ),
    ?assertEqual(
        empty,
        msg_accumulator:get_message()
    ).

test_sleep_tracing() ->
    msg_accumulator:sleep(1000),
    timer:sleep(?MSG_WAIT_TIMEOUT),
    ?assertMatch(
        {value, {#return_value{value = ok}, V}} when V >= 1000000,
        msg_accumulator:get_message()
    ),
    ?assertEqual(
        empty,
        msg_accumulator:get_message()
    ).

test_recursive() ->
    msg_accumulator:recursive_sleep(2),
    timer:sleep(?MSG_WAIT_TIMEOUT),
    ?assertMatch(
        {value, {#return_value{value = ok}, V}} when V >= 2000000,
            msg_accumulator:get_message()
    ),
    ?assertEqual(
        empty,
        msg_accumulator:get_message()
    ).


