-module(trace_info).

-include_lib("stdlib/include/ms_transform.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_event callbacks
-export([
    init/1,
    terminate/2,
    handle_info/2
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
    toggle_trace(true),

    lists:foreach(
        fun({Module, _, _}) ->
            case code:is_sticky(Module) of
                true -> ok;
                false ->
                    {module, Module} = code:load_file(Module)
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
    {trace_ts, Pid, call, {Mod, Fun, ArgList}, Timestamp} = Info,
    #state{call_records = CallRecords} = State
) ->
    error_logger:info_msg("call: ~p~n", [Info]),
    Key = {Pid, {Mod, Fun, length(ArgList)}},
    true = ets:insert_new(CallRecords, {Key, Timestamp}),
    {noreply, State};
handle_info(
    {trace_ts, Pid, return_from, {Mod, Fun, Arity},
        ReturnValue, FinishTimestamp} = Info,
    #state{call_records = CallRecords} = State
) ->
    error_logger:info_msg("return: ~p~n", [Info]),
    Key = {Pid, {Mod, Fun, Arity}},
    StartTimestamp = ets:lookup_element(CallRecords, Key, 2),
    ets:delete(CallRecords, Key),
    MonitorInfo = {
        ReturnValue,
        timer:now_diff(FinishTimestamp, StartTimestamp)
    },
    error_logger:info_msg("monitor: ~p~n", [MonitorInfo]),
    reporter:notify(MonitorInfo),
    {noreply, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

toggle_trace(Enabled) ->
    0 = erlang:trace(new, Enabled, [call, timestamp]).

toggle_trace_pattern(MatchSpec, TraceTargets) ->
    lists:foreach(
        fun(MFA) ->
            1 = erlang:trace_pattern(MFA, MatchSpec, [local])
        end,
        TraceTargets
    ).

%%%===================================================================
%%% Tests
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

cleanup_test() ->
    timer:sleep(0),
    {ok, TracePid} = start_link([{timer, sleep, 1}]),
    {ok, TestPid} = gen_event:start_link(),

    ?assertEqual(
        {tracer, TracePid},
        erlang:trace_info(TestPid, tracer)
    ),

    unlink(TracePid),
    exit(TracePid, kill),
    ?assertEqual(
        {tracer, []},
        erlang:trace_info(TestPid, tracer)
    ).

trace_test_() ->
    {
        setup,
        fun() ->
            reporter:start_link(),
            msg_accumulator:start(),
            start_link([{timer, sleep, 1}])
        end,
        fun(_) ->
            [
                fun test_sleep_tracing/0
            ]
        end
    }.

test_sleep_tracing() ->
    timer:sleep(1000),
    timer:sleep(1000),
    ?assertMatch(
        {value, {ok, V}} when V >= 1000000,
        msg_accumulator:get_message()
    ).
