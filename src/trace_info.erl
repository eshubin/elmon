-module(trace_info).

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
    trace_target
}).

start_link(TraceTargets) ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, TraceTargets, []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init(TraceTargets) ->
    toggle_trace(true),
    toggle_trace_pattern(true, TraceTargets),
    {ok, #state{trace_target = TraceTargets}}.

terminate(_Reason, #state{trace_target = TraceTargets}) ->
    toggle_trace_pattern(false, TraceTargets),
    toggle_trace(false),
    ok.

handle_info(Info, State) ->
    error_logger:info_msg("received event: ~p", [Info]),
    reporter:notify(Info),
    {noreply, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

toggle_trace(Enabled) ->
    0 = erlang:trace(new, Enabled, [call]).

toggle_trace_pattern(Enabled, TraceTargets) ->
    lists:foreach(
        fun(MFA) ->
            1 = erlang:trace_pattern(MFA, Enabled, [call_time])
        end,
        TraceTargets
    ).

%%%===================================================================
%%% Tests
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

cleanup_test44() ->
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
            timer:sleep(0),
            start_link([{timer, sleep, 1}])
        end,
        fun(_) ->
            [
                fun test_sleep_tracing/0
            ]
        end
    }.

test_sleep_tracing() ->
    spawn(timer, sleep, [500]),
    timer:sleep(500),
    ?assertEqual(
        1,
        msg_accumulator:get_message()
    ).
