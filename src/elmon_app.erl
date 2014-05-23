-module(elmon_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, TraceTargets} = application:get_env(elmon, trace_targets),
    elmon_sup:start_link(TraceTargets).

stop(_State) ->
    ok.

%%%===================================================================
%%% Tests
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

launch_test_() ->
    {
        setup,
        fun() ->
            ok = application:start(elmon)
        end,
        fun(_) ->
            ok = application:stop(elmon)
        end,
        fun(_) ->
            [
                ?_assertEqual(
                    [{specs,2},
                        {active,2},
                        {supervisors,0},
                        {workers,2}],
                    supervisor:count_children(elmon_sup)
                ),
                ?_assertMatch(
                    [
                        {trace_info,_,worker,[trace_info]},
                        {reporter,_,worker,[reporter]}
                    ],
                    supervisor:which_children(elmon_sup)
                )
            ]
        end
    }.

