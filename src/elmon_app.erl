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

