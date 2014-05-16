
-module(elmon_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(
    CHILD(I, Type, Args),
    {I, {I, start_link, Args}, permanent, 5000, Type, [I]}
).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(TraceTargets) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, TraceTargets).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(TraceTargets) ->
    {ok, { {one_for_one, 5, 10}, [
        ?CHILD(reporter, worker, []),
        ?CHILD(trace_info, worker, [TraceTargets])
    ]} }.

