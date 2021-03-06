-module(msg_accumulator).

-behaviour(gen_event).

-export([get_message/0, start/0]).

-export([crashing_function/0, sleep/1, recursive_sleep/1]).

%% gen_event callbacks
-export([
    init/1,
    handle_event/2,
    handle_call/2,
    terminate/2
]).

get_message()->
    gen_event:call(reporter, ?MODULE, get).

start() ->
    reporter:add_handler(?MODULE, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

init([]) ->
    {ok, queue:new()}.

handle_event(Message, Q) ->
    {ok, queue:in(Message, Q)}.

handle_call(get, Q) ->
    {Resp, Q2} = queue:out(Q),
    {ok, Resp, Q2}.


terminate(_Arg, _State) ->
    ok.

crashing_function() ->
    throw(aborted).

sleep(Time) ->
    timer:sleep(Time).

recursive_sleep(0) ->
    ok;
recursive_sleep(N) ->
    timer:sleep(1000),
    recursive_sleep(N-1).

