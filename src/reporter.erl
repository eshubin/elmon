-module(reporter).

%% API
-export([
    start_link/0,
    add_handler/2,
    notify/1
]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_event:start_link({local, ?SERVER}).

notify(Info) ->
    gen_event:sync_notify(?SERVER, Info).

add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).


%%%===================================================================
%%% Internal functions
%%%===================================================================
