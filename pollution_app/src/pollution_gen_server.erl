-module(pollution_gen_server).
-author("Bartol").
-behavior(gen_server).

%% API
-export([init/1, handle_call/3, handle_cast/2, crash/0]).


init(Args) ->
    erlang:error(not_implemented).

handle_call({getMonitor}, _From, Value) ->
    {reply, Value, Value}.

handle_cast(Request, State) ->
    erlang:error(not_implemented).

crash() ->
    1/0.
