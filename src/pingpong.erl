-module(pingpong).
-author("Admin").

%% API
-export([start/0, stop/0, play/1, ping_loop/0, pong_loop/0]).

start() ->
    register(ping, spawn(?MODULE, ping_loop, [])),
    register(pong, spawn(?MODULE, pong_loop, [])).

stop() ->
    ping ! pause,
    pong ! pause.

play(N) ->
    ping ! N.

ping_loop() ->
    receive
        pause -> ok;
        0 -> io:format("ping got 0~n"), ping_loop();
        N -> io:format("ping~n"), timer:sleep(300), pong ! N-1, ping_loop()
    after
        20000 -> ping ! pause, pong ! pause
    end.

pong_loop() ->
    receive
        pause -> ok;
        0 -> io:format("pong got 0~n"), pong_loop();
        N -> io:format("pong~n"), timer:sleep(300), ping ! N-1, pong_loop()
    after
        20000 -> ping ! pause, pong ! pause
    end.
