-module(qsort).
-author("Admin").

%% API
-export([random_elems/3, qs/1, compare_speeds/3]).

less_than(List, Arg) ->
    [X || X <- List, X < Arg].

grt_eq_than(List, Arg) ->
    [X || X <- List, X >= Arg].

qs([]) -> [];
qs([Pivot|Tail]) ->
    qs( less_than(Tail,Pivot) ) ++ [Pivot] ++ qs( grt_eq_than(Tail,Pivot) ).

random_elems(N, Min, Max) ->
    [Min + rand:uniform(Max - Min) || _ <- lists:seq(1, N)].

compare_speeds(List, Fun1, Fun2) ->
    {TimeFun1, _} = timer:tc(Fun1, [List]),
    {TimeFun2, _} = timer:tc(Fun2, [List]),
    io:format("Fun1: ~w ~n", [TimeFun1]),
    io:format("Fun2: ~w ~n", [TimeFun2]).
    % wywołanie: qsort:compare_speeds(A, fun qsort:qs/1, fun lists:sort/1).

