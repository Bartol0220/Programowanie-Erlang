-module(lab1).
-author("bartol").

%% API
-export([helloWorld/0, power/2, factorial/1, factorial_tail/1, contains/2, contains2/2, duplicateElements/1,
  sumFloats/1, sumFloats_tail/1]).


helloWorld() ->
  io:format("Hello World~n").


power(_, 0) -> 1;
power(X, N) ->
  power(X, N-1)*X.


factorial(0) -> 1;
factorial(N) -> factorial(N-1)*N.

factorial_tail(N) -> factorial_tail(N, 1).
factorial_tail(0, F) -> F;
factorial_tail(N, F) -> factorial_tail(N-1, F*N).


contains([], _) -> false;
contains([H | T], X) ->
  case H of
    X -> true;
    _ -> contains(T, X)
  end.

contains2([], _) -> false;
contains2([H | _], X) when H == X -> true;
contains2([_ | T], X) -> contains2(T, X).


duplicateElements([]) -> [];
duplicateElements([H | T]) ->
  [H, H] ++ duplicateElements(T).


sumFloats([]) -> 0;
sumFloats([H | T]) ->
  H + sumFloats(T).

sumFloats_tail(Fl) -> sumFloats_tail(Fl, 0).
sumFloats_tail([], Sum) -> Sum;
sumFloats_tail([H | T], Sum) -> sumFloats_tail(T, Sum + H).
