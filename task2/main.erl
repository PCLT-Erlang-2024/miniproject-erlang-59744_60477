-module(main).

%% API
-export([start/0, c2f/1, f2c/1]).

c2f(C) -> (C * 9) / 5 + 32.
f2c(F) -> (F - 32) * 5 / 9.

start() ->
  io:format('100 fahrenheit to centigrade: ~p~n', [f2c(100)]).
