-module(Fak)
-export([ifak/1])

fak(0) -> 1;
fak(n) -> n * fak(n - 1).

Fak:fak(5)
