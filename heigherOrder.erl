-module(heigherOrder).
-export([map/2, main/1]).

map(_, []) -> [];
map(Fun, [X|XS]) -> [Fun(X)|map(Fun, XS)].

filter(_, []) -> [];
filter(Pred, [X|XS]) -> case Pred(X) of
	true -> [X | filter(Pred, XS)];
	_	 -> filter(Pred, XS)
end.

fold(_, Acc, []) -> Acc;
fold(Fun, Acc, [X|XS]) -> fold(Fun, Fun(X, Acc), XS).

main(_) ->
	List = fold(fun(X, Acc) -> [X rem 2 =:= 0|Acc] end ,[], [1, 5, 4, 3, 7]),
	io:format("~p ~n", [List]),
	ok.

