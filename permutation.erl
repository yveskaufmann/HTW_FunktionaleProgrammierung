-module(permutation).
-export([
	perm/1,
	main/1
]).

perm([]) -> [[]];
perm(L) -> lists:flatmap(fun(X) -> lists:map(fun(Y) -> [X] ++ Y end, perm(L--[X]) ) end, L).

main(_) ->
	L = [1, 2, 3],
	io:format("Perms ~p: ~p ~n", [L, perm(L)]).
