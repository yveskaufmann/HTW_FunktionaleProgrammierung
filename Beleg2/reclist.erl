-module(reclist).
-export([
	reclist/1,
	main/1
]).


reclist(0) -> [[]];
reclist(X) -> [ Y ++ [Q] || Y <-reclist(X - 1), Q <- lists:seq(1,3)].

main(_) ->
	LIST = reclist(3),
	io:format("~p ~n", [LIST]).

