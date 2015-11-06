-module(homework2).
-export([flatList/1, main/1]).

%% Option 1. with foldl
flatlist(List) when is_list(List) ->
	lists:foldl(fun (X, L) -> L ++ flatlist(X)  end, [], List);
flatlist(List) -> [List].

%% Option 2. Vanilla
flatList(List) when is_list(List) -> flatList([], List);
flatList(List) -> flatList([List], []).
flatList(ACC, []) -> ACC;
flatList(ACC, [H|T]) -> flatList(ACC ++ flatList(H),T).

main(_) ->
	FlattedList = flatlist([1, [1,3], [], [5, [6,7]]]),
	io:format("FlattedList: ~p ~n", [FlattedList]),
	ok.
