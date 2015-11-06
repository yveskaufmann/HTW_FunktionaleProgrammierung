-module(homework2).
-export([flatlist/1, main/1]).


flatlist(List) ->
	lists:foldl(
	  fun (X, L) -> 
		LIST = if 
			is_list(X) -> flatlist(X);
			true -> [X]
		end,
		L ++ LIST 
	  end, [], List).

main(_) ->
	FlattedList = flatlist([1, [1,3], [], [5, [6,7]]]),
	io:format("FlattedList: ~p ~n", [FlattedList]),
	ok.
