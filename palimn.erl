-module(palimn).
-export([main/1, is_palimdrom/1]).

is_palimdrom(N) -> 
	N_STR = hd(io_lib:format("~p", [N])),
	N_STR =:= lists:reverse(N_STR).

find_palindrom() -> find_palindrom(900, 999).
find_palindrom(Min, Max) -> find_palindrom(Min, Max, 0, Min).
find_palindrom(_, Min, P, Min) -> P;
find_palindrom(K, K, P, Min) -> find_palindrom(Min, K - 1, P, Min);
find_palindrom(I, K, P, Min) -> 
	Number = I * K,
	case is_palimdrom(Number) of
		true ->  find_palindrom(I + 1, K, max(Number, P), Min); 
		false -> find_palindrom (I + 1, K, P, Min)
	end.

main(_) ->	
	 io:format('~p ~n', [find_palindrom()]),
	done.
