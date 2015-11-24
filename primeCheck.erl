-module(primeCheck).
-export([main/1, check/1]).


check(Number) -> 1 =:= length( 
	lists:filter( 
		fun(N) -> Number rem N =:= 0 end, 
		lists:seq(1, round(math:sqrt( Number )))
	)). 
main(_) ->	
	lists:foreach(fun (Num) -> 
		io:format("Is ~p  a prime ?  ~p ~n ", [Num, check(Num)])
	end, lists:seq(1, 100)),
	done.
