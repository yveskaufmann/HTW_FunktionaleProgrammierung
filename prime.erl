-module(prime).
-export([
	is_prime/1,
	find_prime/1,
	find_10001th_prime_number/0,
	main/1
]).

is_prime(2) -> true;
is_prime(X) when (X == 1) or (X rem 2 == 0) -> false;
is_prime(X) -> is_prime(X, 3, math:sqrt(X)).
is_prime(_, N, RANGE) when N > RANGE -> true;
is_prime(X, N, _) when X rem N == 0 -> false;
is_prime(X, N, RANGE) -> is_prime(X, N + 2, RANGE).

find_prime(N) when N =< 0 -> error_N_must_be_greater_than_0;
find_prime(N) 	-> find_prime(1, N).
find_prime(I, 0) -> I - 1;
find_prime(I, N) -> find_prime(I, N, is_prime(I)).
find_prime(I, N, true) -> find_prime(I + 1, N - 1);
find_prime(I, N, false) -> find_prime(I + 1 , N).

find_10001th_prime_number() ->
	find_prime(10001).


main(_) ->
	find_10001th_prime_number().

