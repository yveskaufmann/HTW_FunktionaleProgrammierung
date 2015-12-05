-module(hFunc).
-compile(export_all).
-include("../print.hrl").


mpList([]) -> [];
mpList(L) when is_list(L) -> lists:map(fun(X) -> mpList(X) end, L);
mpList(N) when is_number(N) -> lists:map(fun(X) -> N * X end, lists:seq(1, 10)).

is_prime(N) -> 
	NSQRT = round(math:sqrt(N)),
	if
		N  < 2 -> false;
		N =:= 2 -> true; 
		N rem 2 =:= 0 -> false;
		true -> length(lists:filter(fun(X) -> N rem X == 0  end, lists:seq(3 ,NSQRT, 2)))  == 0
	end.

group_by(Fun, List) -> lists:foldl(fun(X, G) -> dict:append_list(Fun(X), [X], G) end, dict:new(), List).

crTuple(X) when not is_list(X) -> crTuple([X]);
crTuple(L) -> [{X,X} || X <- L, (X+X) rem 3 =:= 0].
crTuple(L, I) -> [{X1, X2, X3} || X1 <- L, X2 <- lists:seq(1, I), X3 <- lists:seq(1, I * 2)].

crTupleHO(L, I) -> lists:flatmap(fun(X) -> 
	lists:flatmap(fun(Y) -> lists:map(fun(Z) -> {X,Y,Z} end, lists:seq(1, I * 2)) end, lists:seq(1, I)) end, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Y =  []  ==> Qi = i, i..3  , X = 0
%%% [] ++ [1] = [1] \
%%% [] ++ [2] = [2]  ==> [[1], [2], [3]] ==> [1, 2, 3]
%%% [] ++ [3] = [3] /
%%% 
%%% Y = [[1], [2], [3]], X = 1
%%% [1] ++ [1] = [1, 1] \
%%% [1] ++ [2] = [1, 2]  \
%%% [1] ++ [3] = [1, 3]   \
%%% [2] ++ [1] = [2, 1]    \
%%% [2] ++ [2] = [2, 2]     ==> [[1,1], [1,2], [1,3], [2,1], [2, 2], [2, 3], [3, 1], [3, 2], [3,3]]
%%% [2] ++ [3] = [2, 3]    /
%%% [3] ++ [1] = [3, 1]   /
%%% [3] ++ [2] = [3, 2]  /
%%% [3] ++ [3] = [3, 3] /
%%%
%%%
recListV(0) -> [[]];
recListV(X) -> [Y ++ [Q] || Y <- recList(X - 1), Q <- lists:seq(1,3) ].

recList(0) -> [[]];
recList(X) -> lists:flatmap(fun(Y) -> lists:map(fun(Q) -> Y ++ [Q] end, lists:seq(1, 3)) end, recList(X - 1)).


permutations([]) -> [[]];
permutations(L) -> [[X|Y]|| X <- L, Y <-permutations(L -- [X]) ].

siblingsDeltaIs(Delta) -> 
	fun([]) -> false;
	   ([_]) -> true;
	   ([X1, X2|_]) when abs(X1-X2) < Delta -> false;
	   ([_|L]) -> siblingsDeltaIsThree(L)
	end.

siblingsDeltaIsThree([]) -> false;
siblingsDeltaIsThree([_]) -> true;
siblingsDeltaIsThree([X1, X2|_]) when abs(X1-X2) < 2 -> false;
siblingsDeltaIsThree([_|L]) -> siblingsDeltaIsThree(L).

t(L) -> lists:filter(siblingsDeltaIs(3), permutations(L)).

main(_) ->
	Groups = group_by(fun erlang:length/1, ["Hallo", "Das", "Die", "Der", "Ksenia", "Hello"]),
	?Print(dict:to_list(Groups)),	
	
	?Print(crTuple(["a","b","c"], 5)),
	?Print(crTupleHO(["a","b","c"], 5)),
	?Print(recList(2)), 
	?Print(recListV(2)), 
	
	?Print(permutations([1,2,3,4])),
	?Print(t(lists:seq(1,8))),
	ok.
