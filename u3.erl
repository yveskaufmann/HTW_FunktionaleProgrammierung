-module(u3).
-export([
    member/2,
    sel_sort/2,
    checksum/1,
	sort/1,
	sortCool/1
]).
% Check if a Number exists inside a list

member(X, [X|_]) -> true;
member(X, [_|L]) -> member(X,L);
member(_, []) -> false.

% Calcualtes the checksum of a given number.
checksum(0) -> 0;
checksum(N) -> N rem 10 + checksum(N div 10).

% Sort a number into a sorted list
sel_sort(X, L) -> sel_sort(sort_in,[],[X|L]).
sel_sort(sort_in, H,[X,N|T]) when X > N -> sel_sort(sort_in, H ++ [N],[X|T]);
sel_sort(sort_in, H, L) -> H ++ L.

% Sorts an unsorted list
sort(L) -> lists:foldl(fun sel_sort/2, [], L).

sortCool(L) -> sortCool([], L).
sortCool(N,[X|L]) -> sortCool(sel_sort(X,N), L);
sortCool(N, []) -> N.


