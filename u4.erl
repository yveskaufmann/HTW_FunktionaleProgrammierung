-module(u4).
-record(node, {key, value, left = nil, right = nil}).
-export([
	create_tree/1,
	insert/3,
	height/1,
	traverse_in_order/2,
	traverse_in_depth/2,
	create_traverse/1
]).
% Check if a Number exists inside a list

insert(Key, Value, nil) -> #node{value= Value, key= Key};
insert(Key, Value, Node) when Key == Node#node.key ->
	Node#node{value = Value};
insert(Key, Value, Node) when Node#node.key < Key ->
	Node#node{right= insert(Key, Value, Node#node.right)};
insert(Key, Value, Node) ->
	Node#node{left=insert(Key, Value, Node#node.left)}.


create_tree([]) -> nil;
create_tree([X|XS]) -> insert(X, X, create_tree(XS)).

height(Node) -> round(math:log2(height_r(Node, 0))).
height_r(nil, N) -> N;
height_r(Node, N) -> height_r(Node#node.left, 1 + height_r(Node#node.right, N)).

strategie(n) -> 
	fun(Node, Fun, _) -> apply(Fun, [Node#node.value]) end;
strategie(l) -> 
	fun(Node, Fun, Traverse) -> apply(Traverse, [Node#node.left, Fun]) end;
strategie(r) -> 
	fun(Node, Fun, Traverse) -> apply(Traverse,[Node#node.right, Fun]) end.

create_traverse([S1, S2, S3]) -> 
	Strategie = [strategie(S1), strategie(S2), strategie(S3)],
	fun Traverse(Node, Visitor) ->
		case Node of
			nil -> ok;
			_ -> lists:foreach(
				fun(S) -> 
					apply(S, [Node, Visitor, Traverse]) 
				end, Strategie)			
		end
	end.


