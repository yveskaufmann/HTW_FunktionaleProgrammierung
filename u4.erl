-module(u4).
-record(node, {key, value, left = nil, right = nil}).
-export([
	create_tree/1,
	insert/3,
	height/1
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
create_tree([X|XS]) -> insert(X, 0, create_tree(XS)).

height(Node) -> round(math:log2(height_r(Node))).
height_r(nil) -> 0;
height_r(Node) -> height_r(Node#node.left) + height_r(Node#node.right) + 1.
