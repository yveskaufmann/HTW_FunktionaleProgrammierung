-module(u4).
-export([
	create_tree/1,
	insert/3,
	height/1,
	create_traverse/1,
	main/1
]).

-record(node, {
	key, 
	value, 
	left = nil, 
	right = nil
}).

% Check if a Number exists inside a list
insert(Key, Value, nil) -> #node{value= Value, key= Key};
insert(Key, Value, Node) when Key == Node#node.key ->
	Node#node{value = Value};
insert(Key, Value, Node) when Node#node.key < Key ->
	Node#node{right= insert(Key, Value, Node#node.right)};
insert(Key, Value, Node) ->
	Node#node{left=insert(Key, Value, Node#node.left)}.


create_tree(L) -> create_tree(L,nil).
create_tree([], Node) -> Node;
create_tree([X|XS], Node) -> create_tree(XS, insert(X,X, Node)).

height(Node) -> round(math:log2(height_r(Node, 0))).
height_r(nil, N) -> N;
height_r(Node, N) -> height_r(Node#node.left, 1 + height_r(Node#node.right, N)).

create_traverse([_|_]=StrategieSteps) -> 
	% Converts the strategie atoms to a strategie step list	
	Strategie = lists:map(fun
		(n) -> 
			fun(Node, Visitor, _) -> Visitor(Node) end;
		(l) ->
			fun(Node, Visitor, Traverse) -> Traverse(Node#node.left, Visitor) end;
		(r) ->
			fun(Node, Visitor, Traverse) -> Traverse(Node#node.right, Visitor) end
	end, StrategieSteps),
	
	% Builds the concrete tree traverser						
	fun Traverse(nil, _) -> ok;
		Traverse(Node, Visitor) -> 
			lists:foreach(fun(Step) -> 
				Step(Node, Visitor, Traverse) 
			end, Strategie)			
	end.

widthTraverse([]) -> ok;
widthTraverse(Node) when not is_list(Node) -> widthTraverse([Node]);
widthTraverse([nil|NextNodes]) -> widthTraverse(NextNodes);
widthTraverse([CurrentNode|NextNodes]) ->
	io:format("~b ", [CurrentNode#node.value]),
	widthTraverse(NextNodes ++ [CurrentNode#node.left, CurrentNode#node.right]).


widthTraverse_p([]) -> ok;
widthTraverse_p(Node) when not is_list(Node) -> widthTraverse_p([Node]);
widthTraverse_p([nil|NextNodes]) -> widthTraverse_p(NextNodes);
widthTraverse_p(NodeList) ->
	CurrentNode = lists:last(NodeList),
	io:format("~b ", [CurrentNode#node.value]),
	widthTraverse_p([CurrentNode#node.right | [CurrentNode#node.left | lists:droplast(NodeList)]]).


findSuccessor(#node{left=Left, right=Right, key=Key}) ->
	Right#node.key.

main(_) ->
	TestTree = create_tree([10, 6, 14, 2, 7, 12 , 17]),
	TraverseSorted = create_traverse([l, n, r]),
	DeepTraverse = create_traverse([n, r, l]),
	
	io:fwrite('TraverseSorted: '),
	TraverseSorted(TestTree, fun(NodeValue) -> io:format('~b ', [NodeValue#node.value]) end),
	io:format('~n'),	

	io:fwrite('DeepTraverse: '),
	DeepTraverse(TestTree, fun(NodeValue) -> io:format('~b ', [NodeValue#node.value]) end),
	io:format('~n'),	
	
	io:fwrite('WidthTraverse: '),
	widthTraverse_p(TestTree),
	io:format('~n~n'),	
	done.

