-module(u4).
-export([
	create_tree/1,
	insert/3,
	height/1,
	create_traverse/1,
	widthTraverse/1,
	leftNode/1,
	rightNode/1,
	minNode/1,
	maxNode/1,
	findSuccessor/1,
	findNode/2,
	deleteNode/2,
	main/1
]).

-record(node, {
	key, 
	value, 
	left = nil, 
	right = nil,
	parent = nil
}).


% Check if a Number exists inside a list
insert(Key, Value, Node) -> insert(Key, Value, Node, nil).
insert(Key, Value, nil, Parent) -> #node{value= Value, key= Key, parent=Parent};
insert(Key, Value, Node, _ ) when Key == Node#node.key ->
	Node#node{value = Value};
insert(Key, Value, Node, _ ) when Node#node.key < Key ->
	Node#node{right= insert(Key, Value, Node#node.right, Node)};
insert(Key, Value, Node, _) ->
	Node#node{left=insert(Key, Value, Node#node.left, Node)}.


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


minNode(Node) when is_atom(Node) -> 
	Node;
minNode(#node{left=Left, key=Key, value=Value}) when Left =:= nil-> 
	#node{key=Key, value=Value};
minNode(#node{left=Left}) ->
	minNode(Left).

maxNode(Node) when is_atom(Node) -> 
	Node;
maxNode(#node{right=Right, key=Key, value=Value}) when Right =:= nil-> 
	#node{key=Key, value=Value};
maxNode(#node{right=Right}) ->
	maxNode(Right).

leftNode(#node{left=Left}) -> 
	Left.

rightNode(#node{right=Right}) -> 
	Right.

findNode(_, nil) -> nil;
findNode(Key, Node) when Node#node.key == Key -> Node;
findNode(Key, Node) when Node#node.key > Key ->
	findNode(Key, Node#node.left);
findNode(Key, Node) when Node#node.key < Key ->
	findNode(Key, Node#node.right).

findSuccessor(Node) -> 
	minNode(rightNode(Node)).

deleteNode(_, nil) -> nil;
deleteNode(Key, #node{left=Left, right=Right, key=K}) when (Key == K) and (Left == nil) -> Right; 
deleteNode(Key, #node{left=Left, right=Right, key=K}) when (Key == K) and (Right == nil) -> Left; 
deleteNode(Key, #node{left=Left, right=Right, key=K}=Node) when (Key == K) -> 
	#node{value=SValue, key=SKey} = findSuccessor(Node),
	#node{ left=Left, key = SKey, value = SValue, right = deleteNode(SKey, Right)};
deleteNode(Key, Node) when Key > Node#node.key ->
	Node#node{right = deleteNode(Key, Node#node.right)};
deleteNode(Key, Node) when Key < Node#node.key ->
	Node#node{left = deleteNode(Key, Node#node.left)}.

%deleteNode(Key, Node) -> deleteNode(findNode(Key, Node)).
%deleteNode(nil) -> nil;
% Case two childs
% deleteNode(#node{value=Value, key=Key, left=Left, right=Right} = Node) when (Left /=nil) and (Right /=nil) ->
%	Successor = findSuccessor(Node),
%	deleteNode(Successor),
%	Node#node{key=Successor#node.key, value=Successor#node.value};
%	% Case only one child
%deleteNode(#node{value=Value, key=Key, left=Left, right=Right} = Node) when (Left /= nil) and (Right == nil) ->
%	Node#node{key=Key, value=Value, left=Left#node.left, right=Left#node.right};
%deleteNode(#node{value=Value, key=Key, left=Left, right=Right} = Node) when (Left == nil) and (Right /= nil) ->	
%	Node#node{key=Key, value=Value, left=Right#node.left, right=Right#node.right};
%deleteNode(#node{parent=Parent, key=Key}) when (Parent /= nil) ->
%	if 
%		Key > Parent#node.key -> Parent#node{right=nil};
%		Key < Parent#node.key -> Parent#node{left=nil}
%	end;
% deleteNode(_) -> nil.

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
	widthTraverse(TestTree),
	io:format('~n~n'),
	case findSuccessor(TestTree) of
		nil -> io:fwrite("nil ~n");
		Node -> io:format("Successor: ~p~n", [Node#node.value])
	end,
	
	T = deleteNode(10, TestTree),
	io:fwrite('TraverseSorted: '),
	TraverseSorted(T, fun(NodeValue) -> io:format('~b ', [NodeValue#node.value]) end),
	io:format('~n'),	
	done.
