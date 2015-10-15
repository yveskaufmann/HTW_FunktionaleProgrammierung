-module(test).
-export([
	add/2,
	gender/2,
	und/2,
	convert/1,
	hello/1,
	curry/2
]).

add(A, B) -> 
	A + B.

gender(male, Name) ->
	io:format('Hello, Mr. ~s!', [Name]);

gender(female, Name) ->
	io:format('Hello, Mrs. ~s!', [Name]);

gender(_, Name) ->
	io:format('Hello, Mr. ~s!', [Name]).


und(true, true) ->
	true;
und(false, _) -> 
	false;
und(_, false) ->
	false;
und(_,_) ->
	io:format("Invalid arguments: booleans required").


convert({fahrenheit, Temperature}) ->
	{fahrenheit, Temperature * 1.8 + 32};
convert({reamur, Temperature }) ->
	{reamur, Temperature * 0.52500 + 7.50 };
convert({kelvin, Temperature }) ->
	{kelvin, Temperature + 273.15 };
convert({_,_}) ->
	io:format("Invalid unit specified").

hello(0) ->
	"zurück";
hello(X) when X >= 0 ->
	io:fwrite("~s", ["Hello World\n"]),
	hello(X - 1);
hello(_)  ->
	error.

curry(Fun, Step) ->
	fun(X) ->
		Fun(X, Step)
	end.

