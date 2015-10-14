-module(u1).
-export([
	quadrat/1,
	douple/1,
	min/2,
	und/2,
	convert/2,
	hello/1
]).

quadrat(X) ->
	X * X.

douple(X) -> 
	2 * X.

min(X,Y) when X < Y -> X;
min(X,Y) when X >= Y -> Y. 

und(true, true) -> true;
und(false, _) -> false;
und(_, false) -> false;
und(_,_) -> error.

convert(fahrenheit, Temp) -> {fahrenheit, Temp * 1.8 + 32.0};
convert(reamur, Temp) -> {reamur, Temp * 0.8000};
convert(kelvin, Temp) -> {kelvin, Temp + 273.15 }.

hello(0) -> zurück;
hello(N) when N < 0 -> 
	error;
hello(N) -> 
	io:fwrite("~s", ["Hello, World!\n"]),
	hello(N - 1).


	




