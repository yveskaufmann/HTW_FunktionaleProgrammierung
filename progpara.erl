-module(progpara).
-export([main/1]).

langParadigm() -> [{erlang, funktional}, {erlang, logisch}, {prolog, logisch}, {scala, funktional},
				  {scala, objektorientiert}, {scala, logisch},{java,objektorientiert}].

findObjLanguage() -> 
	[Lang || {Lang, objektorientiert} <- langParadigm()].

findParadigmasOfJavaAndErlang() -> 
	[Paradigma || {Lang, Paradigma} <- langParadigm(), (Lang =:=  java) or (Lang =:= scala)].

listOfParadigmas() -> 
	[{Language, [Para || {Lang, Para} <- langParadigm(), Lang == Language]} || {Language, _} <- langParadigm()].

main(_) ->
	io:format("Object Oriented Languages: ~p ~n~n", [findObjLanguage()]),
	io:format("Paradima of Java and Erlang: ~p ~n~n", [findParadigmasOfJavaAndErlang()]),
	io:format("Paradimas of languages ~p ~n~n", [listOfParadigmas()]),
	ok.
