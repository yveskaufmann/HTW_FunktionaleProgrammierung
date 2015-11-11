-module(palimn_tests).
-include_lib("eunit/include/eunit.hrl").

is_palimdrom_test_() ->
	[ ?_assertEqual(true, palimn:is_palimdrom(121)),
	  ?_assertEqual(false, palimn:is_palimdrom(125))].
