-module(ofs_store_rest_tests).

-include_lib("eunit/include/eunit.hrl").

ofs_store_rest_test_() ->
    {setup,
     fun() -> ok end,
     [
        fun test_me/0
     ]
    }.

test_me() ->
    ?assert(true). 