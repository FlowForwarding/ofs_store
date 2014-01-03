-module(ofs_store_test).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {foreach,
        fun setup/0,
        fun cleanup/1,
        [
            fun create_flow/0
        ]
    }.

setup() ->
    ok.

cleanup(ok) ->
    ok.

create_flow() ->
    ok.
