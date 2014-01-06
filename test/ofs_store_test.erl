-module(ofs_store_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("ofs_store/include/ofs_store.hrl").

-define(APPS, [compiler, syntax_tools, mnesia, xmerl, lager, ofs_store]).
-define(V4, 4).
-define(VERSION, ?V4).
-define(DATAPATH_ID, {0,<<8,0,39,197,149,72>>}).

all_test_() ->
    {foreach,
        fun setup/0,
        fun cleanup/1,
        [
            fun create_flow/0
        ]
    }.

setup() ->
    start_apps(),
    ok.

cleanup(ok) ->
    stop_apps(),
    ok.

create_flow() ->
    FlowAdd = of_msg_lib:flow_add(?VERSION,
                                    [{in_port, 1}],
                                    [{apply_actions, [{output, 2, 32768}]}],
                                    []),
    Update = #ofs_store_request{
        datapath_id = ?DATAPATH_ID,
        message = FlowAdd
    },
    ofs_store:update(Update),
    ok.

start_apps() ->
    error_logger:tty(false),
    [ok = application:start(A) || A <- ?APPS],
    ok = lager:set_loglevel(lager_console_backend, error).

stop_apps() ->
    [ok = application:stop(A) || A <- lists:reverse(?APPS)].
