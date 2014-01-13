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

-define(PRIORITY, 100).
-define(TABLEID, 1).
-define(INSTRUCTIONS, [{apply_actions, [{output, 2, 32768}]}]).

create_flow() ->
    ofs_store:clear(flow_entry),
    FlowAdd = of_msg_lib:flow_add(?VERSION,
                                    [{in_port, 1}],
                                    ?INSTRUCTIONS,
                                    [{priority, ?PRIORITY}, {table_id, ?TABLEID}]),
    Update = #ofs_store_request{
        datapath_id = ?DATAPATH_ID,
        message = FlowAdd
    },
    ok = ofs_store:request(Update),
    GetFlowStats = of_msg_lib:get_flow_statistics(?VERSION, all, [], []),
    Get = #ofs_store_request{
        datapath_id = ?DATAPATH_ID,
        message = GetFlowStats
    },
    FlowStatsReply = ofs_store:request(Get),
    {flow_stats_reply, _, ReplyBody} = of_msg_lib:decode(FlowStatsReply),
    [Flow] = proplists:get_value(flows, ReplyBody),
    ?assertEqual(?TABLEID, proplists:get_value(table_id, Flow)),
    ?assertEqual(?PRIORITY, proplists:get_value(priority, Flow)),
    ?assertEqual(?INSTRUCTIONS, proplists:get_value(instructions, Flow)).

start_apps() ->
    error_logger:tty(false),
    [ok = application:start(A) || A <- ?APPS],
    ok = lager:set_loglevel(lager_console_backend, error).

stop_apps() ->
    [ok = application:stop(A) || A <- lists:reverse(?APPS)].
