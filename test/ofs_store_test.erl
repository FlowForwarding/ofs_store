-module(ofs_store_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("ofs_store/include/ofs_store.hrl").

-define(APPS, [compiler, syntax_tools, mnesia, xmerl, lager, ofs_store]).
-define(V4, 4).
-define(VERSION, ?V4).
-define(DATAPATH_ID, {0,<<8,0,39,197,149,72>>}).

all_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
             fun create_flow/0
            ,fun modify_flow/0
            ,fun modify_strict_flow/0
            ,fun delete_flow/0
            ,fun delete_strict_flow/0
            ,fun delete_all/0
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
-define(OUTPORT, 2).
-define(COOKIE, <<4:64>>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TESTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_flow() ->
    ofs_store:clear(flow_entry),
    Instructions = instructions(2),
    FlowAdd = of_msg_lib:flow_add(?VERSION,
                                    [{in_port, 1}],
                                    Instructions,
                                    [{priority, ?PRIORITY}, {table_id, ?TABLEID}]),
    request(FlowAdd),
    [Flow] = get_flows(),
    ?assertEqual(?TABLEID, proplists:get_value(table_id, Flow)),
    ?assertEqual(?PRIORITY, proplists:get_value(priority, Flow)),
    ?assertEqual(Instructions, proplists:get_value(instructions, Flow)).

modify_flow() ->
    ofs_store:clear(flow_entry),
    InPort1 = 1,
    InPort2 = 2,
    InPort3 = 3,
    Cookie1 = <<1:64>>,
    Cookie2 = <<3:64>>,
    Cookie3 = <<2:64>>,
    CookieMod = <<1:64>>,
    CookieMask = <<1:64>>,

    Match1 = match(InPort1),
    Match2 = match(InPort2),
    add_flow(InPort1, 10, Cookie1),
    add_flow(InPort2, 11, Cookie2),
    add_flow(InPort3, 13, Cookie3),
    NewInstructions = instructions(30),
    ModifyStrictFlow = of_msg_lib:flow_modify(?VERSION,
                                                [],
                                                NewInstructions,
                                                [{cookie, CookieMod},
                                                 {cookie_mask, CookieMask},
                                                 {priority, ?PRIORITY},
                                                 {table_id, ?TABLEID}]),
    ok = request(ModifyStrictFlow),
    Flow1 = find_flow(?TABLEID, ?PRIORITY, Match1, get_flows()),
    ?assertEqual(NewInstructions, proplists:get_value(instructions, Flow1)),
    Flow2 = find_flow(?TABLEID, ?PRIORITY, Match2, get_flows()),
    ?assertEqual(NewInstructions, proplists:get_value(instructions, Flow2)).

modify_strict_flow() ->
    ofs_store:clear(flow_entry),
    InPort1 = 1,
    Match1 = match(1),
    add_flow(InPort1, 10, ?COOKIE),
    add_flow(2, 20, ?COOKIE),
    NewInstructions = instructions(30),
    ModifyStrictFlow = of_msg_lib:flow_modify(?VERSION,
                                                Match1,
                                                NewInstructions,
                                                [{priority, ?PRIORITY},
                                                 {table_id, ?TABLEID},
                                                 {strict, true}]),
    ok = request(ModifyStrictFlow),
    Flow = find_flow(?TABLEID, ?PRIORITY, Match1, get_flows()),
    ?assertEqual(NewInstructions, proplists:get_value(instructions, Flow)).

delete_flow() ->
    ok.

delete_strict_flow() ->
    ok.

delete_all() ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_apps() ->
    error_logger:tty(false),
    [ok = application:start(A) || A <- ?APPS],
    ok = lager:set_loglevel(lager_console_backend, error).

stop_apps() ->
    [ok = application:stop(A) || A <- lists:reverse(?APPS)].

% send ofp_message to ofs_store
request(Msg) ->
    Request = #ofs_store_request{
        datapath_id = ?DATAPATH_ID,
        message = Msg
    },
    ofs_store:request(Request).

instructions(OutPort) ->
    [{apply_actions, [{output, OutPort, 32768}]}].

match(InPort) ->
    [{in_port, <<InPort:32>>}].

% get flows
get_flows() ->
    GetFlowStats = of_msg_lib:get_flow_statistics(?VERSION, all, [], []),
    FlowStatsReply = request(GetFlowStats),
    {flow_stats_reply, _, ReplyBody} = of_msg_lib:decode(FlowStatsReply),
    proplists:get_value(flows, ReplyBody).

find_flow(TableId, Priority, Match, Flows) ->
    [Flow] = lists:foldl(fun(F, Acc) ->
                    case proplists:get_value(table_id, F) == TableId andalso
                         proplists:get_value(priority, F) == Priority andalso
                         proplists:get_value(match, F) == Match of
                        true ->
                            [F|Acc];
                        false ->
                            Acc
                    end
                end, [], Flows),
    Flow.

% Add a simple flow
add_flow(InPort) ->
    add_flow(InPort, ?OUTPORT, ?COOKIE).

add_flow(InPort, OutPort, Cookie) ->
    FlowAdd = of_msg_lib:flow_add(?VERSION,
                                    match(InPort),
                                    instructions(OutPort),
                                    [{priority, ?PRIORITY},
                                     {cookie, Cookie},
                                     {table_id, ?TABLEID}]),
    request(FlowAdd).
