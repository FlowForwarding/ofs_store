-module(ofs_store_rest_tests).

-include_lib("eunit/include/eunit.hrl").

ofs_store_rest_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        fun add_switch/0,
        fun get_switch/0
     ]
    }.

setup() ->
    ofs_store_app:start_deps().

cleanup(_) ->
    ok.

add_switch() ->
    %% No Graph  ( POST )
    SendingJSON = 
    {
        [
            {<<"datapath_id">>, <<"00:00:08:00:27:55:C4:25">>},
            {<<"labels">>,      {[{<<"ip">>,<<"10.151.1.68">>}]} },
            {<<"alias">>,  <<"1">>}
        ]
    },
    EncodedJSON = jiffy:encode(SendingJSON),
    {ok,"200",RespHeaders,Body} = ibrowse:send_req("http://127.0.0.1:8080/switches", [{content_type,"application/json"}], post, EncodedJSON),
    {ok,[{GraphName,_DG}]} = dg:graphs(),
    ?assertEqual(lists:flatten(io_lib:format("~p",[GraphName])),Body),
    true = dg:del_graph(GraphName),

    %% With GraphName ( POST )
    SendingJSON2 = 
    {
        [
            {<<"graph_name">>,  <<"test_graph">>},
            {<<"datapath_id">>, <<"00:00:08:00:27:55:C4:25">>},
            {<<"labels">>,      {[{<<"ip">>,<<"10.151.1.68">>}]} },
            {<<"alias">>,  <<"1">>}
        ]
    },
    EncodedJSON2 = jiffy:encode(SendingJSON2),
    {ok,"200",RespHeaders2,Body2} = ibrowse:send_req("http://127.0.0.1:8080/switches", [{content_type,"application/json"}], post, EncodedJSON2),
    {ok,[{<<"test_graph">>,_DG2}]} = dg:graphs(),
    ?assertEqual("\"true\"",Body2),
    true = dg:del_graph(<<"test_graph">>),

    %% Add vertex with Graphname ( PUT )

    SendingJSON3 = 
    {
        [
            {<<"graph_name">>,  <<"test_graph">>},
            {<<"datapath_id">>, <<"00:00:08:00:27:55:C4:25">>},
            {<<"labels">>,      {[{<<"ip">>,<<"10.151.1.68">>}]} },
            {<<"alias">>,  <<"1">>}
        ]
    },
    EncodedJSON3 = jiffy:encode(SendingJSON3),
    {ok,"200",RespHeaders3,Body3} = ibrowse:send_req("http://127.0.0.1:8080/switches", [{content_type,"application/json"}], put, EncodedJSON3),
    {ok,[{<<"test_graph">>,_DG3}]} = dg:graphs(),
    ?assertEqual("\"true\"",Body3),
    true = dg:del_graph(<<"test_graph">>).


get_switch() ->
    %% With GraphName
    SendingJSON2 = 
    {
        [
            {<<"graph_name">>,  <<"test_graph">>},
            {<<"datapath_id">>, <<"00:00:08:00:27:55:C4:25">>},
            {<<"labels">>,      {[{<<"ip">>,<<"10.151.1.68">>}]} },
            {<<"alias">>,  <<"1">>}
        ]
    },
    EncodedJSON2 = jiffy:encode(SendingJSON2),
    {ok,"200",RespHeaders2,Body2} = ibrowse:send_req("http://127.0.0.1:8080/switches", [{content_type,"application/json"}], post, EncodedJSON2),
    {ok,[{<<"test_graph">>,_DG2}]} = dg:graphs(),
    ?assertEqual("\"true\"",Body2),

    {ok,"200",RespHeaders,Body} = ibrowse:send_req("http://127.0.0.1:8080/switch/test_graph/1", [{content_type,"application/json"}], get),
    ?assertEqual("{\"datapath_id\":\"00:00:08:00:27:55:C4:25\",\"labels\":{\"alias\":\"1\",\"ip\":\"10.151.1.68\"},\"alias\":\"1\"}",Body),

    true = dg:del_graph(<<"test_graph">>). 

del_switch() ->
    SendingJSON2 = 
    {
        [
            {<<"graph_name">>,  <<"test_graph">>},
            {<<"datapath_id">>, <<"00:00:08:00:27:55:C4:25">>},
            {<<"labels">>,      {[{<<"ip">>,<<"10.151.1.68">>}]} },
            {<<"alias">>,  <<"1">>}
        ]
    },
    EncodedJSON2 = jiffy:encode(SendingJSON2),
    {ok,"200",RespHeaders2,Body2} = ibrowse:send_req("http://127.0.0.1:8080/switches", [{content_type,"application/json"}], post, EncodedJSON2),
    {ok,[{<<"test_graph">>,_DG2}]} = dg:graphs(),
    ?assertEqual("\"true\"",Body2),

    {ok,"200",RespHeaders,Body} = ibrowse:send_req("http://127.0.0.1:8080/switch/test_graph/1", [{content_type,"application/json"}], delete),
    ?assertEqual("\"true\"",Body),

    {ok,"200",RespHeaders2,Body2} = ibrowse:send_req("http://127.0.0.1:8080/switch/test_graph/1", [{content_type,"application/json"}], get),
    ?assertEqual("\"false\"",Body2),

    {ok,"200",RespHeaders3,Body3} = ibrowse:send_req("http://127.0.0.1:8080/switch/test_graph/1", [{content_type,"application/json"}], delete),
    ?assertEqual("\"false\"",Body3),

    true = dg:del_graph(<<"test_graph">>).














