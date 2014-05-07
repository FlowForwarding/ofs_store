-module (dg).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(TV,dg_vertex).
-define(TE,dg_edge).

-record(dg,{v_tbl,      %% { VertexName, DigraphVertex, Labels }
            e_tbl,      %% { EdgeName :: {VertexName1,VertexName2}, DigraphEdge, Labels } 
            digraph}).

-record(vertex,{name,vertex,labels}).
-record(edge,{name,edge,labels}).

% @doc Instantiates a new digraph and associated ETS table.
-spec new() -> {ok,#dg{}}.
new() ->
    G = digraph:new(),
    VTID=ets:new(?TV,[set,public]),
    ETID=ets:new(?TE,[set,public]),
    {ok,#dg{ v_tbl = VTID,
             e_tbl = ETID,
             digraph = G }}.

%% --- Vertices ------------------------------------------------------------------------------------------

% @doc Calls ?MODULE:add_vertex/3
-spec add_vertex(#dg{}, term()) -> true | error.
add_vertex(DG, Name) ->
    add_vertex(DG, Name,[]).


% @doc Adds Name to ETS table TID, and creates a vertex on digraph G.
-spec add_vertex(#dg{}, term(), list()) -> true | error.
add_vertex(#dg{v_tbl = VTID, digraph = G} = _DG, Name, Labels) ->
    V = digraph:add_vertex(G),
    case ets:insert_new(VTID,{Name,V,Labels}) of
        false -> true = digraph:del_vertex(G, V);
        true  -> true
    end.

%% @doc Rename a vertex
-spec rename_vertex(#dg{},term(),term()) -> true | error.
rename_vertex(#dg{v_tbl = VTID} = _DG,Name,NewName) ->
    ets_exist(VTID,Name,fun({ON,V_OR_E,Labels}) -> 
        true = ets:insert(VTID,{NewName,V_OR_E,Labels}),
        true = ets:delete(VTID, ON)
    end).

% @doc Add a label to the dg Vertex ETS Entry, Can also be used to modify a label on a vertex.
-spec add_vertex_label(#dg{},term(),term(),term()) -> true | error.
add_vertex_label(#dg{v_tbl = VTID} = _DG,VertexName,Key,Value) ->
    ets_exist(VTID,VertexName,fun({Name,Vertex,Labels}) ->
        NewLabels=lists:append(proplists:delete(Key,Labels),[{Key,Value}]),
        true = ets:insert(VTID,{Name,Vertex,NewLabels})
    end).

% @doc Get a vertex's labels from TID in ETS
-spec get_vertex_label(#dg{},term(),term()) -> list() | error.
get_vertex_label(#dg{v_tbl = VTID} = _DG,VertexName, Key) ->
    ets_exist(VTID,VertexName,fun({Name,_,Labels}) -> 
        proplists:get_value(Key, Labels) 
    end).


% @doc Get all vertex Labels
-spec get_vertex_labels(#dg{},term()) -> list() | error.
get_vertex_labels(#dg{v_tbl = VTID} = _DG,VertexName) -> 
    ets_exist(VTID,VertexName,fun({Name,_,Labels}) -> 
        Labels 
    end).

% @doc Remove a label from a Vertex TID ETS entry.
-spec remove_vertex_label(#dg{},term(),term()) -> true | error.
remove_vertex_label(#dg{v_tbl = VTID} = _DG,VertexName, Key) ->
    ets_exist(VTID,VertexName,fun({Name,Vertex,Labels}) -> 
        true = ets:insert(VTID,{Name,Vertex,proplists:delete(Key, Labels)}) 
    end).

% @doc Remove a Vertex TID ETS entry, and remove vertex from digraph Graph, also remnoved Edges stored in ETID.
-spec remove_vertex(#dg{},term()) -> true | error.
remove_vertex(#dg{ v_tbl = VTID, e_tbl = ETID, digraph = G} = _DG,VertexName) ->
    ets_exist(VTID,VertexName,fun({VN,V,L}) ->
        FromList = ets:match(ETID, {{VertexName,'$1'},'_','_'}),
        ToList   = ets:match(ETID, {{'$1',VertexName},'_','_'}),
        [ true = ets:delete(ETID, {FromVertexName,VertexName}) || [FromVertexName] <- FromList ],
        [ true = ets:delete(ETID, {ToVertexName,VertexName})   || [ToVertexName]   <- ToList ],
        true = ets:delete(VTID, VertexName),
        true = digraph:del_vertex(G, V) %% Digraph will delete the digraph edges...
    end).

%% --- Edges ---------------------------------------------------------------------------------------------

% @doc calls add_edge/4 
-spec add_edge(#dg{},term(),term()) -> true | error.
add_edge(#dg{digraph = G} = _DG,FromVertexName,ToVertexName) ->
    add_edge(#dg{digraph = G} = _DG,FromVertexName,ToVertexName,[]).

% @doc Creates a Edge on ETID, and creates a edge on the Digraph G, and adds labels to ETS and digraph.
-spec add_edge(#dg{},term(),term(),term()) -> true | error.
add_edge(#dg{v_tbl = VTID, e_tbl = ETID, digraph = G} = _DG,FromVertexName,ToVertexName,Labels) ->
    ets_exist(VTID,FromVertexName,fun({_,V1,_}) -> 
        ets_exist(VTID,ToVertexName,fun({_,V2,_}) -> 
            E = digraph:add_edge(G,V1,V2,Labels),
            case ets:insert_new(ETID,{{FromVertexName,ToVertexName},E,Labels}) of
                false -> true = digraph:del_edge(G, E);
                true  -> true
            end 
        end)
    end).

% @doc Add a label to the digraph Edge in ETS and digraph, Can also be used to modify a label on a edge. Edge needs to exist in ETS.
-spec add_edge_label(#dg{},term(),term(),term(),term()) -> true | error.
add_edge_label(#dg{v_tbl = VTID, e_tbl = ETID, digraph = G} = _DG,FromVertexName,ToVertexName,Key,Value) ->
    ets_exist(VTID,FromVertexName,fun({_,V1,_}) -> 
        ets_exist(VTID,ToVertexName,fun({_,V2,_}) -> 
            ets_exist(ETID,{FromVertexName,ToVertexName},fun({EName,E,Labels}) ->
                NewLabels=lists:append(proplists:delete(Key,Labels),[{Key,Value}]),
                true = ets:insert(ETID,{EName,E,NewLabels}),
                E = digraph:add_edge(G, E, V1, V2, NewLabels),
                true
            end)
        end)
    end).

% @doc Get edge's labels from ETS
-spec get_edge_label(#dg{},term(),term(),term()) -> true | error.
get_edge_label(#dg{e_tbl = ETID} = DG,FromVertexName,ToVertexName,Key) ->
    ets_exist(ETID,{FromVertexName,ToVertexName},fun({EName,E,Labels}) ->
        proplists:get_value(Key, Labels) 
    end).

% @doc get all edge's labels.
-spec get_edge_labels(#dg{},term(),term()) -> true | error.
get_edge_labels(#dg{e_tbl = ETID} = DG,FromVertexName,ToVertexName) ->
    ets_exist(ETID,{FromVertexName,ToVertexName},fun({EName,E,Labels}) ->
        Labels
    end).

% @doc remove edge from ETS and digraph.
-spec remove_edge(#dg{},term(),term()) -> true | error.
remove_edge(#dg{e_tbl = ETID, digraph = G} = DG,FromVertexName,ToVertexName) ->
    ets_exist(ETID,{FromVertexName,ToVertexName},fun({{VN1,VN2},E,_}) ->
        true = ets:delete(ETID, {FromVertexName,ToVertexName}),
        true = digraph:del_edge(G, E)
    end).

%% --- Internal ------------------------------------------------------------------------------------------

ets_exist(Tbl,Key,Fun) ->
    case ets:lookup(Tbl,Key) of
        []     -> error;
        [Item] -> Fun(Item)
    end.

ets_noexist(Tbl,Key,Fun) -> 
    case ets:lookup(Tbl,Key) of
        [] -> Fun();
        _  -> error
    end.

%% ---------------------------------


test() ->
    application:start(compiler),
    application:start(syntax_tools),
    application:start(lager),
    application:start(mnesia),
    application:start(ofs_store),

    {ok,DG}  = new(),
    {ok,DG2} = new(),
    #dg{ digraph = G ,v_tbl = VTID, e_tbl = ETID } = DG,
    #dg{ digraph = G2 ,v_tbl = VTID2, e_tbl = ETID2 } = DG2,
    
    ?assertEqual(true,add_vertex(DG, "Vertex0")),
    ?assertEqual(true,add_vertex(DG, "Vertex1")),
    ?assertEqual(true,add_vertex(DG, "Vertex2")),

    ?assertEqual(true,add_vertex(DG2, "Vertex0")),
    ?assertEqual(true,add_vertex(DG2, "Vertex1")),
    ?assertEqual(true,add_vertex(DG2, "Vertex2")),

    %% trying duplicate....
    ?assertEqual(true,add_vertex(DG,"Vertex2")),
    ?assertEqual(true,add_vertex(DG,"Vertex2")),
    ?assertEqual(true,add_vertex(DG,"Vertex2")),
    ?assertEqual(3,length(digraph:vertices(G))),
    %% %% Instance 2 trying duplicate....
    ?assertEqual(true,add_vertex(DG2,"Vertex2")),
    ?assertEqual(true,add_vertex(DG2,"Vertex2")),
    ?assertEqual(true,add_vertex(DG2,"Vertex2")),
    ?assertEqual(3,length(digraph:vertices(G2))),

    %% Add vertex with label...
    ?assertEqual(true,add_vertex(DG,"Vertex3",[{key,value}])),
    [{_,DG_Vertex3,_}] = ets:lookup(VTID,"Vertex3"),
    ?assertEqual([{"Vertex3",DG_Vertex3,[{key,value}]}],ets:lookup(VTID,"Vertex3")),
    %% Instance 2 Add vertex with label...
    ?assertEqual(true,add_vertex(DG2,"Vertex3",[{key,value}])),
    [{_,DG2_Vertex3,_}] = ets:lookup(VTID,"Vertex3"),
    ?assertEqual([{"Vertex3",DG2_Vertex3,[{key,value}]}],ets:lookup(VTID,"Vertex3")),

    %% Rename
    ?assertEqual(true,rename_vertex(DG,"Vertex0","NewVertex0name")),
    [{_,DG_Vertex0,_}] = ets:lookup(VTID,"NewVertex0name"),
    ?assertEqual([{"NewVertex0name",DG_Vertex0,[]}],ets:lookup(VTID,"NewVertex0name")),
    %% Instance 2 Rename
    ?assertEqual(true,rename_vertex(DG2,"Vertex0","NewVertex0name")),
    [{_,DG2_Vertex0,_}] = ets:lookup(VTID2,"NewVertex0name"),
    ?assertEqual([{"NewVertex0name",DG2_Vertex0,[]}],ets:lookup(VTID2,"NewVertex0name")),

    %% Add label to NON-label vertex
    ?assertEqual(true,add_vertex_label(DG,"Vertex1",key1,val1)),
    ?assertEqual(val1,get_vertex_label(DG,"Vertex1",key1)),

    %% Instance 2 Add label to NON-label vertex
    ?assertEqual(true,add_vertex_label(DG2,"Vertex1",key1,val1)),
    ?assertEqual(val1,get_vertex_label(DG2,"Vertex1",key1)),

    %% Add label to vertex with labels at create time.
    ?assertEqual(true,add_vertex_label(DG,"Vertex3",key1,val1)),
    ?assertEqual(value,get_vertex_label(DG,"Vertex3",key)),
    ?assertEqual(val1,get_vertex_label(DG,"Vertex3",key1)),
    ?assertEqual([{key,value},{key1,val1}],get_vertex_labels(DG,"Vertex3")),

    %% Instance 2 Add label to vertex with labels at create time.
    ?assertEqual(true,add_vertex_label(DG2,"Vertex3",key1,val1)),
    ?assertEqual(value,get_vertex_label(DG2,"Vertex3",key)),
    ?assertEqual(val1,get_vertex_label(DG2,"Vertex3",key1)),
    ?assertEqual([{key,value},{key1,val1}],get_vertex_labels(DG2,"Vertex3")),

    %% modify vertex label key with diff value ...
    ?assertEqual(true,add_vertex_label(DG,"Vertex3",key,changed)),
    ?assertEqual([{key,changed},{key1,val1}],lists:sort(get_vertex_labels(DG,"Vertex3"))),

    %% Instance 2 modify vertex label key with diff value ...
    ?assertEqual(true,add_vertex_label(DG2,"Vertex3",key,changed)),
    ?assertEqual([{key,changed},{key1,val1}],lists:sort(get_vertex_labels(DG2,"Vertex3"))),

    % remove_vertex_label
    ?assertEqual(true,remove_vertex_label(DG,"Vertex3",key)),
    ?assertEqual([{key1,val1}],lists:sort(get_vertex_labels(DG,"Vertex3"))),

    % Instance 2 remove_vertex_label
    ?assertEqual(true,remove_vertex_label(DG2,"Vertex3",key)),
    ?assertEqual([{key1,val1}],lists:sort(get_vertex_labels(DG2,"Vertex3"))),

    %% Create edge
    ?assertEqual(true,add_edge(DG,"Vertex1","Vertex2")),
    [{_,Ea,_}] = ets:lookup(ETID,{"Vertex1","Vertex2"}),
    ?assertEqual([{{"Vertex1","Vertex2"},Ea,[]}],ets:lookup(ETID,{"Vertex1","Vertex2"})),
    ?assertEqual(1,length(digraph:edges(G))),

    %% Instance 2 Create Edge
    ?assertEqual(true,add_edge(DG2,"Vertex1","Vertex2")),
    [{_,Ea2,_}] = ets:lookup(ETID2,{"Vertex1","Vertex2"}),
    ?assertEqual([{{"Vertex1","Vertex2"},Ea,[]}],ets:lookup(ETID2,{"Vertex1","Vertex2"})),
    ?assertEqual(1,length(digraph:edges(G2))),

    %% Create edge with Label
    ?assertEqual(true,add_edge(DG,"Vertex2","Vertex3",[{key,value}])),
    [{_,Eb,_}] = ets:lookup(ETID,{"Vertex2","Vertex3"}),
    ?assertEqual([{{"Vertex2","Vertex3"},Eb,[{key,value}]}],ets:lookup(ETID,{"Vertex2","Vertex3"})),
    ?assertEqual(2,length(digraph:edges(G))),

    %% Instance 2 Create edge with Label
    ?assertEqual(true,add_edge(DG2,"Vertex2","Vertex3",[{key,value}])),
    [{_,Eb2,_}] = ets:lookup(ETID2,{"Vertex2","Vertex3"}),
    ?assertEqual([{{"Vertex2","Vertex3"},Eb,[{key,value}]}],ets:lookup(ETID2,{"Vertex2","Vertex3"})),
    ?assertEqual(2,length(digraph:edges(G))),

    %% Add & Modify edge label
    ?assertEqual(true,add_vertex(DG,"Vertex888")),
    ?assertEqual(true,add_vertex(DG,"Vertex999")),

    ?assertEqual(true,add_edge(DG,"Vertex888","Vertex999",[{key8_9,value8_9}])),
    [{_,Ec,_}] = ets:lookup(ETID,{"Vertex888","Vertex999"}),
    ?assertEqual([{{"Vertex888","Vertex999"},Ec,[{key8_9,value8_9}]}],ets:lookup(ETID,{"Vertex888","Vertex999"})),
    ?assertEqual(3,length(digraph:edges(G))),

    ?assertEqual(true,add_edge_label(DG,"Vertex888","Vertex999",key8_9,some_new_value)),
    ?assertEqual([{{"Vertex888","Vertex999"},Ec,[{key8_9,some_new_value}]}],ets:lookup(ETID,{"Vertex888","Vertex999"})),

    ?assertEqual(some_new_value,get_edge_label(DG,"Vertex888","Vertex999",key8_9)),
    ?assertEqual([{key8_9,some_new_value}],get_edge_labels(DG,"Vertex888","Vertex999")),

    %% Removing Vertices:
    ?assertEqual(true,add_vertex(DG,"VertexTEST")),
    [{_,TESTVertex,_}] = ets:lookup(VTID,"VertexTEST"),
    {TESTVertex,VertexLabel} = digraph:vertex(G, TESTVertex),
    ?assertEqual(true,remove_vertex(DG,"VertexTEST")),
    false = digraph:vertex(G, TESTVertex),

    %% Removing Vertices with edges:
    ?assertEqual(true,add_vertex(DG,"VertexTEST1")),
    ?assertEqual(true,add_vertex(DG,"VertexTEST2")),
    ?assertEqual(true,add_vertex(DG,"VertexTEST3")),
    ?assertEqual(true,add_vertex(DG,"VertexTEST4")),

    [{_,TESTVertex1,_}] = ets:lookup(VTID,"VertexTEST1"),
    [{_,TESTVertex2,_}] = ets:lookup(VTID,"VertexTEST2"),
    [{_,TESTVertex3,_}] = ets:lookup(VTID,"VertexTEST3"),
    [{_,TESTVertex4,_}] = ets:lookup(VTID,"VertexTEST4"),

    {TESTVertex1,VertexLabel1} = digraph:vertex(G, TESTVertex1),
    {TESTVertex2,VertexLabel2} = digraph:vertex(G, TESTVertex2),
    {TESTVertex3,VertexLabel3} = digraph:vertex(G, TESTVertex3),
    {TESTVertex4,VertexLabel4} = digraph:vertex(G, TESTVertex4),

    ?assertEqual(true,add_edge(DG,"VertexTEST1","VertexTEST2")),
    ?assertEqual(true,add_edge(DG,"VertexTEST2","VertexTEST1")),
    ?assertEqual(true,add_edge(DG,"VertexTEST3","VertexTEST2")),
    %% No Edge for Test4
    ?assertEqual(6,length(digraph:edges(G))),

    ?assertEqual(true,remove_vertex(DG,"VertexTEST1")),
    ?assertEqual(true,remove_vertex(DG,"VertexTEST2")),
    ?assertEqual(3,length(digraph:edges(G))),

    false = digraph:vertex(G, TESTVertex1),
    false = digraph:vertex(G, TESTVertex2),
    {TESTVertex3,VertexLabel3} = digraph:vertex(G, TESTVertex3),
    {TESTVertex4,VertexLabel4} = digraph:vertex(G, TESTVertex4),

    [] = ets:lookup(ETID,{"VertexTEST1","VertexTEST2"}),
    [] = ets:lookup(ETID,{"VertexTEST2","VertexTEST1"}),
    [] = ets:lookup(ETID,{"VertexTEST3","VertexTEST2"}),

    %% CLEANUP
    [ ets:delete(TBL) || TBL <- ets:all(), ( ets:info(TBL,name) == ?TV  ) or (  ets:info(TBL,name) == ?TE ) ],
    ?assertEqual(true,digraph:delete(G)),
    ?assertEqual(true,digraph:delete(G2)).

    

