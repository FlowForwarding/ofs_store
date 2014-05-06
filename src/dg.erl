-module (dg).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-define(T,dgtbl).
-record(dg,{digraph}).

% @doc Creates the initial Dictionary and Directed graph
-spec init() -> {ok,{D :: dict(), G :: digraph()}}.
init() ->
    ?T=ets:new(?T, [set,public,named_table]),
    ok.

new() ->
    G = digraph:new(),
    #dg{ digraph = G }.

% @doc add_vertex(G, Name) - add a vertex to the augmented digraph G called Name.
% Note: this function won't rename the Key stored in dict.
%% -spec add_vertex(D :: dict(), G :: digraph(), Name :: term()) -> {ok,D2 :: dict()}.
add_vertex(G, VertexName) ->
    ets_noexist(?T,VertexName,fun() ->
        ['$v'|_] = Vertex = digraph:add_vertex(G),
        true=ets:insert(?T,{VertexName,Vertex,[]})
    end).

%% @doc rename_vertex(OldName, NewName) - rename a vertex
-spec rename_vertex(OldVertexName :: term(), NewVertexName :: term()) -> error | {ok,D2 :: dict()}.
rename_vertex(OldVertexName,NewVertexName) ->
    ets_exist(?T,OldVertexName,fun({OVN,Vertex,Labels}) -> 
        true = ets:insert(?T,{NewVertexName,Vertex,Labels}),
        true = ets:delete(?T, OVN)
    end).

% @doc add_edge(G, Name1, Name2) - create an edge between vertex Name1 and Name2
% -spec add_edge(D :: dict(), G :: digraph(), VertexName1 :: term(), VertexName2 :: term()) -> ok | error | {error, any()}.
add_edge(G,VertexName1,VertexName2) ->
    ets_exist(?T,VertexName1,fun({_,Vertex1,_}) ->
        ets_exist(?T,VertexName2,fun({_,Vertex2,_}) ->
            ['$e'|_] = Edge = digraph:add_edge(G,Vertex1,Vertex2),
            true
        end)
    end).

% add_label(G, Name, Key, Value) - add Key, Value to the vertex
add_label(G,VertexName,Key,Value) ->
    ets_exist(?T,VertexName,fun({Name,Vertex,Labels}) ->
        true=ets:update_element(?T,VertexName,{3,lists:append(Labels,[{Key,Value}])})
    end).

% remove_label(G, Name, Key)
remove_label(G, VertexName, Key) ->
    ets_exist(?T,VertexName,fun({Name,Vertex,Labels}) -> ets:insert(?T,{Name,Vertex,proplists:delete(Key, Labels)}) end).

% get_label(G, Name, Key)
get_label(G, VertexName, Key) ->
    ets_exist(?T,VertexName,fun({Name,_,Labels}) -> proplists:get_value(Key, Labels, []) end).


% get_labels(G, Name)
get_labels(G, VertexName) -> 
    ets_exist(?T,VertexName,fun({Name,_,Labels}) -> Labels end).


%% ---- internal -------------------

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
    try 
        ets:delete(?T)
    catch
        C:E ->
            error
    end,

    {ok,G} = init(),

    true = add_vertex(G, VertexName1="Vertex1"),
    true = add_vertex(G, VertexName2="Vertex2"),
    true = add_vertex(G, VertexName3="Vertex3"),

    true = rename_vertex(VertexName2,"NewVertexName2"),
    add_label(G,"NewVertexName2","Key","Value"),
    "Value" = get_label(G,"NewVertexName2","Key"),

    add_label(G,VertexName1,"Key","Value"),
    add_label(G,VertexName1,"Key2","Value2"),

    "Value" = get_label(G,VertexName1,"Key"),
    "Value2" = get_label(G,VertexName1,"Key2"),
    error = get_label(G,"FakeVertexName","Key"),
    [{"Key","Value"},{"Key2","Value2"}] = get_labels(G,VertexName1),

    true = remove_label(G, VertexName1, "Key2"),
    [{"Key","Value"}] = get_labels(G,VertexName1),

    true = remove_label(G, VertexName1, "Key"),
    [] = get_labels(G,VertexName1),

    true = remove_label(G, VertexName1, "Key"),
    [] = get_labels(G,VertexName1),

    %% Edges:
    true = add_edge(G,VertexName1,VertexName3),
    error = add_edge(G,"FakeVertexName1","FakeVertexName2"),
    [['$e'|0]] = digraph:edges(G).

r() ->
    
    %% Numbering on Edges and Vertices are 0 based. IE: [0,1,2,3,4] Total 5

    %% Example directed graph used below:
    %% http://www2.uwstout.edu/content/faculty/wuming/samples/gifFile/AdjM2.gif

    %% G = digraph:new(),

    %% '$v'
    % V0 = digraph:add_vertex(G),
    % V1 = digraph:add_vertex(G),
    % V2 = digraph:add_vertex(G),
    % V3 = digraph:add_vertex(G),
    % V4 = digraph:add_vertex(G),

    %% '$e'
    % Edge0 = digraph:add_edge(G,V0,V1,"label_0_1"),
    % Edge1 = digraph:add_edge(G,V0,V3,"label_0_3"),
    % Edge2 = digraph:add_edge(G,V1,V2,"label_1_2"),
    % Edge3 = digraph:add_edge(G,V1,V4,"label_1_4"),
    % Edge4 = digraph:add_edge(G,V2,V4,"label_2_4"),
    % Edge5 = digraph:add_edge(G,V3,V4,"label_3_4"),

    % digraph

    % add_edge/5                    get_short_path/3
    % add_edge/4                    in_degree/2
    % add_edge/3                    in_edges/2
    % add_vertex/3                  in_neighbours/2
    % add_vertex/1                  info/1
    % add_vertex/2                  module_info/0
    % del_edge/2                    module_info/1
    % del_edges/2                   new/1
    % del_path/3                    new/0
    % del_vertex/2                  no_edges/1
    % del_vertices/2                no_vertices/1
    % delete/1                      out_degree/2
    % edge/2                        out_edges/2
    % edges/1                       out_neighbours/2
    % edges/2                       sink_vertices/1
    % get_cycle/2                   source_vertices/1
    % get_path/3                    vertex/2
    % get_short_cycle/2             vertices/1

    %% digraph:info(G),
    %% digraph:no_edges(G).
    %% digraph:no_vertices(G).
    %% digraph:out_degree(G,V1).
    %% digraph:out_edges(G,V1).
    %% digraph:out_neighbours(G, V1).
    %% digraph:vertices(G).
    %% digraph:edges(G).

    % digraph_utils

    % arborescence_root/1           postorder/1
    % components/1                  preorder/1
    % condensation/1                reachable/2
    % cyclic_strong_components/1    reachable_neighbours/2
    % is_acyclic/1                  reaching/2
    % is_arborescence/1             reaching_neighbours/2
    % is_tree/1                     strong_components/1
    % loop_vertices/1               subgraph/2
    % module_info/0                 subgraph/3
    % module_info/1                 topsort/1

    %% digraph_utils:arborescence_root(G).
    %% digraph_utils:components(G).
    %% digraph_utils:loop_vertices(G).
    %% digraph_utils:postorder(G).
    %% digraph_utils:reachable([V0], G).
    %% digraph_utils:reaching([V0],G).

%%    digraph:edge(G, Edge0).
ok.







