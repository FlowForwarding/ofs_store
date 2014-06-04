-module (dg_db).

-export([new/0, new/1,
         add_vertex/2, add_vertex/4,
         vertex/2,
         vertex_name/2,
         vertex_alias/2,
         vertices/1,
         rename_vertex/3,
         rename_vertex_alias/3,
         add_vertex_label/3,
         replace_vertex_labels/3,
         del_vertex/2,
         del_vertices/2,
         no_vertices/1,
         add_edge/3,
         add_edge/4,
         alter_edge/5,
         edge/2,
         edges/1,
         edges/2,
         edit_edge_labels/0,
         del_edge/2,
         del_edges/2,
         no_edges/1,
         delete/1
        ]).

-include_lib("eunit/include/eunit.hrl").

-record(dg,{digraph,
            tbl
           }).

%% vertex { Name } <-> dg2_alias{ alias , name }
  
new() ->
    new([]).
  
new(Type) ->
    G = digraph:new(Type),
    TID = ets:new(dg2_alias,[set,public]),
    {ok,#dg{ digraph = G, tbl = TID }}.

delete(#dg{ tbl = TID, digraph = G } = _DG) ->
    true=ets:delete(TID),
    digraph:delete(G).

%% Alias =:= Name ( If no alias is specified, to prevent duplicate alias's with value undefined )
add_vertex(DG, Name) ->
    add_vertex(DG, Name, [], Name).

add_vertex(#dg{ digraph = G, tbl = TID } = _DG, Name, Labels2, Alias) ->
    Labels = proplists:delete(alias,Labels2),
    case digraph:add_vertex(G, Name, lists:append([{alias,Alias}],Labels)) of
        Name  -> ets:insert_new(TID, {Alias,Name});
        false -> false
    end.

vertex(DG,Name) ->
    case vertex_name(DG,Name) of 
        {ok,{Name,Labels}} ->
            Alias = proplists:get_value(alias,Labels),
            {ok,{Name,Labels,Alias}};
        false ->
            false 
    end.

vertex_name(#dg{ digraph = G } = _DG, Name) ->
    case digraph:vertex(G,Name) of
        {Name,Labels} -> {ok,{Name,Labels}};
        false         -> false
    end.

vertex_alias(#dg{ tbl = TID } = DG, Alias) ->
    case ets:lookup(TID,Alias) of
        [{Alias,Name}] ->
            case vertex_name(DG,Name) of
                {ok,{Name,Labels}} -> {ok,{Name,Labels,Alias}};
                false              -> false
            end;
        _ ->
            false
    end.

%% Note: Currently not retrieving Aliases...
vertices(#dg{ digraph = G } = _DG) ->
    digraph:vertices(G).

% TODO: deleting the alias here, is a bit nasty ....
rename_vertex(#dg{ digraph = G, tbl = TID } = DG, Name, NewName) when Name =/= NewName ->
    case vertex_name(DG,Name) of
        {ok,{Name, Labels}} ->
            Alias = proplists:get_value(alias,Labels),
            NewAlias = 
            case Alias of 
                Name  -> NewName;
                Alias -> Alias
            end,
            Labels2 = [ {alias,NewAlias} | proplists:delete(alias, Labels) ],
            case digraph:add_vertex(G, NewName, Labels2) of %% add_vertex/3 updates ...
                NewName ->
                    true=digraph:del_vertex(G, Name),
                    ets:insert(TID, [{Alias,NewName}]);
                false -> 
                    false 
            end;
        false ->
            false
    end.

rename_vertex_alias(#dg{ digraph = G, tbl = TID } = DG, Name, NewAlias) ->
    case vertex_name(DG,Name) of
        {ok,{Name,Labels}} ->
            Alias = proplists:get_value(alias,Labels),
            case ets:lookup(TID, Alias) of
                [{Alias,_}] ->
                    Labels2 = [ {alias,NewAlias} | proplists:delete(alias, Labels) ],
                    Name = digraph:add_vertex(G, Name, Labels2), %% Update alias on vertex
                    true=ets:delete(TID, Alias),
                    ets:insert_new(TID, [{NewAlias,Name}]);
                _ ->
                    false
            end;
        false ->
            false
    end.

rename_vertex_alias(#dg{ digraph = G, tbl = TID } = DG, Name, Alias, NewAlias) when Alias =/= NewAlias ->
    case ets:lookup(TID, Alias) of
        [{Alias,Name}] ->
            case vertex_name(DG,Name) of
                {ok,{Name,Labels}} ->
                    Labels2 = [ {alias,NewAlias} | proplists:delete(alias, Labels) ],
                    Name = digraph:add_vertex(G, Name, Labels2), %% Update alias on vertex
                    true=ets:delete(TID, Alias),
                    ets:insert_new(TID, [{NewAlias,Name}]);
                false ->
                    false 
            end;
        _ ->
            false
    end.

add_vertex_label(#dg{ digraph = G } = DG, Name, AddLabels2) ->
    AddLabels = proplists:delete(alias,AddLabels2),
    case vertex_name(DG, Name) of
        {ok,{Name, Labels}} ->
            Name = digraph:add_vertex(G, Name, lists:append(AddLabels,Labels)), %% add_vertex/3 updates ... 
            true;
        false ->
            false
    end.

replace_vertex_labels(#dg{ digraph = G } = DG, Name, NewLabels2) ->
    NewLabels = proplists:delete(alias,NewLabels2),
    case vertex_name(DG, Name) of 
        {ok,{Name,Labels}} ->
            Alias = proplists:get_value(alias,Labels),
            Name=digraph:add_vertex(G, Name, [{alias,Alias}|NewLabels]), %% add_vertex/3 updates ...
            true;
        false ->
            false
    end.

del_vertex(#dg{ digraph = G, tbl = TID } = DG,Name) ->
    case vertex_name(DG,Name) of
        {ok,{Name,Labels}} ->
            Alias = proplists:get_value(alias,Labels),
            case digraph:del_vertex(G, Name) of
                true  -> ets:delete(TID, Alias);
                false -> false
            end;
        false ->
            fasle 
    end.

del_vertices(#dg{ digraph = G, tbl = TID } = DG,Vertices) ->
    F = fun(V) ->
        {ok,{_,_,Alias}} = vertex(DG,V), 
        ets:delete(TID,Alias)
    end,
    ok = lists:foreach(F,Vertices),
    true = digraph:del_vertices(G, Vertices).
    

no_vertices(#dg{digraph = G} = _DG) ->
    digraph:no_vertices(G).

%% ----------------------------------------------------------------------------------------
 
add_edge(DG, VertexName1, VertexName2) ->
    add_edge(DG,VertexName1,VertexName2,[]).

%% TODO: maybe get the edge if exists...
add_edge(#dg{ digraph = G } = _DG, VertexName1, VertexName2, Labels) ->
    case digraph:add_edge(G, VertexName1, VertexName2, Labels) of
        {error,Reason} -> {error,Reason};
        Edge           -> {ok,Edge}
    end.

alter_edge(#dg{ digraph = G, tbl = _TID } = _DG, Edge, VertexName1, VertexName2, Labels) ->
    case digraph:add_edge(G, Edge, VertexName1, VertexName2, Labels) of
        {error,Reason} -> {error,Reason};
        Edge           -> {ok,Edge}
    end.        

edge(#dg{digraph = G} = _DG, Edge) ->
    digraph:edge(G,Edge).

edges(#dg{digraph = G} = _DG) ->
    digraph:edges(G).
    
edges(#dg{digraph = G} = _DG, VertexName) ->
    digraph:edges(G, VertexName).

% rename_edge() ->
%     ok.

edit_edge_labels() ->
   ok.

del_edge(#dg{digraph = G} = _DG, Edge) ->
    digraph:del_edge(G, Edge).

del_edges(#dg{digraph = G} = _DG, Edges) ->
    digraph:del_edges(G, Edges).

no_edges(#dg{digraph = G} = _DG) -> 
    digraph:no_edges(G).

%% ----------------------------------------------------------------------------------------
 
dg_db_test() ->
    {ok,DG} = dg_db:new(),
    %%#dg{tbl = TID} = DG,

    %% Vertices:

    ?assertEqual(true,dg_db:add_vertex(DG, test1)),
    ?assertEqual(true,dg_db:add_vertex(DG, test2)),

    ?assertEqual({ok,{test1,[{alias,test1}],test1}},dg_db:vertex(DG, test1)),
    ?assertEqual({ok,{test2,[{alias,test2}],test2}},dg_db:vertex(DG, test2)),

    ?assertEqual(true,dg_db:add_vertex(DG, test3, [{key1,value1},{alias,alias_1}], alias_1 )),
    ?assertEqual(true,dg_db:add_vertex(DG, test4, [{key2,value2},{alias,alias_2}], alias_2 )),

    ?assertEqual({ok,{test3,[{alias,alias_1},{key1,value1}],alias_1}}, dg_db:vertex(DG, test3)),
    ?assertEqual({ok,{test4,[{alias,alias_2},{key2,value2}],alias_2}}, dg_db:vertex(DG, test4)),

    Vertices = dg_db:vertices(DG),
    ?assertEqual(true,lists:member(test1,Vertices)),
    ?assertEqual(true,lists:member(test2,Vertices)),
    ?assertEqual(true,lists:member(test3,Vertices)),
    ?assertEqual(true,lists:member(test4,Vertices)),

    true = dg_db:rename_vertex(DG,test1,test_rename),
    ?assertEqual({ok,{test_rename,[{alias,test_rename}],test_rename}},dg_db:vertex(DG,test_rename)),

    %% Simply overwrite
    ?assertEqual(true,rename_vertex_alias(DG,test3,alias_overwrited)),
    ?assertEqual({ok,{test3,[{alias,alias_overwrited},{key1,value1}],alias_overwrited}}, dg_db:vertex(DG, test3)),

    %% only rename if exists
    ?assertEqual(false,rename_vertex_alias(DG,test3,alias_1,alias_no_overwrite)),
    ?assertEqual({ok,{test3,[{alias,alias_overwrited},{key1,value1}],alias_overwrited}}, dg_db:vertex(DG, test3)),

    ?assertEqual(true,rename_vertex_alias(DG,test3,alias_overwrited,alias_new)),
    ?assertEqual({ok,{test3,[{alias,alias_new},{key1,value1}],alias_new}}, dg_db:vertex(DG, test3)),

    %% Add labels to existing proplist
    ?assertEqual(true,dg_db:add_vertex_label(DG,test3,[{key3,value3}])),
    ?assertEqual({ok,{test3,[{key3,value3},{alias,alias_new},{key1,value1}],alias_new}}, dg_db:vertex(DG, test3)), 

    %% edit exiting key's in proplist
    ?assertEqual(true,dg_db:replace_vertex_labels(DG,test3,[{key300,value300}])),
    ?assertEqual({ok,{test3,[{alias,alias_new},{key300,value300}],alias_new}}, dg_db:vertex(DG, test3)),

    ?assertEqual(true,dg_db:del_vertex(DG,test_rename)),
    ?assertEqual(false,dg_db:vertex(DG, test_rename)), 

    ?assertEqual({ok,{test2,[{alias,test2}],test2}},dg_db:vertex(DG, test2)), 

    ?assertEqual(true,dg_db:del_vertices(DG,[test2,test3])),
    ?assertEqual(false,dg_db:vertex(DG, test2)),
    ?assertEqual(false,dg_db:vertex(DG, test3)),

    ?assertEqual(1,dg_db:no_vertices(DG)).

    %% Edges:

    % add_edge(DG, VertexName1, VertexName2) ->
    % add_edge(#dg{ digraph = G, tbl = TID } = _DG, VertexName1, VertexName2, Labels) ->
    % alter_edge(#dg{ digraph = G, tbl = _TID } = _DG, Edge, VertexName1, VertexName2, Labels) ->
    % edge(#dg{digraph = G} = _DG, Edge) ->
    % edges(#dg{digraph = G} = _DG) ->
    % edges(#dg{digraph = G} = _DG, VertexName) ->
    % % rename_edge() ->
    % edit_edge_labels() ->
    % del_edge(#dg{digraph = G} = _DG, Edge) ->
    % del_edges(#dg{digraph = G} = _DG, Edges) ->
    % no_edges(#dg{digraph = G} = _DG) ->    