-module (dg_db).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-record(dg,{digraph,
            tbl
           }).

%% vertex { Name } <-> dg2_alias{ alias , Name }

new() ->
    new([]).

new(Type) ->
    G = digraph:new(Type),
    TID = ets:new(dg2_alias,[set,public]),
    {ok,#dg{ digraph = G, tbl = TID }}.

delete(#dg{ tbl = TID, digraph = G } = _DG) ->
    true=ets:delete(TID),
    digraph:delete(G).

%% ----------------------------------------------------------------------------------------

add_vertex(DG, Name) ->
    add_vertex(DG, Name, undefined).

add_vertex(DG, Name, Alias) ->
    add_vertex(DG, Name, Alias, []).

add_vertex(#dg{ digraph = G, tbl = TID } = _DG, Name, Alias, Labels) ->
    case digraph:add_vertex(G, Name, Labels) of
        Name  -> ets:insert_new(TID, {Name,Alias});
        false -> false
    end.

vertex(#dg{ tbl = TID } = DG,Name) ->
    case vertex_name(DG,Name) of
        {ok,{Name,Labels}} ->
            case ets:lookup(TID,Name) of
                [{Name,Alias}] -> {ok,{Name,Alias,Labels}};
                _              -> false
            end;
        false ->
            false
    end.

vertex_name(#dg{ digraph = G } = _DG, Name) ->
    case digraph:vertex(G, Name) of
        {Name,Labels} -> {ok,{Name,Labels}};
        false         -> false
    end.

%% Note: Currently not retrieving Aliases...
vertices(#dg{ digraph = G } = _DG) ->
    digraph:vertices(G).

rename_vertex(#dg{ digraph = G, tbl = TID } = DG, Name, NewName) ->
    case vertex_name(DG,Name) of
        {ok,{Name, Labels}} ->
            case digraph:add_vertex(G, NewName, Labels) of %% add_vertex/3 updates ...
                NewName ->
                    true=digraph:del_vertex(G, Name),
                    [{Name,Alias}] = ets:lookup(TID,Name),
                    true=ets:delete(TID, Name),
                    ets:insert_new(TID, [{NewName,Alias}]);
                false -> 
                    false 
            end;
        false ->
            false
    end.

rename_vertex_alias(#dg{ tbl = TID } = _DG, Name, NewAlias) ->
    case ets:lookup(TID, Name) of
        [{Name,_}] ->
            true=ets:delete(TID, Name),
            ets:insert_new(TID, [{Name,NewAlias}]);
        _ ->
            false
    end.

rename_vertex_alias(#dg{ tbl = TID } = _DG, Name, Alias, NewAlias) ->
    case ets:lookup(TID, Name) of
        [{Name,Alias}] ->
            true=ets:delete(TID, Name),
            ets:insert_new(TID, [{Name,NewAlias}]);
        _ ->
            false
    end.

add_vertex_label(#dg{ digraph = G } = DG, Name, AddLabels) ->
    case vertex(DG, Name) of
        {ok,{Name, Labels}} -> digraph:add_vertex(G, Name, lists:append(AddLabels,Labels)); %% add_vertex/3 updates ...            
        false               -> false
    end.

edit_vertex_labels(#dg{ digraph = G } = DG, Name, NewLabels) ->
    case vertex(DG, Name) of
        {ok,{Name,_}} -> digraph:add_vertex(G, Name, NewLabels); %% add_vertex/3 updates ...
        false         -> false
    end.

del_vertex(#dg{ digraph = G, tbl = TID } = _DG,Name) ->
    case digraph:del_vertex(G, Name) of
        true  -> ets:delete(TID, Name);
        false -> false
    end.

%% ----------------------------------------------------------------------------------------

add_edge(DG, VertexName1, VertexName2) ->
    add_edge(DG,VertexName1,VertexName2,undefined,[]).

add_edge(DG, VertexName1, VertexName2, Alias) ->
    add_edge(DG,VertexName1,VertexName2,Alias,[]).

add_edge(#dg{ digraph = G, tbl = TID } = _DG, VertexName1, VertexName2, Alias, Labels) ->
    case digraph:add_edge(G, VertexName1, VertexName2, Labels) of
        {error,Reason} -> {error,Reason};
        Edge           -> {ok,Edge}
    end.

alter_edge(#dg{ digraph = _G, tbl = _TID } = _DG, _Edge, _VertexName1, _VertexName2, _Label) ->
    ok.

edges(#dg{digraph = G} = _DG, VertexName) ->
    digraph:edges(G, VertexName).

rename_edge() ->
    ok.

edit_edge_labels() ->
   ok.

del_edge() ->
    ok.

%% ----------------------------------------------------------------------------------------
 
test() ->
    ok.
%     {ok,DG} = new(),
%     TID = DG#dg.tbl,

%     ?assertEqual(true,
%         add_vertex(DG,"Machine1","ALIAS_MACHINE1",[{0,"127.0.0.1"}])),
%     ?assertEqual([{"Machine1","ALIAS_MACHINE1"}],ets:lookup(TID,"Machine1")),

%     ?assertEqual(true,
%         add_vertex(DG,"Machine2","ALIAS_NMR_2",[])),
%     ?assertEqual([{"Machine2","ALIAS_NMR_2"}],ets:lookup(TID,"Machine2")),

%     ?assertEqual(true,
%         add_vertex(DG,name)),
%     ?assertEqual([{"Machine2",undefined}],ets:lookup(TID,name)),
%     % Duplicate
%     ?assertEqual(false,
%         add_vertex(DG,name)),

%     ?assertEqual({ok,{"Machine1","ALIAS_MACHINE1",[{0,"127.0.0.1"}]}},
%         vertex(DG,"Machine1")).

% destroy_loop(DG,Count,Max) when Count >= Max ->
%     ok;
% destroy_loop(DG,Count,Max) ->
%     del_vertex(DG,"vertex"++integer_to_list(Count)),
%     destroy_loop(DG,Count+1,Max).

% loop(DG,Count) when Count =< 0 ->
%     ok;
% loop(DG,Count) ->
%     add_vertex(DG,"vertex"++integer_to_list(Count)),
%     loop(DG,Count-1).

