-module(dg_srv).

-export([start_link/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(STATE,dg_srv_state). 
-record(?STATE,{tbl}).

%%-------------------------------------------------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

init({}) -> 
    TID=ets:new(graphs,[set,public]),
    {ok, #?STATE{tbl = TID}}.

handle_call(create_graph,_From,#?STATE{ tbl = TID } = State) ->
    Sec=calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(now())),
    {reply,
        new_graph(TID,"graph_"++integer_to_list(Sec)),State};
handle_call({create_graph,Name},_From,#?STATE{ tbl = TID } = State) ->
    {reply,
        new_graph(TID,Name),State};
handle_call({create_graph,Name,Type},_From,#?STATE{ tbl = TID } = State) ->
    {reply,
        new_graph(TID,Name,Type),State};
handle_call({add_vertex,GraphName,Properties}, _From, #?STATE{ tbl = TID } = State) ->
    F = fun(DG) ->
            case Properties of
                {VertexName}              -> dg_db:add_vertex(DG,VertexName);
                {VertexName,Labels}       -> dg_db:add_vertex(DG,VertexName,Labels);
                {VertexName,Labels,Alias} -> dg_db:add_vertex(DG,VertexName,Labels,Alias);
                _                         -> error
            end
        end,
    {reply,
        graph_operation(TID,GraphName,F),State};
handle_call({vertices,GraphName},_From,#?STATE{ tbl = TID } = State) ->
    {reply,
        graph_operation(TID,GraphName,fun(DG) -> dg_db:vertices(DG) end),State};
handle_call({vertex,GraphName,VertexName},_From,#?STATE{ tbl = TID } = State) ->
    {reply,
        graph_operation(TID,GraphName,fun(DG) -> dg_db:vertex(DG,VertexName) end),State};
% handle_call({vertex_alias,GraphName,VertexAlias},_From,#?STATE{ tbl = TID } = State) ->
%     {reply,
%         graph_operation(TID,GraphName,fun(DG) -> dg_db:vertex_alias(DG,VertexAlias) end),State};
handle_call({rename_vertex,GraphName,VertexName,NewVertexName},_From,#?STATE{ tbl = TID } = State) ->
    {reply,
        graph_operation(TID,GraphName,fun(DG) -> dg_db:rename_vertex(DG,VertexName,NewVertexName) end),State};
handle_call({rename_vertex_alias,GraphName,VertexName,NewAlias},_From,#?STATE{ tbl = TID } = State) ->
    {reply,
        graph_operation(TID,GraphName,fun(DG) -> dg_db:rename_vertex_alias(DG,VertexName,NewAlias) end),State};
handle_call({rename_vertex_alias,GraphName,VertexName,Alias,NewAlias},_From,#?STATE{ tbl = TID } = State) ->
    {reply,
        graph_operation(TID,GraphName,fun(DG) -> dg_db:rename_vertex_alias(DG,VertexName,Alias,NewAlias) end),State};
handle_call({del_vertex,GraphName,VertexName},_From,#?STATE{ tbl = TID } = State) ->
    {reply,
        graph_operation(TID,GraphName,fun(DG) -> dg_db:del_vertex(DG,VertexName) end),State};
handle_call({del_vertices,GraphName,Vertices},_From,#?STATE{ tbl = TID } = State) ->
    {reply,
        graph_operation(TID,GraphName,fun(DG) -> dg_db:del_vertices(DG,Vertices) end),State};
handle_call({add_vertex_labels,GraphName,VertexName,VertexLabels},_From,#?STATE{ tbl = TID } = State) ->
    {reply,
        graph_operation(TID,GraphName,fun(DG) -> dg_db:add_vertex_label(DG,VertexName,VertexLabels) end),State};
handle_call({no_vertices,GraphName},_From,#?STATE{ tbl = TID } = State) ->
    {reply,
        graph_operation(TID,GraphName,fun(DG) -> dg_db:no_vertices(DG) end),State};
handle_call(tables,_From,#?STATE{ tbl = TID } = State) ->
    {reply,{ok,ets:tab2list(TID)},State};
handle_call({del_graph,GraphName},_From,#?STATE{ tbl = TID } = State) ->
    F = fun(DG) -> 
        dg_db:delete(DG),
        ets:delete(TID,GraphName)
    end,
    {reply,
        graph_operation(TID,GraphName,F),State};
handle_call({rename_graph,OldName,NewName},_From,#?STATE{ tbl = TID } = State) ->
    {reply,rename_graph(TID,OldName,NewName),State};
handle_call({add_edge,GraphName,Properties},_From,#?STATE{ tbl = TID } = State) ->
    F = fun(DG) -> 
            case Properties of
                {V1,V2}        -> dg_db:add_edge(DG,V1,V2);
                {V1,V2,Labels} -> dg_db:add_edge(DG,V1,V2,Labels);
                _              -> error
            end
     end,
    {reply,
        graph_operation(TID,GraphName,F),State};
handle_call({alter_edge,GraphName,Properties},_From,#?STATE{ tbl = TID } = State) ->
    F = fun(DG) -> 
        case Properties of
            {E,V1,V2} ->
                {E, V1, V2, Labels} = dg_db:edge(DG,E),
                dg_db:alter_edge(DG,E,V1,V2,Labels);
            {E,V1,V2,Labels} ->
                dg_db:alter_edge(DG,E,V1,V2,Labels);
            _ ->
                error
        end
    end,
    {reply,
        graph_operation(TID,GraphName,F),State};
handle_call({edges,GraphName},_From,#?STATE{ tbl = TID } = State) ->
    {reply,
        graph_operation(TID,GraphName,fun(DG) -> dg_db:edges(DG) end),State}; 
handle_call({edges,GraphName,VertexName},_From,#?STATE{ tbl = TID } = State) ->
    {reply,
        graph_operation(TID,GraphName,fun(DG) -> dg_db:edges(DG,VertexName) end),State}; 
handle_call({edge,GraphName,Edge},_From,#?STATE{ tbl = TID } = State) ->
    {reply,
        graph_operation(TID,GraphName,fun(DG) -> dg_db:edge(DG,Edge) end),State};
handle_call({no_edges,GraphName},_From,#?STATE{ tbl = TID } = State) ->
    {reply,
        graph_operation(TID,GraphName,fun(DG) -> dg_db:no_edges(DG) end),State};
handle_call({del_edge,GraphName,Edge},_From,#?STATE{ tbl = TID } = State) ->
    {reply,
        graph_operation(TID,GraphName,fun(DG) -> dg_db:del_edge(DG,Edge) end),State};
handle_call({del_edges,GraphName,Edges},_From,#?STATE{ tbl = TID } = State) ->
    {reply,
        graph_operation(TID,GraphName,fun(DG) -> dg_db:del_edges(DG,Edges) end),State}.

%%---------------------------------------------------------

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) -> 
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-------------------------------------------------------------------------------------------------------------------------

new_graph(TID,Name) ->
    new_graph(TID,Name,[]).

new_graph(TID,Name,Type) ->
    {ok,DG} = dg_db:new(),
    Obj = {Name,DG},
    case ets:insert_new(TID,Obj) of
        true  -> {ok,Name};
        false -> {ok,Name}
    end.

graph_operation(TID,GraphName,F) ->
    case ets:lookup(TID,GraphName) of
        [{GraphName,DG}] -> 
            F(DG);
        [] ->
            {ok,Name} = new_graph(TID,GraphName),
            graph_operation(TID,GraphName,F)
    end.
    
rename_graph(TID,OldName,NewName) ->
    case ets:lookup(TID,OldName) of
        [{OldName,DG}] -> 
            case ets:insert_new(TID,[{NewName,DG}]) of
                true  -> ets:delete(TID,OldName);
                false -> false
            end;
        [] ->
            false
    end.
