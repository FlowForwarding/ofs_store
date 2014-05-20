-module(dg_srv).

-export([start_link/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([add_vertex/1, add_vertex/2,
         vertices/1,
         vertex/2, 
         rename_vertex/3,
         set_vertex_alias/3, 
         rename_vertex_alias/4,
         del_vertex/2,
         add_edge/2,
        %% DEBUG
        del_graph/1,
         graphs/0
    ]).

-define(STATE,dg_srv_state). 
-record(?STATE,{tbl}).

%%-------------------------------------------------------------------------------------------------------------------------
%% Vertices:

%% @doc 
-spec add_vertex( {VertexName :: term()} | 
                  {VertexName :: term(), Alias :: term()} | 
                  {VertexName :: term(), Alias :: term(), Labels :: term()} ) -> {ok,GraphName :: term()}.
add_vertex(Properties) when is_tuple(Properties) ->
    {ok,GraphName} = gen_server:call(?MODULE,create_graph),
    gen_server:call(?MODULE,{add_vertex,GraphName,Properties}),
    {ok,GraphName}.

%% @doc 
-spec add_vertex( GraphName :: term() , {VertexName :: term()} | 
                                        {VertexName :: term(), Alias :: term()} | 
                                        {VertexName :: term(), Alias :: term(), Labels :: term()} ) -> boolean().
add_vertex(GraphName,Properties) when is_tuple(Properties) ->
    gen_server:call(?MODULE,{add_vertex,GraphName,Properties}).

%% @doc 
-spec vertices(GraphName :: term()) -> [digraph:vertex()].
vertices(GraphName) ->
    gen_server:call(?MODULE, {vertices,GraphName}). 

%% @doc 
-spec vertex(GraphName :: term(), VertexName :: term()) -> {ok,{VertexName :: term(),Alias :: term(),Labels :: term()}}.
vertex(GraphName,VertexName) ->
    gen_server:call(?MODULE, {vertex,GraphName,VertexName}).

%% @doc 
%% -spec 
rename_vertex(GraphName,VertexName,NewVertexName) -> 
    gen_server:call(?MODULE, {rename_vertex,GraphName,VertexName,NewVertexName}).

set_vertex_alias(GraphName,VertexName,NewAlias) ->
    gen_server:call(?MODULE, {rename_vertex_alias,GraphName,VertexName,NewAlias}).

rename_vertex_alias(GraphName,VertexName,Alias,NewAlias) ->
    gen_server:call(?MODULE, {rename_vertex_alias,GraphName,VertexName,Alias,NewAlias}).

del_vertex(GraphName,VertexName) ->
    gen_server:call(?MODULE,{del_vertex,GraphName,VertexName}).

del_graph(GraphName) ->
    gen_server:call(?MODULE,{del_graph,GraphName}).

graphs() ->
    gen_server:call(?MODULE,tables).

%%-------------------------------------------------------------------------------------------------------------------------
%% Edges

add_edge(GraphName,Properties) ->
    gen_server:call(?MODULE, {add_edge,GraphName,Properties}).

%%-------------------------------------------------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

init({}) ->
    TID=ets:new(graphs,[set,public]),
    {ok, #?STATE{tbl = TID}}.

%%---------------------------------------------------------

handle_call(create_graph,_From,#?STATE{ tbl = TID } = State) ->
    Sec=calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(now())),
    {reply,
        new_graph(TID,"graph_"++integer_to_list(Sec)),State};
handle_call({create_graph,Name},_From,#?STATE{ tbl = TID } = State) ->
    {reply,
        new_graph(TID,Name),State};
handle_call({add_vertex,GraphName,Properties}, _From, #?STATE{ tbl = TID } = State) ->
    F = fun(DG) ->
            case Properties of
                {VertexName}              -> dg:add_vertex(DG,VertexName);
                {VertexName,Alias}        -> dg:add_vertex(DG,VertexName,Alias);
                {VertexName,Alias,Labels} -> dg:add_vertex(DG,VertexName,Alias,Labels);
                _                         -> error
            end
        end,
    {reply,
        graph_operation(TID,GraphName,F),State};
handle_call({vertices,GraphName},_From,#?STATE{ tbl = TID } = State) ->
    {reply,
        graph_operation(TID,GraphName,fun(DG) -> dg:vertices(DG) end),State};
handle_call({vertex,GraphName,VertexName},_From,#?STATE{ tbl = TID } = State) ->
    {reply,
        graph_operation(TID,GraphName,fun(DG) -> dg:vertex(DG,VertexName) end),State};
handle_call({rename_vertex,GraphName,VertexName,NewVertexName},_From,#?STATE{ tbl = TID } = State) ->
    {reply,
        graph_operation(TID,GraphName,fun(DG) -> dg:rename_vertex(DG,VertexName,NewVertexName) end),State};
handle_call({rename_vertex_alias,GraphName,VertexName,NewAlias},_From,#?STATE{ tbl = TID } = State) ->
    {reply,
        graph_operation(TID,GraphName,fun(DG) -> dg:rename_vertex_alias(DG,VertexName,NewAlias) end),State};
handle_call({rename_vertex_alias,GraphName,VertexName,Alias,NewAlias},_From,#?STATE{ tbl = TID } = State) ->
    {reply,
        graph_operation(TID,GraphName,fun(DG) -> dg:rename_vertex_alias(DG,VertexName,Alias,NewAlias) end),State};
handle_call({del_vertex,GraphName,VertexName},_From,#?STATE{ tbl = TID } = State) ->
    {reply,
        graph_operation(TID,GraphName,fun(DG) -> dg:del_vertex(DG,VertexName) end),State};
handle_call(tables,_From,#?STATE{ tbl = TID } = State) ->
    {reply,{ok,ets:tab2list(TID)},State};
handle_call({del_graph,GraphName},_From,#?STATE{ tbl = TID } = State) ->
    F = fun(DG) -> 
        dg:delete(DG),
        ets:delete(TID,GraphName)
    end,
    {reply,
        graph_operation(TID,GraphName,F),State};
handle_call({add_edge,GraphName,Properties},_From,#?STATE{ tbl = TID } = State) ->
    F = fun(DG) -> 
            case Properties of
                {V1,V2}        -> dg:add_edge(DG,V1,V2);
                {V1,V2,Labels} -> dg:add_edge(DG,V1,V2,Labels);
                _              -> error
            end
     end,
    {reply,
        graph_operation(TID,GraphName,F),State}.
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
    {ok,DG} = dg:new(),
    Obj = {Name,DG},
    true = ets:insert_new(TID,Obj),
    {ok,Name}.

graph_operation(TID,GraphName,F) ->
    case ets:lookup(TID,GraphName) of
        [{GraphName,DG}] -> 
            F(DG);
        [] ->
            {ok,Name} = new_graph(TID,GraphName),
            graph_operation(TID,GraphName,F)
    end.
  


