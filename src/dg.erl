-module (dg).

-export([ new/0,
          new/1,
          new/2 ,
          del_graph/1,
          graphs/0,
          rename_graph/2,
          add_vertex/1,
          add_vertex/2,
          vertices/1,
          vertex/2,
          rename_vertex/3,
          set_vertex_alias/3,
          rename_vertex_alias/4,
          del_vertex/2,
          del_vertices/2,
          add_vertex_labels/3,
          no_vertices/1,
          add_edge/2,
          alter_edge/2,
          edges/1,
          edges/2,
          edge/2,
          no_edges/1,
          del_edge/2,
          del_edges/2
        ]).

% Not yet implemented: ( In-Progress )

% del_path
% get_cycle
% get_path
% get_short_cycle
% get_short_path
% in_degree
% in_edges
% in_neighbours
% info
% out_degree
% out_edges
% out_neighbours

-define(SERVER,dg_srv).  

new() ->
    {ok,GraphName} = gen_server:call(?SERVER,create_graph).

new(Name) ->
    {ok,GraphName} = gen_server:call(?SERVER,{create_graph,Name}).

new(Name,Type) ->
    {ok,GraphName} = gen_server:call(?SERVER,{create_graph,Name,Type}).

del_graph(GraphName) ->
    gen_server:call(?SERVER,{del_graph,GraphName}).

graphs() ->
    gen_server:call(?SERVER,tables).

rename_graph(OldName,NewName) ->
    gen_server:call(?SERVER,{rename_graph,OldName,NewName}).

%%-------------------------------------------------------------------------------------------------------------------------
%% Vertices:

add_vertex(Properties) when size(Properties) >=1; 
                            size(Properties) =<3 ->
    {ok,GraphName} = gen_server:call(?SERVER,create_graph),
    true = add_vertex(GraphName,Properties),
    {ok,GraphName}.

add_vertex(GraphName,Properties) when size(Properties) >=1; 
                                      size(Properties) =<3 ->
    gen_server:call(?SERVER,{add_vertex,GraphName,Properties}).

vertices(GraphName) ->
    gen_server:call(?SERVER,{vertices,GraphName}). 

vertex(GraphName,VertexName) ->
    gen_server:call(?SERVER,{vertex,GraphName,VertexName}).

% vertex_alias(GraphName,VertexAlias) ->
%     gen_server:call(?SERVER,{vertex_alias,GraphName,VertexAlias}).

rename_vertex(GraphName,VertexName,NewVertexName) -> 
    gen_server:call(?SERVER,{rename_vertex,GraphName,VertexName,NewVertexName}).

set_vertex_alias(GraphName,VertexName,NewAlias) ->
    gen_server:call(?SERVER,{rename_vertex_alias,GraphName,VertexName,NewAlias}).

rename_vertex_alias(GraphName,VertexName,Alias,NewAlias) ->
    gen_server:call(?SERVER,{rename_vertex_alias,GraphName,VertexName,Alias,NewAlias}).

del_vertex(GraphName,VertexName) ->
    gen_server:call(?SERVER,{del_vertex,GraphName,VertexName}).

del_vertices(GraphName,Vertices) ->
    gen_server:call(?SERVER,{del_vertices,GraphName,Vertices}).

add_vertex_labels(GraphName,VertexName,Labels) ->
    gen_server:call(?SERVER,{add_vertex_labels,GraphName,VertexName,Labels}).

no_vertices(GraphName) ->
    gen_server:call(?SERVER,{no_vertices,GraphName}).

%%-------------------------------------------------------------------------------------------------------------------------
%% Edges

add_edge(GraphName,Properties) when size(Properties) =:= 2;
                                    size(Properties) =:= 3 ->
    gen_server:call(?SERVER,{add_edge,GraphName,Properties}).

alter_edge(GraphName,Properties) when size(Properties) =:= 3;
                                      size(Properties) =:= 4 ->
    gen_server:call(?SERVER,{alter_edge,GraphName,Properties}).

edges(GraphName) ->
  gen_server:call(?SERVER,{edges,GraphName}).

edges(GraphName,VertexName) ->
    gen_server:call(?SERVER,{edges,GraphName,VertexName}).

edge(GraphName,Edge) ->
    gen_server:call(?SERVER,{edge,GraphName,Edge}).

no_edges(GraphName) ->
    gen_server:call(?SERVER,{no_edges,GraphName}).

del_edge(GraphName,Edge) ->
    gen_server:call(?SERVER,{del_edge,GraphName,Edge}).

del_edges(GraphName,Edges) ->
    gen_server:call(?SERVER,{del_edges,GraphName,Edges}).
%%-------------------------------------------------------------------------------------------------------------------------
