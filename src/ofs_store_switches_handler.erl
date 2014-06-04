-module (ofs_store_switches_handler).

-export([init/3,
         allowed_methods/2,
         allow_missing_post/2,
         % charsets_provided/2,
         content_types_accepted/2,
         content_types_provided/2,
         resource_exists/2,
         % delete_completed/2,
         % delete_resource/2,
         % expires/2,
         % forbidden/2,
         % generate_etag/2,
         % is_authorized/2,
         % is_conflict/2,
         % known_content_type/2,
         known_methods/2,
         % languages_provided/2,
         % last_modified/2,
         % malformed_request/2,
         % moved_permanently/2,
         % moved_temporarily/2,
         % multiple_choices/2,
         % options/2,
         % previously_existed/2,
         % service_available/2,
         % uri_too_long/2,
         % valid_content_headers/2,
         % valid_entity_length/2,
         % variances/2,
         do_handle_json/2
        ]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.

allow_missing_post(Req,State) ->
    {true,Req,State}.

% charsets_provided(Req, State) ->
%     {[],Req,State}.

content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, do_handle_json}
     ], Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, do_handle_json}
     ],Req,State}.

resource_exists(Req,State) ->
    {true,Req,State}.

% delete_completed(Req,State) -> 
%     {true,Req,State}.

% delete_resource(Req,State) ->
%     {false,Req,State}.

% expires(Req,State) ->
%     {undefined,Req,State}.

% forbidden(Req,State) -> 
%     {false,Req,State}.

% generate_etag(Req,State) -> 
%     {undefined,Req,State}.

% is_authorized(Req,State) -> 
%     {true,Req,State}.

% is_conflict(Req,State) -> 
%     {false,Req,State}.

% known_content_type(Req,State) -> 
%     {true,Req,State}.

known_methods(Req,State) -> 
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>],Req,State}.

% languages_provided(Req,State) -> 
%     {[],Req,State}.

% last_modified(Req,State) -> 
%     {undefined,Req,State}.

% malformed_request(Req,State) -> 
%     {false,Req,State}.

% moved_permanently(Req,State) -> 
%     {false,Req,State}.

% moved_temporarily(Req,State) -> 
%     {false,Req,State}.

% multiple_choices(Req,State) -> 
%     {false,Req,State}.

% options(Req,State) ->
%     {ok,Req,State}.

% previously_existed(Req,State) -> 
%     {false,Req,State}.

% service_available(Req,State) -> 
%     {true,Req,State}.

% uri_too_long(Req,State) -> 
%     {false,Req,State}.

% valid_content_headers(Req,State) -> 
%     {true,Req,State}.

% valid_entity_length(Req,State) ->
%     {true,Req,State}.

% variances(Req,State) -> 
%     {[],Req,State}.

% handle_html(Req, State) ->,Req,State}
%     Body = <<"HTML Response">>,
%     {Body, Req, State}.

do_handle_json(Req, State) ->
    {Method,Req1} = cowboy_req:method(Req),
    do_handle_json_method(Req1,State,Method).

do_handle_json_method(Req,State,<<"PUT">>) ->
    do_add_vertex(Req,State);
do_handle_json_method(Req,State,<<"POST">>) ->
    do_add_vertex(Req,State).

do_add_vertex(Req,State) ->
    {ok,ReqBody,Req1} = cowboy_req:body(Req),
    {DecodedBody} = jiffy:decode(ReqBody),
    GraphName = proplists:get_value(<<"graph_name">>, DecodedBody,undefined),
    Dpid      = proplists:get_value(<<"datapath_id">>,DecodedBody),
    {Labels}  = proplists:get_value(<<"labels">>,     DecodedBody),
    Alias     = proplists:get_value(<<"alias">>, DecodedBody),
    Properties = {Dpid, Labels, Alias},
    Response = jiffy:encode(
        case GraphName of
            undefined ->
                {ok,NewGraphName} = dg:add_vertex(Properties),
                list_to_binary(NewGraphName);
            GraphName -> 
                case dg:add_vertex(GraphName,Properties) of
                    true  -> <<"true">>;
                    false -> <<"false">>
                end
        end
    ),
    {true, cowboy_req:set_resp_body(Response, Req1), State}.