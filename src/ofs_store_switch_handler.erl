-module (ofs_store_switch_handler).

-export([init/3,
         allowed_methods/2,
%         content_types_accepted/2,
         delete_completed/2,
         delete_resource/2,
         content_types_provided/2,
         resource_exists/2,
         known_methods/2,
         do_handle_plain/2
        ]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>,<<"DELETE">>], Req, State}.

% content_types_accepted(Req, State) ->
%     {[
%       {{<<"text">>, <<"plain">>, []}, do_handle_plain},
%       {{<<"text">>, <<"html">>, []}, do_handle_html},
%       {{<<"application">>, <<"json">>, []}, do_handle_json}
%      ], Req, State}.

delete_completed(Req,State) -> 
    {true,Req,State}.

delete_resource(Req,State) ->
    {GraphNameBin,Req1} = cowboy_req:binding(graph_name,Req),
    {Alias,Req2}        = cowboy_req:binding(alias,Req1),
    case dg:vertex_alias(GraphNameBin, Alias) of
        {ok,{Name,Labels,Alias}} ->
            {dg:del_vertex(GraphNameBin,Name),cowboy_req:set_resp_body(<<"true">>, Req1),State};
        false ->
            {false,cowboy_req:set_resp_body(<<"false">>, Req1),State}
    end.

content_types_provided(Req, State) ->
    {[{<<"*">>, do_handle_plain}],Req,State}.

resource_exists(Req,State) ->
    {true,Req,State}.

known_methods(Req,State) -> 
    {[<<"GET">>,<<"DELETE">>],Req,State}.

do_handle_plain(Req,State) ->
    {GraphNameBin,Req1} = cowboy_req:binding(graph_name,Req),
    {Alias,Req2}        = cowboy_req:binding(alias,Req1),
    case dg:vertex_alias(GraphNameBin,Alias) of
        false ->
            {<<"error, switch key does not exist">>,Req1,State};
        {ok,{Name,Labels,Alias}} ->
            Switch=
            {
                [
                    {<<"datapath_id">>,Name},
                    {<<"labels">>,     {Labels} },
                    {<<"alias">>,      Alias}
                ]
            },
            EncodedJSON = jiffy:encode(Switch),
            {EncodedJSON,Req1,State}
    end.