-module (ofs_store_rest_handler).

-export([init/3,
         content_types_provided/2,
%%       handle_html/2,
%%       handle_text/2
         handle_json/2
        ]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
    {[ {<<"text/html">>,        handle_json},
       {<<"application/json">>, handle_json},
       {<<"text/plain">>,       handle_json}
    ], Req, State}.

% handle_html(Req, State) ->
%     Body = <<"HTML Response">>,
%     {Body, Req, State}.

handle_json(Req, State) ->
    %% cowboy_req:has_body(Req),
    io:format("~p\n",[cowboy_req:body_qs(Req)]),
    Body = <<"{\"rest\": \"REST\"}">>,
    {Body, Req, State}.

% handle_text(Req, State) ->
%     {<<"text">>, Req, State}.