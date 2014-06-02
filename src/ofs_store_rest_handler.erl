-module (ofs_store_rest_handler).

-export([init/3,
         content_types_provided/2,
         handle_html/2,
         handle_json/2,
         handle_text/2
        ]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
    {[ {<<"text/html">>,        handle_html},
       {<<"application/json">>, handle_json},
       {<<"text/plain">>,       handle_text}
    ], Req, State}.

handle_html(Req, State) ->
    Body = <<"HTML Response">>,
    {Body, Req, State}.

handle_json(Req, State) ->
    Body = <<"{\"rest\": \"Hello World!\"}">>,
    {Body, Req, State}.

handle_text(Req, State) ->
    {<<"text">>, Req, State}.