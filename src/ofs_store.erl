-module(ofs_store).

-export([request/1,
         clear/1,
         dump/1]).

-include_lib("of_protocol/include/of_protocol.hrl").
-include("ofs_store.hrl").

request(Request = #ofs_store_request{}) ->
    ofs_store_logic:request(Request).

% debugging/testing
clear(Table) ->
    ofs_store_db:clear(Table).

dump(Table) ->
    ofs_store_db:dump(Table).
