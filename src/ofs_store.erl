-module(ofs_store).

-export([update/1]).

-include_lib("of_protocol/include/of_protocol.hrl").
-include("ofs_store.hrl").

update(Request = #ofs_store_request{}) ->
    ofs_store:update(Request).
