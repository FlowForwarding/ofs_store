-module(ofs_store).

-export(update/2).

-include("ofs_store.hrl").

update(Request = #ofs_store_request{}) ->
    gen_server:call(ofs_store, {update, Request}).
