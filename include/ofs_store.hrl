% XXX better definition for datapath_id
-type datapath_id() :: term().

-record(ofs_store_request, {
    datapath_id :: datapath_id(),
    message :: ofp_message()
}).
