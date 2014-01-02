% registered name of ofs_store pid
-define(OFS_STORE_NAME, ofs_store).

-record(ofs_store_request, {
    datapath_id :: datapath_id(),
    request :: ofp_message()
}).
