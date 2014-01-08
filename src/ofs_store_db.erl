-module(ofs_store_db).

-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("of_protocol/include/ofp_v4.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("ofs_store_logger.hrl").
-include("ofs_store.hrl").
-include("linc_us4.hrl").

-export([install/0,
         insert_flow_entry/1,
         find_exact_match_flow/4
        ]).

install() ->
    try
        install_try()
    catch
        C:E ->
            ?ERROR("DB install failed: ~p:~p~n~p~n",
                                        [C, E, erlang:get_stacktrace()])
            % XXX crash?
    end,
    ok.

%% insert new flow
-spec insert_flow_entry(flow_entry()) -> ok.
insert_flow_entry(FlowEntry) ->
    mnesia:dirty_write(FlowEntry).

-spec find_exact_match_flow(datapath_id(), table_id(), priority(), ofp_match()) -> flow_entry() | no_match.
find_exact_match_flow(DatapathId, TableId, Priority, Match) ->
    Pattern = ets:fun2ms(fun (#flow_entry{datapath_id = Datap,
                                          table_id = Tabl,
                                          priority = Prio,
                                          match=#ofp_match{fields=Fields}}=Flow)
                               when Datap==DatapathId, 
                                    Tabl==TableId, 
                                    Prio==Priority, 
                                    Fields==Match ->
                                 Flow
                         end),
    case mnesia:dirty_select(flow_entry, Pattern) of
        [Flow|_] =_Match->
            Flow;
        [] ->
            no_match
    end.

%% Helper functions

install_try() ->
    application:stop(mnesia),
    mnesia:create_schema([node()]),
    application:start(mnesia),
    create_tables(),
    wait_for_tables(),
    ok.

create_tables() ->
    [mnesia:create_table(T, D) || {T, D} <- table_defs()].

wait_for_tables() ->
    Tables = [T || {T, _D} <- table_defs()],
    ok = mnesia:wait_for_tables(Tables, infinity).

table_defs() ->
    [
         {flow_entry, [{attributes, record_info(fields, flow_entry)}]}
    ].
