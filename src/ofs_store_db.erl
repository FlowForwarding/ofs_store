-module(ofs_store_db).

-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("of_protocol/include/ofp_v4.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("ofs_store_logger.hrl").
-include("ofs_store.hrl").
-include("linc_us4.hrl").

-export([install/0,
         insert_flow_entry/1,
         update_flow_entry/5,
         delete_flow_entry/1,
         get_flow_entry/4,
         get_flow_entries/2,
         clear/1,
         dump/1
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
insert_flow_entry(FlowEntry = #flow_entry{}) ->
    mnesia:dirty_write(FlowEntry),
    ok.

-spec update_flow_entry(datapath_id(), table_id(), priority(), ofp_match(), [ofp_instruction()]) -> ok | no_match.
update_flow_entry(DatapathId, TableId, Priority, #ofp_match{fields = Match}, NewInstructions) ->
    case get_flow_entry(DatapathId, TableId, Priority, Match) of
        no_match ->
            no_match;
        OldEntry ->
            NewEntry = OldEntry#flow_entry{instructions = NewInstructions},
            ok = insert_flow_entry(NewEntry)
    end.

-spec delete_flow_entry(flow_id()) -> ok.
delete_flow_entry(FlowId) ->
    mnesia:dirty_delete(flow_entry, FlowId).

-spec get_flow_entry(datapath_id(), table_id(), priority(), [ofp_field()]) -> flow_entry() | no_match.
get_flow_entry(DatapathId, TableId, Priority, Match) ->
    Pattern = ets:fun2ms(fun (#flow_entry{datapath_id = Datap,
                                          table_id = Tabl,
                                          priority = Prio,
                                          match = #ofp_match{fields = Fields}} = Flow)
                               when Datap == DatapathId, 
                                    Tabl == TableId, 
                                    Prio == Priority, 
                                    Fields == Match ->
                                 Flow
                         end),
    case mnesia:dirty_select(flow_entry, Pattern) of
        [Flow|_] =_Match->
            Flow;
        [] ->
            no_match
    end.

-spec get_flow_entries(datapath_id(), table_id()) -> [flow_entry()].
get_flow_entries(DatapathId, TableId) ->
    Pattern = ets:fun2ms(fun (#flow_entry{datapath_id = Datap,
                                          table_id = Tabl} = Flow)
                               when Datap==DatapathId, Tabl==TableId ->
                                 Flow
                         end),
    mnesia:dirty_select(flow_entry, Pattern).

%% debugging/testing functions
clear(Table) ->
    {atomic, ok} = mnesia:clear_table(Table).

dump(Table) ->
    Iterator = fun(R, L) -> [R | L] end,
    Exec = fun({Fun, Tab}) -> mnesia:foldl(Fun, [], Tab) end,
    mnesia:activity(transaction, Exec, [{Iterator, Table}]).

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
