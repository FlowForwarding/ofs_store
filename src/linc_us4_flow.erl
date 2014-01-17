%%------------------------------------------------------------------------------
%% Copyright 2012 FlowForwarding.org
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%-----------------------------------------------------------------------------

%% @author Erlang Solutions Ltd. <openflow@erlang-solutions.com>
%% @copyright 2012 FlowForwarding.org
%% @doc Module for handling flows.
-module(linc_us4_flow).

% XXX may have lost idle time/hard timeout for flow
% XXX should not use "|| Id <- lists:seq(0, ?OFPTT_MAX)]"

%% API
-export([initialize/0,
         table_mod/1,
         modify/2,
         get_flow_table/2,
         delete_where_group/2,
         delete_where_meter/2,
         clear_table_flows/2,
         get_stats/2,
         set_table_config/3,
         get_table_config/2]).

-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("of_protocol/include/ofp_v4.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("ofs_store_logger.hrl").
-include("ofs_store.hrl").
-include("linc_us4.hrl").

-define(MAX64, 16#FFFFFFFF). % Max countervalue for 64 bit counters
-define(INSTRUCTIONS, [ofp_instruction_goto_table, ofp_instruction_write_metadata,
                       ofp_instruction_write_actions, ofp_instruction_apply_actions,
                       ofp_instruction_clear_actions, ofp_instruction_experimenter]).

%% @doc Initialize the flow tables. Only to be called on system startup.
-spec initialize() -> ok.
initialize() ->
    ets:new(flow_table_config,
                              [public,
                               {keypos, #flow_table_config.id},
                               {read_concurrency, true}]),

    % XXX create one big flow table.  Compound index of table id and flow id?
    % no need to prepopulate.  Only load in config information as it's
    % provided by the NE
    % create_flow_table().
    ok.


%% @doc Handle ofp_table_mod request
%% In version 1.3 This doesn't do anything anymore.
-spec table_mod(#ofp_table_mod{}) -> ok.
table_mod(#ofp_table_mod{}) ->
    ok.

%% @doc Handle a flow_mod request from a controller.
%% This may add/modify/delete one or more flows.
-spec modify(datapath_id(), #ofp_flow_mod{}) ->
                    ok | {error, {Type :: atom(), Code :: atom()}}.
modify(_DatapathId, #ofp_flow_mod{command = Cmd, table_id = all})
  when Cmd == add orelse Cmd == modify orelse Cmd == modify_strict ->
    {error, {flow_mod_failed, bad_table_id}};
modify(DatapathId, #ofp_flow_mod{command = add,
                               table_id = TableId,
                               priority = Priority,
                               flags = Flags,
                               instructions = Instructions,
                               match = Match} = FlowMod) ->
    case lists:member(check_overlap, Flags) of
        true ->
            add_new_flow(DatapathId, TableId, FlowMod);
        false ->
            %% Check if there is any entry with the exact same 
            %% priority and match
            case find_exact_match(DatapathId, TableId, Priority, Match) of
                #flow_entry{} ->
                    replace_existing_flow(DatapathId, TableId, Priority,
                                          Match, Instructions);
                no_match ->
                    add_new_flow(DatapathId, TableId, FlowMod)
            end
    end;
modify(DatapathId, #ofp_flow_mod{command = modify,
                               cookie = Cookie,
                               cookie_mask = CookieMask,
                               table_id = TableId,
                               match = #ofp_match{fields = Match},
                               instructions = Instructions}) ->
    modify_matching_flows(DatapathId, TableId, Cookie, CookieMask,
                                  Match, Instructions),
    ok;
modify(DatapathId, #ofp_flow_mod{command = modify_strict,
                               table_id = TableId,
                               priority = Priority,
                               match = Match,
                               instructions = Instructions}) ->
    replace_existing_flow(DatapathId, TableId, Priority, Match, Instructions);
modify(DatapathId, #ofp_flow_mod{command = Cmd, table_id = all} = FlowMod)
  when Cmd == delete; Cmd == delete_strict ->
    [modify(DatapathId, FlowMod#ofp_flow_mod{table_id = Id})
     || Id <- lists:seq(0, ?OFPTT_MAX)],
    ok;
modify(DatapathId, #ofp_flow_mod{command = delete,
                               table_id = TableId,
                               cookie = Cookie,
                               cookie_mask = CookieMask,
                               out_port = OutPort,
                               out_group = OutGroup,
                               match = #ofp_match{fields = Match}}) ->
    delete_matching_flows(DatapathId, TableId, Cookie, CookieMask,
                          Match, OutPort, OutGroup),
    ok;
modify(DatapathId, #ofp_flow_mod{command = delete_strict,
                                table_id = TableId,
                                priority = Priority,
                                match = #ofp_match{fields = Match}}) ->
    case find_exact_match(DatapathId, TableId, Priority, Match) of
        #flow_entry{} = FlowEntry ->
            delete_flow(FlowEntry);
        _ ->
            %% Do nothing
            ok
    end.

%% @doc Get all entries in one flow table.
-spec get_flow_table(integer(), integer()) -> [FlowTableEntryRepr :: term()].
get_flow_table(SwitchId, TableId) ->
    lists:reverse(ets:tab2list(flow_table_ets(SwitchId, TableId))).

%% @doc Delete all flow entries that are using a specific group.
-spec delete_where_group(integer(), integer()) -> ok.
delete_where_group(SwitchId, GroupId) ->
    [delete_where_group(SwitchId, GroupId, TableId)
     || TableId <- lists:seq(0, ?OFPTT_MAX)],
    ok.

%% @doc Delete all flow entries that are pointing to a given meter.
-spec delete_where_meter(integer(), integer()) -> ok.
delete_where_meter(SwitchId, MeterId) ->
    [delete_where_meter(SwitchId, MeterId, TableId)
     || TableId <- lists:seq(0, ?OFPTT_MAX)],
    ok.

%% @doc Delete all flow entries in the given flow table.
%%
%% `ofp_flow_removed' events are not sent.
-spec clear_table_flows(integer(), 0..?OFPTT_MAX) -> ok.
clear_table_flows(SwitchId, TableId) ->
    ets:foldl(fun(#flow_entry{} = FlowEntry, _Acc) ->
                      delete_flow(FlowEntry)
              end, ok, flow_table_ets(SwitchId, TableId)).

%% @doc Get flow statistics.
-spec get_stats(datapath_id(), #ofp_flow_stats_request{}) -> #ofp_flow_stats_reply{}.
get_stats(DatapathId, #ofp_flow_stats_request{table_id = all,
                                            out_port = OutPort,
                                            out_group = OutGroup,
                                            cookie = Cookie,
                                            cookie_mask = CookieMask,
                                            match = #ofp_match{fields=Match}}) ->
    Stats = [get_flow_stats(DatapathId, TableId, Cookie, CookieMask,
                            Match, OutPort,
                            OutGroup) || TableId <- lists:seq(0, ?OFPTT_MAX)],
    #ofp_flow_stats_reply{body = lists:concat(Stats)};

get_stats(DatapathId, #ofp_flow_stats_request{table_id = TableId,
                                            out_port = OutPort,
                                            out_group = OutGroup,
                                            cookie = Cookie,
                                            cookie_mask = CookieMask,
                                            match = #ofp_match{fields=Match}}) ->
    Stats = get_flow_stats(DatapathId, TableId, Cookie, CookieMask,
                           Match, OutPort, OutGroup),
    #ofp_flow_stats_reply{body = Stats}.


-spec set_table_config(integer(), integer(), linc_table_config()) -> ok.
set_table_config(SwitchId, TableId, Config) ->
    true = ets:insert(linc:lookup(SwitchId, flow_table_config),
                      #flow_table_config{id = TableId, config = Config}),
    ok.

-spec get_table_config(integer(), integer()) -> linc_table_config().
get_table_config(SwitchId, TableId) ->
    case ets:lookup(linc:lookup(SwitchId, flow_table_config), TableId) of
        [#flow_table_config{config = Config}] ->
            Config;
        [] ->
            drop
    end.

%%=============================================================================

%% Return flow table name for table id.
flow_table_name(TableId) ->
    list_to_atom(lists:concat([flow_table_,TableId])).

%% Return flow table ETS tid for given switch id and table id.
flow_table_ets(SwitchId, TableId) ->
    TableName = flow_table_name(TableId),
    linc:lookup(SwitchId, TableName).

%% Add a new flow entry.
add_new_flow(DatapathId, TableId, FlowMod) ->
    NewEntry = create_flow_entry(DatapathId, TableId, FlowMod),
    ok = ofs_store_db:insert_flow_entry(NewEntry),
    ?DEBUG("[FLOWMOD] Added new flow entry with id ~w: ~w",
           [NewEntry#flow_entry.id, FlowMod]),
    ok.

%% Replace an existing flow
replace_existing_flow(DatapathId, TableId, Priority, Match, Instructions) ->
    ofs_store_db:update_flow_entry(DatapathId, TableId, Priority, Match,
                                                                Instructions).

%% Delete a flow
delete_flow(#flow_entry{id = FlowId}) ->
    ok = ofs_store_db:delete_flow_entry(FlowId).

-spec create_flow_entry(datapath_id(), table_id(), ofp_flow_mod()) -> #flow_entry{}.
create_flow_entry(DatapathId, TableId, #ofp_flow_mod{priority = Priority,
                                cookie = Cookie,
                                flags = Flags,
                                match = Match,
                                instructions = Instructions}) ->
    #flow_entry{id = make_ref(),
                datapath_id = DatapathId,
                table_id = TableId,
                priority = Priority,
                cookie = Cookie,
                flags = Flags,
                match = Match,
                %% All record of type ofp_instruction() MUST have
                %% seq number as a first element.
                instructions = lists:keysort(2, Instructions)}.

%%============================================================================
%% Various counter functions

get_flow_stats(DatapathId, TableId, Cookie, CookieMask,
               Match, OutPort, OutGroup) ->
    lists:foldl(fun (#flow_entry{cookie = MyCookie,
                               flags = Flags,
                               priority = Priority,
                               match = MyMatch,
                               instructions = Instructions}=FlowEntry, Acc) ->
                      case cookie_match(MyCookie, Cookie, CookieMask)
                          andalso non_strict_match(FlowEntry, Match)
                          andalso port_and_group_match(OutPort,
                                                       OutGroup,
                                                       Instructions)
                      of
                          true ->
                              Stats = #ofp_flow_stats{
                                         table_id = TableId,
                                         flags = Flags,
                                         priority = Priority,
                                         cookie = MyCookie,
                                         match = MyMatch,
                                         instructions = Instructions},
                              [Stats|Acc];
                          false ->
                              Acc
                      end
              end, [], ofs_store_db:get_flow_entries(DatapathId, TableId)).


%%============================================================================
%% Various lookup functions

%% Find an existing flow with the same Priority and the exact
%% same match expression.
find_exact_match(DatapathId, TableId, Priority, Match) ->
        ofs_store_db:get_flow_entry(DatapathId, TableId, Priority, Match).

%% Modify flows that are matching
modify_matching_flows(DatapathId, TableId, Cookie, CookieMask,
                      Match, Instructions) ->
    lists:foreach(fun (#flow_entry{cookie = MyCookie,
                                   match = MyMatch,
                                   priority = MyPriority} = FlowEntry) ->
                      case cookie_match(MyCookie, Cookie, CookieMask)
                          andalso non_strict_match(FlowEntry, Match) of
                          true ->
                              replace_existing_flow(DatapathId, TableId, MyPriority, MyMatch, Instructions);
                          false ->
                              do_nothing
                      end
              end, ofs_store_db:get_flow_entries(DatapathId, TableId)).

%% Delete flows that are matching 
delete_matching_flows(DatapathId, TableId, Cookie, CookieMask,
                      Match, OutPort, OutGroup) ->
    lists:foreach(fun (#flow_entry{cookie = MyCookie,
                               instructions = Instructions} = FlowEntry) ->
                        case cookie_match(MyCookie, Cookie, CookieMask)
                            andalso non_strict_match(FlowEntry, Match)
                            andalso port_and_group_match(OutPort,
                                                         OutGroup,
                                                         Instructions)
                        of
                            true ->
                                delete_flow(FlowEntry);
                            false ->
                                do_nothing
                      end
                end, ofs_store_db:get_flow_entries(DatapathId, TableId)).

non_strict_match(#flow_entry{match = #ofp_match{fields = EntryFields}},
                 FlowModFields) ->
    lists:all(fun(#ofp_field{name = Field} = FlowModField) ->
                      case lists:keyfind(Field, #ofp_field.name, EntryFields) of
                          #ofp_field{} = EntryField ->
                              is_more_specific(EntryField, FlowModField);
                          false ->
                              false
                      end
              end, FlowModFields);
non_strict_match(_FlowEntry, _Match) ->
    throw(#ofp_error_msg{type = bad_match, code = bad_type}).

cookie_match(Cookie1, Cookie2, CookieMask) ->
    mask_match(Cookie1, Cookie2, CookieMask).

mask_match(Bin1,Bin2,MaskBin) ->
    Bits = bit_size(Bin1),
    <<Val1:Bits>> = Bin1,
    <<Val2:Bits>> = Bin2,
    <<Mask:Bits>> = MaskBin,
    Val1 band Mask == Val2 band Mask.

is_more_specific(#ofp_field{class = Cls1}, #ofp_field{class = Cls2}) when
      Cls1 =/= openflow_basic; Cls2 =/= openflow_basic ->
    throw(#ofp_error_msg{type = bad_match, code = bad_field});
is_more_specific(#ofp_field{has_mask = true},
                 #ofp_field{has_mask = false}) ->
    false; %% masked is less specific than non-masked
is_more_specific(#ofp_field{has_mask = false, value = Value},
                 #ofp_field{has_mask = _____, value = Value}) ->
    true; %% value match with no mask is more specific
is_more_specific(#ofp_field{has_mask = true, mask = M1, value = V1},
                 #ofp_field{has_mask = true, mask = M2, value = V2}) ->
    %% M1 is more specific than M2 (has all of it's bits)
    %% and V1*M2 == V2*M2
    is_mask_more_specific(M1, M2)
        andalso
        mask_match(V1, V2, M2);
is_more_specific(_MoreSpecific, _LessSpecific) ->
    false.

-spec is_mask_more_specific(binary(), binary()) -> boolean().
is_mask_more_specific(Bin1, Bin2) ->
    Bits = bit_size(Bin1),
    <<Mask1:Bits>> = Bin1,
    <<Mask2:Bits>> = Bin2,
    Mask1 bor Mask2 == Mask1.

port_and_group_match(any,any,_Instructions) ->
    true;
port_and_group_match(Port,Group,
                     [#ofp_instruction_write_actions{actions=Actions}|Instructions]) ->
    port_and_group_match_actions(Port,Group,Actions) 
        orelse port_and_group_match(Port,Group,Instructions);
port_and_group_match(Port,Group,
                     [#ofp_instruction_apply_actions{actions=Actions}|Instructions]) ->
    port_and_group_match_actions(Port,Group,Actions) 
        orelse port_and_group_match(Port,Group,Instructions);
port_and_group_match(Port,Group,[_|Instructions]) ->
    port_and_group_match(Port,Group,Instructions);
port_and_group_match(_Port,_Group,[]) ->
    false.

port_and_group_match_actions(OutPort,_OutGroup,
                             [#ofp_action_output{port=OutPort}|_]) ->
    true;
port_and_group_match_actions(_OutPort,OutGroup,
                             [#ofp_action_group{group_id=OutGroup}|_]) ->
    true;
port_and_group_match_actions(OutPort,OutGroup,[_|Instructions]) ->
    port_and_group_match_actions(OutPort,OutGroup,Instructions);
port_and_group_match_actions(_OutPort,_OutGroup,[]) ->
    false.

%% Remove all flows that have output to GroupId.
delete_where_group(SwitchId, GroupId, TableId) ->
    ets:foldl(fun (#flow_entry{instructions=Instructions}=FlowEntry, Acc) ->
                      case port_and_group_match(any, GroupId, Instructions) of
                          true ->
                              delete_flow(FlowEntry);
                          false ->
                              Acc
                      end
              end, ok, flow_table_ets(SwitchId, TableId)).

%% Remove all flows that use MeterId.
delete_where_meter(SwitchId, MeterId, TableId) ->
    ets:foldl(fun (#flow_entry{instructions=Instructions}=FlowEntry, Acc) ->
                      case meter_match(MeterId, Instructions) of
                          true ->
                              delete_flow(FlowEntry);
                          false ->
                              Acc
                      end
              end, ok, flow_table_ets(SwitchId, TableId)).

meter_match(MeterId, Instructions) ->
    [MeterId] == [Id || #ofp_instruction_meter{meter_id=Id} <- Instructions, Id==MeterId].
