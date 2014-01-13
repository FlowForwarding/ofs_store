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
%% @doc Userspace implementation of the OpenFlow Switch logic.
-module(linc_us4).

-export([handle_message/2]).

%% Handle all message types
-export([ofp_flow_mod/2,
         ofp_table_mod/2,
         ofp_port_mod/2,
         ofp_group_mod/2,
         ofp_get_config_request/2,
         ofp_set_config/2,
         ofp_queue_get_config_request/2,
         ofp_flow_stats_request/2,  % only way to see flow tables
         ofp_port_desc_request/2,
         ofp_group_desc_request/2,
         ofp_meter_mod/2,
         ofp_meter_config_request/2]).

-define(V4, 4).

-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("of_protocol/include/ofp_v4.hrl").
-include("ofs_store.hrl").
-include("ofs_store_logger.hrl").
-include("linc_us4.hrl").

% XXX temporary - until code is ported
-record(state, {
    switch_id,
    switch_config
}).
-type state() :: #state{}.

%%%-----------------------------------------------------------------------------
%%% main entry
%%%-----------------------------------------------------------------------------

-spec handle_message(datapath_id(), ofp_message_body()) -> ok.
handle_message(DatapathId, MessageBody) ->
    MessageName = element(1, MessageBody),
    erlang:apply(?MODULE, MessageName, [DatapathId, MessageBody]).

%%%-----------------------------------------------------------------------------
%%% Handling of messages
%%%-----------------------------------------------------------------------------

%% @doc Modify flow entry in the flow table.
-spec ofp_flow_mod(datapath_id(), ofp_flow_mod()) -> ok | {error, term()}.
ofp_flow_mod(DatapathId, FlowMod) ->
    linc_us4_flow:modify(DatapathId, FlowMod).

%% @doc Modify flow table configuration.
-spec ofp_table_mod(state(), ofp_table_mod()) ->
                           {noreply, #state{}} |
                           {reply, ofp_message(), #state{}}.
ofp_table_mod(State, #ofp_table_mod{} = TableMod) ->
    case linc_us4_flow:table_mod(TableMod) of
        ok ->
            {noreply, State};
        {error, {Type, Code}} ->
            ErrorMsg = #ofp_error_msg{type = Type,
                                      code = Code},
            {reply, ErrorMsg, State}
    end.

%% @doc Modify port configuration.
-spec ofp_port_mod(state(), ofp_port_mod()) ->
                          {noreply, #state{}} |
                          {reply, ofp_message(), #state{}}.
ofp_port_mod(#state{switch_id = SwitchId} = State,
             #ofp_port_mod{} = PortMod) ->
    case linc_us4_port:modify(SwitchId, PortMod) of
        ok ->
            {noreply, State};
        {error, {Type, Code}} ->
            ErrorMsg = #ofp_error_msg{type = Type,
                                      code = Code},
            {reply, ErrorMsg, State}
    end.

%% @doc Modify group entry in the group table.
-spec ofp_group_mod(state(), ofp_group_mod()) ->
                           {noreply, #state{}} |
                           {reply, ofp_message(), #state{}}.
ofp_group_mod(#state{switch_id = SwitchId} = State,
              #ofp_group_mod{} = GroupMod) ->
    case linc_us4_groups:modify(SwitchId, GroupMod) of
        ok ->
            {noreply, State};
        {error, ErrorMsg} ->
            {reply, ErrorMsg, State}
    end.

%% @doc Reply to get config request.
-spec ofp_get_config_request(state(), ofp_get_config_request()) ->
                                    {reply, ofp_get_config_reply(), #state{}}.
ofp_get_config_request(#state{switch_config = SwitchConfig} = State,
                       #ofp_get_config_request{}) ->
    ConfigReply = #ofp_get_config_reply{flags = proplists:get_value(
                                                  flags,
                                                  SwitchConfig),
                                        miss_send_len = proplists:get_value(
                                                          miss_send_len,
                                                          SwitchConfig)},
    {reply, ConfigReply, State}.

%% @doc Set switch configuration.
-spec ofp_set_config(state(), ofp_set_config()) -> {noreply, state()}.
ofp_set_config(State, #ofp_set_config{flags = Flags,
                                      miss_send_len = MissSendLength}) ->
    SwitchConfig = [{flags, Flags}, {miss_send_len, MissSendLength}],
    {noreply, State#state{switch_config = SwitchConfig}}.

%% @doc Reply to get queue config request.
-spec ofp_queue_get_config_request(state(), ofp_queue_get_config_request()) ->
                                          {reply, ofp_get_config_reply(),
                                           #state{}}.
ofp_queue_get_config_request(State,
                             #ofp_queue_get_config_request{port = Port}) ->
    QueueConfigReply = #ofp_queue_get_config_reply{port = Port,
                                                   queues = []},
    {reply, QueueConfigReply, State}.

%% @doc Get flow entry statistics.
-spec ofp_flow_stats_request(datapath_id(), ofp_flow_stats_request()) ->
                                    ofp_message().
ofp_flow_stats_request(DatapathId, #ofp_flow_stats_request{} = Request) ->
    of_msg_lib:create_message(?V4, linc_us4_flow:get_stats(DatapathId, Request)).

%% @doc Get port description.
-spec ofp_port_desc_request(state(), ofp_port_desc_request()) ->
                                   {reply, ofp_port_desc_reply(), #state{}}.
ofp_port_desc_request(#state{switch_id = SwitchId} = State,
                      #ofp_port_desc_request{}) ->
    Reply = linc_us4_port:get_desc(SwitchId),
    {reply, Reply, State}.

%% @doc Get group description statistics.
-spec ofp_group_desc_request(state(), ofp_group_desc_request()) ->
                                    {reply, ofp_group_desc_reply(), #state{}}.
ofp_group_desc_request(#state{switch_id = SwitchId} = State,
                       #ofp_group_desc_request{} = Request) ->
    Reply = linc_us4_groups:get_desc(SwitchId, Request),
    {reply, Reply, State}.

%% Meters ----------------------------------------------------------------------

ofp_meter_mod(#state{switch_id = SwitchId} = State,
              #ofp_meter_mod{} = MeterMod) ->
    case linc_us4_meter:modify(SwitchId, MeterMod) of
        noreply ->
            {noreply, State};
        {reply, Reply} ->
            {reply, Reply, State}
    end.

ofp_meter_config_request(#state{switch_id = SwitchId} = State,
                         #ofp_meter_config_request{meter_id = Id}) ->
    {reply, linc_us4_meter:get_config(SwitchId, Id), State}.

%%%-----------------------------------------------------------------------------
%%% Helpers
%%%-----------------------------------------------------------------------------
