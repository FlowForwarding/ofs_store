%%------------------------------------------------------------------------------
%% Copyright 2014 FlowForwarding.org
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
%% @copyright 2014 FlowForwarding.org

%% @doc Switch level data store.

-module(ofs_store_logic).

-behaviour(gen_server).

%% API
-export([request/1,
         %% Backend general
         get_backend_flow_tables/1,
         %% Backend ports
         get_backend_ports/1,
         get_port_config/2,
         set_port_config/3,
         get_port_features/2,
         set_port_features/3,
         is_port_valid/2,
         %% Backend queues
         get_backend_queues/1,
         get_queue_min_rate/3,
         set_queue_min_rate/4,
         get_queue_max_rate/3,
         set_queue_max_rate/4,
         is_queue_valid/3
        ]).

%% Internal API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("of_config/include/of_config.hrl").
-include("ofs_store.hrl").
-include("ofs_store_logger.hrl").

-record(state, {
    datapath_id,
    switch_id,
    ofconfig_backend_mod,
    backend_mod,
    backend_state
 }).

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

request(Request = #ofs_store_request{}) ->
    gen_server:call(?MODULE, {request, Request}).

-spec get_backend_flow_tables(integer()) -> list(#flow_table{}).
get_backend_flow_tables(SwitchId) ->
    gen_server:call(linc:lookup(SwitchId, linc_logic), get_backend_flow_tables).

-spec get_backend_ports(integer()) -> list(#port{}).
get_backend_ports(SwitchId) ->
    gen_server:call(linc:lookup(SwitchId, linc_logic), get_backend_ports).

-spec get_port_config(integer(), integer()) -> #port_configuration{}.
get_port_config(SwitchId, PortNo) ->
    gen_server:call(linc:lookup(SwitchId, linc_logic), {get_port_config,
                                                        PortNo}).

-spec set_port_config(integer(), integer(), #port_configuration{}) -> ok.
set_port_config(SwitchId, PortNo, PortConfig) ->
    gen_server:cast(linc:lookup(SwitchId, linc_logic), {set_port_config,
                                                        PortNo, PortConfig}).

-spec get_port_features(integer(), integer()) -> #port_features{}.
get_port_features(SwitchId, PortNo) ->
    gen_server:call(linc:lookup(SwitchId, linc_logic), {get_port_features,
                                                        PortNo}).

-spec set_port_features(integer(), integer(), #port_features{}) -> ok.
set_port_features(SwitchId, PortNo, PortFeatures) ->
    gen_server:cast(linc:lookup(SwitchId, linc_logic), {set_port_features,
                                                        PortNo, PortFeatures}).

-spec is_port_valid(integer(), integer()) -> boolean().
is_port_valid(SwitchId, PortNo) ->
    gen_server:call(linc:lookup(SwitchId, linc_logic), {is_port_valid, PortNo}).

-spec get_backend_queues(integer()) -> list(#queue{}).
get_backend_queues(SwitchId) ->
    gen_server:call(linc:lookup(SwitchId, linc_logic), get_backend_queues).

-spec get_queue_min_rate(integer(), integer(), integer()) -> integer().
get_queue_min_rate(SwitchId, PortNo, QueueId) ->
    gen_server:call(linc:lookup(SwitchId, linc_logic), {get_queue_min_rate,
                                                        PortNo, QueueId}).

-spec set_queue_min_rate(integer(), integer(), integer(), integer()) -> ok.
set_queue_min_rate(SwitchId, PortNo, QueueId, Rate) ->
    gen_server:cast(linc:lookup(SwitchId, linc_logic), {set_queue_min_rate,
                                                        PortNo, QueueId, Rate}).

-spec get_queue_max_rate(integer(), integer(), integer()) -> integer().
get_queue_max_rate(SwitchId, PortNo, QueueId) ->
    gen_server:call(linc:lookup(SwitchId, linc_logic), {get_queue_max_rate,
                                                        PortNo, QueueId}).

-spec set_queue_max_rate(integer(), integer(), integer(), integer()) -> ok.
set_queue_max_rate(SwitchId, PortNo, QueueId, Rate) ->
    gen_server:cast(linc:lookup(SwitchId, linc_logic), {set_queue_max_rate,
                                                        PortNo, QueueId, Rate}).

-spec is_queue_valid(integer(), integer(), integer()) -> boolean().
is_queue_valid(SwitchId, PortNo, QueueId) ->
    gen_server:call(linc:lookup(SwitchId, linc_logic), {is_queue_valid,
                                                        PortNo, QueueId}).

%% @doc Start the OF Switch logic.
-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call({request, Request}, _From, State) ->
    #ofs_store_request{datapath_id = DataPathId, message = Message} = Request,
    #ofp_message{version = Version, body = Body} = Message,
    ProtocolModule = protocol_module(Version),
    Reply = ProtocolModule:handle_message(DataPathId, Body),
    {reply, Reply, State};
handle_call(get_datapath_id, _From, #state{datapath_id = DatapathId} = State) ->
    {reply, DatapathId, State};
handle_call(get_backend_flow_tables, _From,
            #state{ofconfig_backend_mod = OFConfigBackendMod,
                   datapath_id = DatapathId,
                   switch_id = SwitchId} = State) ->
    FlowTables = OFConfigBackendMod:get_flow_tables(SwitchId, DatapathId),
    {reply, FlowTables, State};
handle_call(get_backend_ports, _From,
            #state{ofconfig_backend_mod = OFConfigBackendMod,
                   switch_id = SwitchId} = State) ->
    Ports = OFConfigBackendMod:get_ports(SwitchId),
    {reply, Ports, State};
handle_call({get_port_config, PortNo}, _From,
            #state{ofconfig_backend_mod = OFConfigBackendMod,
                   switch_id = SwitchId} = State) ->
    PortConfig = OFConfigBackendMod:get_port_config(SwitchId, PortNo),
    {reply, PortConfig, State};
handle_call({get_port_features, PortNo}, _From,
            #state{ofconfig_backend_mod = OFConfigBackendMod,
                   switch_id = SwitchId} = State) ->
    PortFeatures = OFConfigBackendMod:get_port_features(SwitchId, PortNo),
    {reply, PortFeatures, State};
handle_call(get_backend_queues, _From,
            #state{ofconfig_backend_mod = OFConfigBackendMod,
                   switch_id = SwitchId} = State) ->
    BackendQueues = OFConfigBackendMod:get_queues(SwitchId),
    RealQueues = lists:filter(fun(#queue{id = default}) ->
                                      false;
                                 (#queue{})->
                                      true
                              end, BackendQueues),
    {reply, RealQueues, State};
handle_call(get_queue_min_rate, _From,
            #state{ofconfig_backend_mod = OFConfigBackendMod,
                   switch_id = SwitchId} = State) ->
    BackendQueues = OFConfigBackendMod:get_queue_min_rate(SwitchId),
    {reply, BackendQueues, State};
handle_call(get_queue_max_rate, _From,
            #state{ofconfig_backend_mod = OFConfigBackendMod,
                   switch_id = SwitchId} = State) ->
    BackendQueues = OFConfigBackendMod:get_queue_max_rate(SwitchId),
    {reply, BackendQueues, State};
handle_call({is_port_valid, PortNo}, _From,
            #state{backend_mod = BackendMod,
                   switch_id = SwitchId} = State) ->
    Validity = BackendMod:is_port_valid(SwitchId, PortNo),
    {reply, Validity, State};
handle_call({is_queue_valid, PortNo, QueueId}, _From,
            #state{backend_mod = BackendMod,
                   switch_id = SwitchId} = State) ->
    Validity = BackendMod:is_queue_valid(SwitchId, PortNo, QueueId),
    {reply, Validity, State};
handle_call(_Message, _From, State) ->
    {reply, ok, State}.

handle_cast({set_port_config, PortNo, PortConfig},
            #state{ofconfig_backend_mod = OFConfigBackendMod,
                   switch_id = SwitchId} = State) ->
    OFConfigBackendMod:set_port_config(SwitchId, PortNo, PortConfig),
    {noreply, State};
handle_cast({set_port_features, PortNo, PortFeatures},
            #state{ofconfig_backend_mod = OFConfigBackendMod,
                   switch_id = SwitchId} = State) ->
    OFConfigBackendMod:set_port_features(SwitchId, PortNo, PortFeatures),
    {noreply, State};
handle_cast({set_queue_min_rate, PortNo, QueueId, Rate},
            #state{ofconfig_backend_mod = OFConfigBackendMod,
                   switch_id = SwitchId} = State) ->
    OFConfigBackendMod:set_queue_min_rate(SwitchId, PortNo, QueueId, Rate),
    {noreply, State};
handle_cast({set_queue_max_rate, PortNo, QueueId, Rate},
            #state{ofconfig_backend_mod = OFConfigBackendMod,
                   switch_id = SwitchId} = State) ->
    OFConfigBackendMod:set_queue_max_rate(SwitchId, PortNo, QueueId, Rate),
    {noreply, State};
handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%%-----------------------------------------------------------------------------
%%% Helpers
%%%-----------------------------------------------------------------------------

protocol_module(4) ->
    linc_us4.
