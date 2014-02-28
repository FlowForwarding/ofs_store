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

%% @doc Module implementing port's queues in userspace switch

-module(linc_us4_queue).

%% XXX rewrite so this stores config data in a table, not in process state.

%% Queue API
-export([attach_all/4,
         detach_all/2,
         set_min_rate/4,
         set_max_rate/4,
         get_all_queues_state/2,
         is_valid/3]).

%% Internal API
-export([start_link/2,
         initialize/1,
         terminate/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("of_config/include/of_config.hrl").
-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("of_protocol/include/ofp_v4.hrl").
-include("ofs_store_logger.hrl").
-include("ofs_store.hrl").
-include("linc_us4.hrl").
-include("linc_us4_queue.hrl").

%% have history of 10 buckets and total length of one second
-define(HIST_BUCKET_SIZE, 100).
-define(HIST_BUCKET_COUNT, 10).
-define(HIST_LEN_MS, (?HIST_BUCKET_SIZE * ?HIST_BUCKET_COUNT)).
-define(PAUSE_ON_FULL, 10). % 10ms
-define(DEFAULT_QUEUE, default).

-record(state, {queue_key :: {ofp_port_no(), ofp_queue_id()},
                resource_id :: string(),
                port_rate_bps :: integer(),
                min_rate_bps :: integer(),
                max_rate_bps :: integer(),
                port_rate :: integer(),
                min_rate :: integer(),
                max_rate :: integer(),
                history,
                send_fun,
                switch_id :: integer(),
                throttling_ets}).

%%------------------------------------------------------------------------------
%% Queue API
%%------------------------------------------------------------------------------

-spec attach_all(integer(), ofp_port_no(), fun(), [term()]) -> ok.
attach_all(SwitchId, PortNo, SendFun, QueuesConfig) ->
    ThrottlingETS = ets:new(queue_throttling,
                            [public,
                             {read_concurrency, true},
                             {keypos,
                              #linc_queue_throttling.queue_no}]),

    case lists:keyfind(PortNo, 2, QueuesConfig) of
        {port, PortNo, PortOpts} ->
            {port_rate, PortRateDesc} = lists:keyfind(port_rate, 1, PortOpts),
            {port_queues, PortQueues} = lists:keyfind(port_queues, 1, PortOpts);
        false ->
            PortRateDesc = {1000, mbps},
            PortQueues = []
    end,
    Sup = linc:lookup(SwitchId, linc_us4_queue_sup),
    lists:foreach(fun({QueueId, QueueProps}) ->
                          Args = [{PortNo, QueueId}, PortRateDesc,
                                  ThrottlingETS,
                                  SendFun, QueueProps],
                          supervisor:start_child(Sup, [Args])
                  end, [{?DEFAULT_QUEUE, []}] ++ PortQueues).

-spec detach_all(integer(), ofp_port_no()) -> ok.
detach_all(SwitchId, PortNo) ->
    lists:foreach(fun(#linc_port_queue{queue_pid = Pid}) ->
                          gen_server:call(Pid, detach)
                  end, get_queues(SwitchId, PortNo)).

%% @doc Return queue stats for the given OF port and queue id.

-spec set_min_rate(integer(), ofp_port_no(), ofp_queue_id(), integer()) ->
                          ok |
                          bad_queue.
set_min_rate(SwitchId, PortNo, QueueId, MinRate) ->
    case get_queue_pid(SwitchId, PortNo, QueueId) of
        {error, bad_queue} ->
            bad_queue;
        Pid ->
            gen_server:cast(Pid, {set_min_rate, MinRate})
    end.

-spec set_max_rate(integer(), ofp_port_no(), ofp_queue_id(), integer()) ->
                          ok |
                          bad_queue.
set_max_rate(SwitchId, PortNo, QueueId, MinRate) ->
    case get_queue_pid(SwitchId, PortNo, QueueId) of
        {error, bad_queue} ->
            bad_queue;
        Pid ->
            gen_server:cast(Pid, {set_max_rate, MinRate})
    end.

-spec get_all_queues_state(integer(), ofp_port_no()) ->
                                  tuple(string(), integer(), integer(),
                                        integer(), integer()).
get_all_queues_state(SwitchId, PortNo) ->
    lists:map(fun(#linc_port_queue{queue_pid = Pid}) ->
                      gen_server:call(Pid, get_state)
              end, get_queues(SwitchId, PortNo)).

-spec is_valid(integer(), ofp_port_no(), ofp_queue_id()) -> boolean().
is_valid(SwitchId, PortNo, QueueId) ->
    case get_queue_pid(SwitchId, PortNo, QueueId) of
        {error, bad_queue} ->
            false;
        _Pid ->
            true
    end.

%%------------------------------------------------------------------------------
%% Internal API
%%------------------------------------------------------------------------------

initialize(SwitchId) ->
    LincPortQueue = ets:new(linc_port_queue,
                            [public,
                             {keypos, #linc_port_queue.key},
                             {read_concurrency, true}]),
    linc:register(SwitchId, linc_port_queue, LincPortQueue),
    QueueSup = {linc_us4_queue_sup,
                {linc_us4_queue_sup, start_link, [SwitchId]},
                permanent, 5000, supervisor, [linc_us4_queue_sup]},
    supervisor:start_child(linc:lookup(SwitchId, linc_us4_sup), QueueSup).

terminate(SwitchId) ->
    true = ets:delete(linc:lookup(SwitchId, linc_port_queue)),
    ok.

start_link(SwitchId, QueueOpts) ->
    gen_server:start_link(?MODULE, [SwitchId, QueueOpts], []).

%%%-----------------------------------------------------------------------------
%%% gen_server callbacks
%%%-----------------------------------------------------------------------------

init([SwitchId, [{PortNo, QueueNo} = Key, PortRateDesc, ThrottlingETS,
                 _SendFun, QueueProps]]) ->

    PortRateBps = rate_desc_to_bps(PortRateDesc),
    MinRateBps = get_min_rate_bps(QueueProps, PortRateBps),
    MaxRateBps = get_max_rate_bps(QueueProps, PortRateBps),

    PortRate = bps_to_bphistlen(PortRateBps),
    MinRate = bps_to_bphistlen(MinRateBps),
    MaxRate = bps_to_bphistlen(MaxRateBps),

    LincPortQueue = linc:lookup(SwitchId, linc_port_queue),
    ets:insert(LincPortQueue, #linc_port_queue{key = Key,
                                               properties = QueueProps,
                                               queue_pid = self(),
                                               install_time = erlang:now()}),
    ets:insert(ThrottlingETS, #linc_queue_throttling{queue_no = QueueNo,
                                                     min_rate = MinRate,
                                                     max_rate = MaxRate,
                                                     rate = 0}),
    ResourceId =
        "LogicalSwitch" ++ integer_to_list(SwitchId) ++ "-" ++
        "Port" ++ integer_to_list(PortNo) ++ "-" ++
        case QueueNo of
            default -> "DefaultQueue";
            _ -> "Queue" ++ integer_to_list(QueueNo)
        end,
    {ok, #state{queue_key = Key,
                resource_id = ResourceId,
                port_rate_bps = PortRateBps,
                min_rate_bps = MinRateBps,
                max_rate_bps = MaxRateBps,
                port_rate = PortRate,
                min_rate = MinRate,
                max_rate = MaxRate,
                switch_id = SwitchId,
                throttling_ets = ThrottlingETS}}.

handle_call(detach, _From, #state{queue_key = QueueKey,
                                  switch_id = SwitchId,
                                  throttling_ets = ThrottlingETS} = State) ->
    {_PortNo, QueueId} = QueueKey,
    LincPortQueue = linc:lookup(SwitchId, linc_port_queue),
    ets:delete(LincPortQueue, QueueKey),
    ets:delete(ThrottlingETS, QueueId),
    {reply, ok, State};
handle_call(get_state, _From, #state{resource_id = ResourceId,
                                     queue_key = {PortNo, QueueId},
                                     min_rate_bps = MinRateBps,
                                     max_rate_bps = MaxRateBps} = State) ->
    Queue = {ResourceId, QueueId, PortNo, MinRateBps, MaxRateBps},
    {reply, Queue, State}.

handle_cast({set_min_rate, MinRatePercent},
            #state{queue_key = {_PortNo, QueueId},
                   port_rate_bps = PortRateBps,
                   throttling_ets = ThrottlingETS} = State) ->
    MinRateBps = get_min_rate_bps([{min_rate, MinRatePercent}], PortRateBps),
    MinRate = bps_to_bphistlen(MinRateBps),
    ets:update_element(ThrottlingETS, QueueId,
                       {#linc_queue_throttling.min_rate, MinRate}),
    {noreply, State#state{min_rate = MinRate}};
handle_cast({set_max_rate, MaxRatePercent},
            #state{queue_key = {_PortNo, QueueId},
                   port_rate_bps = PortRateBps,
                   throttling_ets = ThrottlingETS} = State) ->
    MaxRateBps = get_max_rate_bps([{max_rate, MaxRatePercent}], PortRateBps),
    MaxRate = bps_to_bphistlen(MaxRateBps),
    ets:update_element(ThrottlingETS, QueueId,
                       {#linc_queue_throttling.max_rate, MaxRate}),
    {noreply, State#state{max_rate = MaxRate}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------





bps_to_bphistlen(Bps) when is_integer(Bps) ->
    Bps * 1000 div ?HIST_LEN_MS;
bps_to_bphistlen(Special) when is_atom(Special) ->
    Special.

-spec get_queue_pid(integer(), ofp_port_no(), ofp_queue_id()) -> pid() |
                                                                 {error, bad_queue}.
get_queue_pid(SwitchId, PortNo, QueueId) ->
    LincPortQueue = linc:lookup(SwitchId, linc_port_queue),
    case ets:lookup(LincPortQueue, {PortNo, QueueId}) of
        [#linc_port_queue{queue_pid = Pid}] ->
            Pid;
        [] ->
            {error, bad_queue}
    end.

-spec get_queues(integer(), ofp_port_no()) -> list(#linc_port_queue{}).
get_queues(SwitchId, PortNo) ->
    LincPortQueue = linc:lookup(SwitchId, linc_port_queue),
    MatchSpec = #linc_port_queue{key = {PortNo, '_'}, _ = '_'},
    case catch ets:match_object(LincPortQueue, MatchSpec) of
        {'EXIT', _} ->
            [];
        Match ->
            Match
    end.

rate_desc_to_bps(Bps) when is_integer(Bps) ->
    Bps;
rate_desc_to_bps({Value, Unit}) ->
    Value * unit_to_bps(Unit).

unit_to_bps(bps) -> 1;
unit_to_bps(kbps) -> 1000;
unit_to_bps(kibps) -> 1024;
unit_to_bps(mbps) -> 1000 * 1000;
unit_to_bps(mibps) -> 1024 * 1024;
unit_to_bps(gbps) -> 1000 * 1000 * 1000;
unit_to_bps(gibps) -> 1024 * 1024 * 1024.

get_min_rate_bps(QueueProps, PortRateBps) ->
    case lists:keyfind(min_rate, 1, QueueProps) of
        {min_rate, Rate} when Rate =< 1000 ->
            Rate * PortRateBps div 1000;
        false ->
            no_qos
    end.

get_max_rate_bps(QueueProps, PortRateBps) ->
    case lists:keyfind(max_rate, 1, QueueProps) of
        {max_rate, Rate} when Rate =< 1000 ->
            Rate * PortRateBps div 1000;
        _ ->
            no_max_rate
    end.
