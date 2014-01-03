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
%% @doc Module for handling per-flow meters.
-module(linc_us4_meter).

% XXX rewrite so this is no longer a gen_server.  config should be in a table
-behaviour(gen_server).

%% API
-export([modify/2,
         update_flow_count/3,
         get_config/2,
         get_features/0,
         is_valid/2]).

%% Internal API
-export([start/2,
         stop/2,
         start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("of_protocol/include/ofp_v4.hrl").
-include("ofs_store_logger.hrl").
-include("linc_us4.hrl").

-define(SUPPORTED_BANDS, [drop,
                          dscp_remark,
                          experimenter]).
-define(SUPPORTED_FLAGS, [burst,
                          kbps,
                          pktps,
                          stats]).

-record(linc_meter_band, {
          type :: drop | dscp_remark | experimenter,
          rate :: integer(),
          burst_size :: integer(),
          prec_level :: integer() | undefined,
          experimenter :: integer() | undefined,
          pkt_count = 0 :: integer(),
          byte_count = 0 :: integer()
         }).

-record(linc_meter, {
          id :: integer(),
          ets :: integer(),
          rate_value :: kbps | pktps,
          burst :: boolean(),
          stats = true :: boolean(),
          bands = [] :: [#linc_meter_band{}],
          burst_history = [] :: [{erlang:timestamp(), integer()}],
          flow_count = 0 :: integer(),
          pkt_count = 0 :: integer(),
          byte_count = 0 :: integer(),
          install_ts = now() :: {integer(), integer(), integer()}
         }).

-define(BURST_TIME, timer:seconds(30)).

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

%% @doc Add, modify or delete a meter.
-spec modify(integer(), #ofp_meter_mod{}) ->
                    noreply | {reply, Reply :: ofp_message_body()}.
modify(_SwitchId, #ofp_meter_mod{command = add, meter_id = BadId})
  when is_atom(BadId); BadId > ?OFPM_MAX ->
    {reply, error_msg(invalid_meter)};
modify(SwitchId, #ofp_meter_mod{command = add, meter_id = Id} = MeterMod) ->
    case get_meter_pid(SwitchId, Id) of
        undefined ->
            case start(SwitchId, MeterMod) of
                {ok, _Pid} ->
                    noreply;
                {error, Code} ->
                    {reply, error_msg(Code)}
            end;
        _Pid ->
            {reply, error_msg(meter_exists)}
    end;
modify(_SwitchId, #ofp_meter_mod{command = modify, meter_id = BadId})
  when is_atom(BadId); BadId > ?OFPM_MAX ->
    {reply, error_msg(unknown_meter)};
modify(SwitchId, #ofp_meter_mod{command = modify, meter_id = Id} = MeterMod) ->
    case get_meter_pid(SwitchId, Id) of
        undefined ->
            {reply, error_msg(unknown_meter)};
        Pid ->
            case start(SwitchId, MeterMod) of
                {ok, _NewPid} ->
                    stop(SwitchId, Pid),
                    noreply;
                {error, Code} ->
                    {reply, error_msg(Code)}
            end
    end;
modify(SwitchId, #ofp_meter_mod{command = delete, meter_id = all} = MeterMod) ->
    TId = linc:lookup(SwitchId, linc_meter_ets),
    Entries = ets:tab2list(TId),
    [modify(SwitchId, MeterMod#ofp_meter_mod{meter_id = Id})
     || {Id, _Pid} <- Entries],
    noreply;
modify(SwitchId, #ofp_meter_mod{command = delete, meter_id = Id}) ->
    linc_us4_flow:delete_where_meter(SwitchId, Id),
    case get_meter_pid(SwitchId, Id) of
        undefined ->
            ok;
        Pid ->
            stop(SwitchId, Pid)
    end,
    noreply.

%% @doc Update flow entry count associated with a meter.
-spec update_flow_count(integer(), integer(), integer()) -> any().
update_flow_count(SwitchId, Id, Incr) ->
    case get_meter_pid(SwitchId, Id) of
        undefined ->
            ?DEBUG("Updating flow count of an non existing meter ~p", [Id]);
        Pid ->
            gen_server:cast(Pid, {update_flow_count, Incr})
    end.

%% @doc Get meter configuration.
-spec get_config(integer(), integer() | all) ->
                        Reply :: #ofp_meter_config_reply{}.
get_config(SwitchId, all) ->
    TId = linc:lookup(SwitchId, linc_meter_ets),
    Meters = [gen_server:call(Pid, get_state)
              || {_, Pid} <- lists:keysort(1, ets:tab2list(TId))],
    #ofp_meter_config_reply{body = [export_meter(Meter) || Meter <- Meters]};
get_config(SwitchId, Id) when is_integer(Id)  ->
    case get_meter_pid(SwitchId, Id) of
        undefined ->
            #ofp_meter_config_reply{body = []};
        Pid ->
            Meter = gen_server:call(Pid, get_state),
            #ofp_meter_config_reply{body = [export_meter(Meter)]}
    end.

%% @doc Get meter features.
-spec get_features() -> Reply :: #ofp_meter_features_reply{}.
get_features() ->
    #ofp_meter_features_reply{max_meter = ?MAX,
                              band_types = ?SUPPORTED_BANDS,
                              capabilities = ?SUPPORTED_FLAGS,
                              max_bands = ?MAX_BANDS,
                              max_color = 0}.

%% @doc Check if meter with a given id exists.
-spec is_valid(integer(), integer()) -> boolean().
is_valid(SwitchId, Id) ->
    case get_meter_pid(SwitchId, Id) of
        undefined ->
            false;
        _Else ->
            true
    end.

%%------------------------------------------------------------------------------
%% Internal API functions
%%------------------------------------------------------------------------------

start(SwitchId, MeterMod) ->
    Sup = linc:lookup(SwitchId, linc_meter_sup),
    supervisor:start_child(Sup, [MeterMod]).

stop(SwitchId, Pid) ->
    Sup = linc:lookup(SwitchId, linc_meter_sup),
    supervisor:terminate_child(Sup, Pid).

start_link(SwitchId, MeterMod) ->
    gen_server:start_link(?MODULE, [SwitchId, MeterMod], []).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

init([SwitchId, #ofp_meter_mod{meter_id = Id} = MeterMod]) ->
    process_flag(trap_exit, true),
    TId = linc:lookup(SwitchId, linc_meter_ets),
    case import_meter(MeterMod) of
        {ok, State} ->
            ets:insert(TId, {Id, self()}),
            {ok, State#linc_meter{ets = TId}};
        {error, Code} ->
            {stop, Code}
    end.

handle_call(get_state, _From, State) ->
    {reply, State, State}.

handle_cast({update_flow_count, Incr},
            #linc_meter{flow_count = Flows} = State) ->
    {noreply, State#linc_meter{flow_count = Flows + Incr}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #linc_meter{ets = TId,
                               id = Id}) ->
    ets:delete_object(TId, {Id, self()}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

import_meter(#ofp_meter_mod{meter_id = Id,
                            flags = Flags,
                            bands = Bands}) ->
    case import_flags(Flags) of
        {true, {Value, Burst, Stats}} ->
            NewBands = lists:map(fun import_band/1, Bands),
            case lists:any(fun(X) -> X == error end, NewBands) of
                false ->
                    NewMeter = #linc_meter{id = Id,
                                           rate_value = Value,
                                           burst = Burst,
                                           stats = Stats,
                                           bands = NewBands},
                    {ok, NewMeter};
                true ->
                    {error, bad_band}
            end;
        false ->
            {error, bad_flags}
    end.

-spec import_flags([atom()]) ->
                          {'true', {'pktps' | 'kbps', Burst::boolean(), Stats::boolean()}} |
                          'false'.
import_flags(Flags) ->
    SortFlags = lists:usort(Flags),
    case ordsets:is_subset(SortFlags, ?SUPPORTED_FLAGS)
        andalso not ordsets:is_subset([kbps, pktps], SortFlags) of
        true ->
            Value = case lists:member(pktps, Flags) of
                        true ->
                            pktps;
                        false ->
                            kbps
                    end,
            Burst = lists:member(burst, Flags),
            Stats = lists:member(stats, Flags),
            {true, {Value, Burst, Stats}};
        false ->
            false
    end.

import_band(#ofp_meter_band_drop{rate = Rate,
                                 burst_size = Burst}) ->
    case lists:member(drop, ?SUPPORTED_BANDS) of
        true ->
            #linc_meter_band{type = drop,
                             rate = Rate,
                             burst_size = Burst};
        false ->
            error
    end;
import_band(#ofp_meter_band_dscp_remark{rate = Rate,
                                        burst_size = Burst,
                                        prec_level = Prec}) ->
    case lists:member(dscp_remark, ?SUPPORTED_BANDS) of
        true ->
            #linc_meter_band{type = dscp_remark,
                             rate = Rate,
                             burst_size = Burst,
                             prec_level = Prec};
        false ->
            error
    end;
import_band(#ofp_meter_band_experimenter{rate = Rate,
                                         burst_size = Burst,
                                         experimenter = Exp}) ->
    case lists:member(experimenter, ?SUPPORTED_BANDS) of
        true ->
            #linc_meter_band{type = experimenter,
                             rate = Rate,
                             burst_size = Burst,
                             experimenter = Exp};
        false ->
            error
    end;
import_band(_) ->
    error.

export_stats(#linc_meter{id = Id,
                         stats = StatsEnabled,
                         bands = Bands,
                         install_ts = Then} = Meter) ->
    BandStats = [export_band_stats(StatsEnabled, Band) || Band <- Bands],
    MicroDuration = timer:now_diff(now(), Then),
    {Flows, Pkts, Bytes} = case StatsEnabled of
                               true ->
                                   #linc_meter{flow_count = F,
                                               pkt_count = P,
                                               byte_count = B} = Meter,
                                   {F, P, B};
                               false ->
                                   {-1, -1, -1}
                           end,
    #ofp_meter_stats{meter_id = Id,
                     flow_count = Flows,
                     packet_in_count = Pkts,
                     byte_in_count = Bytes,
                     duration_sec = MicroDuration div 1000000,
                     duration_nsec = (MicroDuration rem 1000) * 1000,
                     band_stats = BandStats}.

export_band_stats(true, #linc_meter_band{pkt_count = Pkts,
                                         byte_count = Bytes}) ->
    #ofp_meter_band_stats{packet_band_count = Pkts,
                          byte_band_count = Bytes};
export_band_stats(false, _Band) ->
    #ofp_meter_band_stats{packet_band_count = -1,
                          byte_band_count = -1}.

export_meter(#linc_meter{id = Id,
                         rate_value = Value,
                         burst = Burst,
                         stats = Stats,
                         bands = Bands}) ->
    NewBands = lists:map(fun export_band/1, Bands),
    #ofp_meter_config{flags = export_flags(Value, Burst, Stats),
                      meter_id = Id,
                      bands = NewBands}.

export_band(#linc_meter_band{type = Type,
                             rate = Rate,
                             burst_size = Burst,
                             prec_level = Prec,
                             experimenter = Exp}) ->
    case Type of
        drop ->
            #ofp_meter_band_drop{type = Type,
                                 rate = Rate,
                                 burst_size = Burst};
        dscp_remark ->
            #ofp_meter_band_dscp_remark{type = Type,
                                        rate = Rate,
                                        burst_size = Burst,
                                        prec_level = Prec};
        experimenter ->
            #ofp_meter_band_experimenter{type = Type,
                                         rate = Rate,
                                         burst_size = Burst,
                                         experimenter = Exp}
    end.

export_flags(Value, true, true) ->
    [Value, burst, stats];
export_flags(Value, true, false) ->
    [Value, burst];
export_flags(Value, false, true) ->
    [Value, stats];
export_flags(Value, false, false) ->
    [Value].

get_meter_pid(SwitchId, Id) ->
    TId = linc:lookup(SwitchId, linc_meter_ets),
    case ets:lookup(TId, Id) of
        [] ->
            undefined;
        [{Id, Pid}] ->
            Pid
    end.

error_msg(Code) ->
    #ofp_error_msg{type = meter_mod_failed,
                   code = Code}.
