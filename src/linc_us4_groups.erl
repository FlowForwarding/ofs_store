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
%% @doc Module for handling all group related tasks.
-module(linc_us4_groups).

%% API
-export([initialize/1,
         terminate/1,
         modify/2,
         get_desc/2,
         get_features/1,
         is_valid/2]).

-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("of_protocol/include/ofp_v4.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("ofs_store_logger.hrl").
-include("linc_us4.hrl").

-type linc_bucket_id() :: {integer(), binary()}.

%% @doc Bucket wrapper adding unique_id field to the original OFP bucket
-record(linc_bucket, {
          bucket    :: ofp_bucket(),
          unique_id :: linc_bucket_id()
         }).

%% @doc Group object
-record(linc_group, {
          id               :: ofp_group_id(),
          type    = all    :: ofp_group_type(),
          total_weight = 0 :: integer(),
          buckets = []     :: [#linc_bucket{}],
          refers_to_groups = [] :: ordsets:ordset(integer())
         }).

-type linc_stats_key_type() :: {group, integer(), atom()}
                             | {bucket, {integer(), binary()}, atom()}
                             | {group_start_time, integer()}.

%% @doc Stats item record for storing stats in ETS
-record(linc_group_stats, {
          key   :: linc_stats_key_type(),
          value :: integer()
         }).

%%%==============================================================
%%% API implementation
%%%==============================================================

%% @doc Module startup
initialize(SwitchId) ->
    GroupTable = ets:new(group_table, [public,
                                       {keypos, #linc_group.id},
                                       {read_concurrency, true},
                                       {write_concurrency, true}]),
    linc:register(SwitchId, group_table, GroupTable),
    %% Stats are stored in form of #linc_group_stats{key, value}, key is a tuple
    %% {group, GroupId, packet_count}
    %% {group, GroupId, byte_count}
    %% {bucket, GroupId, BucketId, packet_count}
    %% {bucket, GroupId, BucketId, byte_count}
    %% and value is 32 or 64 bit unsigned counter, which wraps when reaching max
    %% value for 32 or 64 bit.
    GroupStats = ets:new(group_stats, [public,
                                       {keypos, #linc_group_stats.key},
                                       {read_concurrency, true},
                                       {write_concurrency, true}]),
    linc:register(SwitchId, group_stats, GroupStats),

    %% Time for groups is stored in the same way as stats, but holds unix
    %% microtime instead of counter
    ok.

%%--------------------------------------------------------------------
%% @doc Module shutdown
terminate(SwitchId) ->
    ets:delete(linc:lookup(SwitchId, group_table)),
    ets:delete(linc:lookup(SwitchId, group_stats)),
    ok.

%%--------------------------------------------------------------------
-spec modify(integer(), #ofp_group_mod{}) -> ok |
                                             {error, Type :: atom(), Code :: atom()}.

%%------ group_mod ADD GROUP
modify(_SwitchId, #ofp_group_mod{ command = add, group_id = BadGroupId })
  when is_atom(BadGroupId); BadGroupId > ?OFPG_MAX ->
    %% Can't add a group with a reserved group id (represented as
    %% atoms here), or with a group id greater than the largest
    %% allowed.
    {error, #ofp_error_msg{type = group_mod_failed, code = invalid_group}};
modify(SwitchId, #ofp_group_mod{ command = add,
                                 group_id = Id,
                                 type = Type,
                                 buckets = Buckets }) ->
    %% Add new entry to the group table, if entry with given group id is already
    %% present, then return error.
    OFSBuckets = wrap_buckets_into_linc_buckets(Id, Buckets),
    RefersToGroups = calculate_refers_to_groups(Buckets, ordsets:new()),
    Entry = #linc_group{
      id = Id,
      type = Type,
      buckets = OFSBuckets,
      total_weight = calculate_total_weight(Buckets),
      refers_to_groups = RefersToGroups
     },
    case ets:insert_new(linc:lookup(SwitchId, group_table), Entry) of
        true ->
            ok;
        false ->
            {error, #ofp_error_msg{type = group_mod_failed,
                                   code = group_exists}}
    end;

%%------ group_mod MODIFY GROUP
modify(SwitchId, #ofp_group_mod{ command = modify,
                                 group_id = Id,
                                 type = Type,
                                 buckets = Buckets }) ->
    %% Modify existing entry in the group table, if entry with given group id
    %% is not in the table, then return error.
    Entry = #linc_group{
      id = Id,
      type = Type,
      buckets = wrap_buckets_into_linc_buckets(Id, Buckets),
      total_weight = calculate_total_weight(Buckets)
     },
    %% Reset group counters
    %% Delete stats for buckets
    case group_get(SwitchId, Id) of
        not_found ->
            {error, #ofp_error_msg{type = group_mod_failed,
                                   code = unknown_group}};
        _OldGroup ->
            ets:insert(linc:lookup(SwitchId, group_table), Entry),
            ok
    end;

%%------ group_mod DELETE GROUP
modify(SwitchId, #ofp_group_mod{ command = delete,
                                 group_id = Id }) ->
    %% Deletes existing entry in the group table, if entry with given group id
    %% is not in the table, no error is recorded. Flows containing given
    %% group id are removed along with it.
    %% If one wishes to effectively delete a group yet leave in flow entries
    %% using it, that group can be cleared by sending a modify with no buckets
    %% specified.
    case Id of
        all ->
            %% Reset group counters
            ets:delete_all_objects(linc:lookup(SwitchId, group_table)),
            ets:delete_all_objects(linc:lookup(SwitchId, group_stats));
        any ->
            %% TODO: Should we support this case at all?
            ok;
        Id ->
            group_delete(SwitchId, Id)
    end,
    ok.

%%--------------------------------------------------------------------
-spec get_desc(integer(), #ofp_group_desc_request{}) ->
                      #ofp_group_desc_reply{}.
get_desc(SwitchId, #ofp_group_desc_request{}) ->
    #ofp_group_desc_reply{
       body = group_enum_groups(SwitchId)
      }.

%%--------------------------------------------------------------------
-spec get_features(#ofp_group_features_request{}) ->
                          #ofp_group_features_reply{}.
get_features(#ofp_group_features_request{ flags = _F }) ->
    #ofp_group_features_reply{
       types = [all, select, indirect, ff],
       capabilities = [select_weight, chaining], %select_liveness, chaining_checks
       max_groups = {?MAX, ?MAX, ?MAX, ?MAX},
       actions = {?SUPPORTED_WRITE_ACTIONS, ?SUPPORTED_WRITE_ACTIONS,
                  ?SUPPORTED_WRITE_ACTIONS, ?SUPPORTED_WRITE_ACTIONS}
      }.

%%--------------------------------------------------------------------
%% @doc Test if a group exists.
-spec is_valid(integer(), integer()) -> boolean().
is_valid(SwitchId, GroupId) ->
    ets:member(linc:lookup(SwitchId, group_table), GroupId).

%%--------------------------------------------------------------------
%% @internal
%% @doc Called from modify() to wrap incoming buckets into #linc_bucket{}, with
%% counters added, which is wrapped into #linc_bucket{} with unique id added
-spec wrap_buckets_into_linc_buckets(GroupId :: integer(),
                                     [#ofp_bucket{}]) -> [#linc_bucket{}].

wrap_buckets_into_linc_buckets(GroupId, Buckets) ->
    lists:map(fun(B) ->
                      #linc_bucket{
                         bucket = B,
                         unique_id = {GroupId, create_unique_id_for_bucket(B)}
                        }
              end, Buckets).

%%--------------------------------------------------------------------
%% @internal
%% @doc Creates an unique ID based on contents of the bucket. If contents changes,
%% the unique ID will be recalculated and changes as well.
-spec create_unique_id_for_bucket(#ofp_bucket{}) -> term().

create_unique_id_for_bucket(B) ->
    EncodedBucket = term_to_binary(B),

    %% Add a timestamp in case of identical buckets + a random
    {MegaS, S, MicroS} = os:timestamp(),
    Random = random:uniform(16#7FFFFFFF),

    %% NOTE: this may create key collision if random and microseconds match
    Image = <<EncodedBucket/binary, MegaS:32, S:32, MicroS:32, Random:32>>,
    %% Create a hash
    crypto:sha(Image).

%%--------------------------------------------------------------------
%% @internal
%% @doc Reads group from ETS or returns not_found
-spec group_get(integer(), integer()) -> not_found | #linc_group{}.
group_get(SwitchId, GroupId) ->
    case ets:lookup(linc:lookup(SwitchId, group_table), GroupId) of
        [] ->
            not_found;
        [Group] ->
            Group
    end.


%%--------------------------------------------------------------------
%% @internal
%% @doc Iterates over all keys of groups table and creates list of
%% #ofp_group_desc_stats{} standard records for group stats response
-spec group_enum_groups(integer()) -> [#ofp_group_desc_stats{}].
group_enum_groups(SwitchId) ->
    GroupTable = linc:lookup(SwitchId, group_table),
    group_enum_groups_2(GroupTable, ets:first(GroupTable), []).

%% @internal
%% @hidden
%% @doc (does the iteration job for group_enum_groups/0)
group_enum_groups_2(_GroupTable, '$end_of_table', Accum) ->
    lists:reverse(Accum);
group_enum_groups_2(GroupTable, K, Accum) ->
    %% record must always exist, as we are iterating over table keys
    [Group] = ets:lookup(GroupTable, K),
    %% unwrap wrapped buckets
    Buckets = [B#linc_bucket.bucket || B <- Group#linc_group.buckets],
    %% create standard structure
    GroupDesc = #ofp_group_desc_stats{
                   group_id = Group#linc_group.id,
                   type = Group#linc_group.type,
                   buckets = Buckets
                  },
    group_enum_groups_2(GroupTable, ets:next(GroupTable, K),
                        [GroupDesc | Accum]).

%%--------------------------------------------------------------------
%% @internal
%% @doc Deletes a group by Id, also deletes all groups and flows referring
%% to this group
group_delete(SwitchId, Id) ->
    group_delete_2(SwitchId, Id, ordsets:new()).

%% @internal
%% @doc Does recursive deletion taking into account groups already processed to
%% avoid infinite loops. Returns false o
-spec group_delete_2(SwitchId :: integer(), Id :: integer(),
                     ProcessedGroups :: ordsets:ordset()) ->
                            ordsets:ordset().
group_delete_2(SwitchId, Id, ProcessedGroups) ->
    case ordsets:is_element(Id, ProcessedGroups) of
        true ->
            ProcessedGroups;

        false ->
            GroupTable = linc:lookup(SwitchId, group_table),
            FirstGroup = ets:first(GroupTable),
            ReferringGroups = group_find_groups_that_refer_to(SwitchId,
                                                              Id,
                                                              FirstGroup,
                                                              ordsets:new()),

            ets:delete(GroupTable, Id),

            %% Remove flows containing given group along with it
            linc_us4_flow:delete_where_group(SwitchId, Id),

            PG2 = ordsets:add_element(Id, ProcessedGroups),

            %% Remove referring groups
            RFunc = fun(X, Accum) -> group_delete_2(SwitchId, X, Accum) end,
            lists:foldl(RFunc, PG2, ReferringGroups)
    end.

%%--------------------------------------------------------------------
%% @internal
%% @doc Iterates over groups table, filters out groups which refer to the
%% group id 'Id' using cached field in #linc_group{} record
group_find_groups_that_refer_to(_SwitchId, _Id, '$end_of_table', OrdSet) ->
    OrdSet;
group_find_groups_that_refer_to(SwitchId, Id, EtsKey, OrdSet) ->
    %% this should never crash, as we are iterating over existing keys
    GroupTable = linc:lookup(SwitchId, group_table),
    [G] = ets:lookup(GroupTable, EtsKey),

    NextKey = ets:next(GroupTable, EtsKey),
    case ordsets:is_element(Id, G#linc_group.refers_to_groups) of
        false ->
            group_find_groups_that_refer_to(SwitchId, Id, NextKey, OrdSet);
        true ->
            OrdSet2 = ordsets:add_element(EtsKey, OrdSet),
            group_find_groups_that_refer_to(SwitchId, Id, NextKey, OrdSet2)
    end.

%%--------------------------------------------------------------------
%% @internal
%% @doc Iterates over buckets, calculates sum of all weights in a bucket
-spec calculate_total_weight(Buckets :: [ofp_bucket()]) -> integer().
calculate_total_weight(Buckets) ->
    lists:foldl(fun(B, Sum) ->
                        case B#ofp_bucket.weight of
                            W when is_integer(W) -> W;
                            _ -> 1
                        end + Sum
                end, 0, Buckets).

%%--------------------------------------------------------------------
%% @internal
%% @doc Iterates over all buckets and actions, and builds a set of group
%% references, to have ordset of groupids this bucket list refers to
-spec calculate_refers_to_groups([#ofp_bucket{}],
                                 ordsets:ordset(integer())) ->
                                        ordsets:ordset(integer()).

calculate_refers_to_groups([], Set) ->
    Set;
calculate_refers_to_groups([B|Buckets], Set) ->
    Set2 = calculate_refers_to_groups_2(B#ofp_bucket.actions, Set),
    calculate_refers_to_groups(Buckets, Set2).

%% @internal
%% @doc Iterates over actions in a bucket, filtering out #ofp_action_group
%% actions, and adding GroupId in them to Set
calculate_refers_to_groups_2([], Set) -> Set;

calculate_refers_to_groups_2([#ofp_action_group{
                                 group_id = GroupId
                                }|Actions], Set) ->
    Set2 = ordsets:add_element(GroupId, Set),
    calculate_refers_to_groups_2(Actions, Set2);

calculate_refers_to_groups_2([_|Actions], Set) ->
    calculate_refers_to_groups_2(Actions, Set).
