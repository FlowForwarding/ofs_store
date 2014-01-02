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
%% @doc Callback module for OpenFlow Logical Switch application.
-module(ofs_store).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

-export([create/1,
         delete/1,
         register/3,
         lookup/2]).

-include("ofs_store_logger.hrl").

%%------------------------------------------------------------------------------
%% Application callbacks
%%------------------------------------------------------------------------------

%% @doc Starts the application.
-spec start(any(), any()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    ofs_store_sup:start_link().

%% @doc Stops the application.
-spec stop(any()) -> ok.
stop(_State) ->
    ok.

%%------------------------------------------------------------------------------
%% Common LINC helper functions
%%------------------------------------------------------------------------------

% XXX need to flatten these tables so there is one table for all switches
% XXX store in mnesia

-spec create(integer()) -> ets:tid().
create(SwitchId) ->
    ets:new(name(SwitchId), [named_table, public,
                             {read_concurrency, true}]).

-spec delete(integer()) -> true.
delete(SwitchId) ->
    true = ets:delete(name(SwitchId)).

-spec register(integer(), atom(), pid() | ets:tid()) -> true.
register(SwitchId, Name, Pid) ->
    true = ets:insert(name(SwitchId), {Name, Pid}).

-spec lookup(integer(), atom()) -> term().
lookup(SwitchId, Name) ->
    case ets:lookup(name(SwitchId), Name) of
        [{Name, Pid}] ->
            Pid;
        [] ->
            undefined
    end.
