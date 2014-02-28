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

-module(ofs_store).

-export([request/1,
         clear/1,
         dump/1]).

-include_lib("of_protocol/include/of_protocol.hrl").
-include("ofs_store.hrl").

request(Request = #ofs_store_request{}) ->
    ofs_store_logic:request(Request).

% debugging/testing
clear(Table) ->
    ofs_store_db:clear(Table).

dump(Table) ->
    ofs_store_db:dump(Table).
