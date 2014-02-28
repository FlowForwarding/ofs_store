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

-define(PORT_SPEED, 5000). %% Port speed in kbps = 5Mbps
-define(FEATURES, ['100mb_fd', copper, autoneg]).

-record(linc_port, {
          port_no :: ofp_port_no(),
          pid     :: pid()
         }).

%% LINC swich port configuration stored in sys.config
-type linc_port_config() :: tuple(interface, string())
                          | tuple(ip, string())
                          | tuple(config, tuple())
                          | tuple(features, tuple())
                          | tuple(queues, tuple()).

-type linc_port_type() :: physical | logical | reserved.
-type linc_queues_state() :: enabled | disabled.

-record(state, {
          resource_id        :: string(),
          %% Common state of tap and eth interfaces
          interface          :: string(),
          type = physical    :: linc_port_type(),
          switch_id = 0      :: integer(),
          port = #ofp_port{} :: ofp_port(),
          %% State of tap interface
          erlang_port        :: port(),
          port_ref           :: pid(),
          %% State of eth interface
          socket             :: integer(),
          ifindex            :: integer(),
          epcap_pid          :: pid(),
          %% Queue subsystem state
          queues = disabled  :: linc_queues_state()
         }).
