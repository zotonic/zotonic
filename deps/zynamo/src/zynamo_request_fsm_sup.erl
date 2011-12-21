%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2011 Marc Worrell
%% @doc zynamo supervisor for request FSMs

%% Copyright 2010-2011 Marc Worrell
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

-module(zynamo_request_fsm_sup).
-behaviour(supervisor).

-export([start_fsm/1]).
-export([start_link/0, init/1]).

start_fsm(Args) ->
    supervisor:start_child(?MODULE, Args).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Spec = {
        undefined,
        {zynamo_request_fsm, start_link, []},
        temporary, 2000, worker, [zynamo_request_fsm]
    },
    {ok, {{simple_one_for_one, 10, 10}, [Spec]}}.
