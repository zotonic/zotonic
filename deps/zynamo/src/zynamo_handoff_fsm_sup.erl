%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2011 Marc Worrell
%% @doc zynamo supervisor for handoffs to other nodes, one process per node.
%%      Every handoff process matches the services on the two nodes and checks locally which
%%      service has handoffs for the other node.

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

-module(zynamo_handoff_fsm_sup).
-behaviour(supervisor).

-export([start_fsm/1]).
-export([start_link/0, init/1]).

start_fsm(Node) ->
    case node() of
        Node ->
            {error, already_present};
        _ ->
            Spec = {
                Node,
                {zynamo_handoff_fsm, start_link, [Node]},
                permanent, 2000, worker, [zynamo_handoff_fsm]
            },
            case supervisor:start_child(?MODULE, Spec) of
                {error, {already_started, Pid}} = Result ->
                    gen_fsm:send_all_state_event(Pid, nodeup),
                    Result;
                {ok, Pid} = Result ->
                    gen_fsm:send_all_state_event(Pid, nodeup),
                    Result;
                Other -> Other
            end
    end.


%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
        {ok, Pid} ->
            zynamo_handoff_watcher:start(),
            {ok, Pid};
        Other -> 
            Other
    end.

init([]) ->
    {ok, {{one_for_one, 100, 10}, []}}.
