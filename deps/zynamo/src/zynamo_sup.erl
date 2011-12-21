%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2011 Marc Worrell
%% @doc zynamo supervisor

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

%% The hash ring of a zotonic system consists of nodes that are responsible for a part of the ring.
%% Keys are hashed to buckets and the buckets have a primary node that is responsible for storing
%% values hashed to the bucket.
%% 
%% When a node is added it will claim a range from the ring.  This range is calculated depending on 
%% the number of nodes in the ring.  All nodes have an equal weight.

-module(zynamo_sup).
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(supervisor).

-export([
    start_link/0,
    init/1
]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Processes = [
        {zynamo_random, {zynamo_random, start_link, []},
            permanent, 2000, worker, [zynamo_random]},

        {zynamo_event, {zynamo_event, start_link, []},
            permanent, 2000, worker, [zynamo_event]},

        {zynamo_request_fsm_sup, {zynamo_request_fsm_sup, start_link, []},
            permanent, 2000, supervisor, [zynamo_request_fsm_sup]},

        {zynamo_manager, {zynamo_manager, start_link, []},
            permanent, 2000, worker, [zynamo_manager]},

        {zynamo_gossip, {zynamo_gossip, start_link, []},
            permanent, 2000, worker, [zynamo_gossip]},
    
        % Standard zynamo services
        {zynamo_kv, {zynamo_kv, start_link, []},
            permanent, 2000, worker, [zynamo_kv]}
    ],
    {ok, {{one_for_one, 1000, 10}, Processes}}.

