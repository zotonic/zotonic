%% @author Atilla Erdodi <atilla@maximonster.com>
%% @copyright 2010 Maximonster Interactive Things
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

-module(webmachine_id).

-export([generate/0]).

generate() ->
    NodeId = 
        case application:get_env(webmachine, node_id) of
            undefined -> 9;
            {ok, NodeId_} -> NodeId_
        end,
    {MegaSec, Sec, MicroSec} = now(),
    Id = ((NodeId * 1000000 + MegaSec) * 1000000 + Sec) * 1000000 + MicroSec,
    Id.
