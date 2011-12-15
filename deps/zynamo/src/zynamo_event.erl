%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2011 Marc Worrell
%% @doc Event stream for the ring

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

-module(zynamo_event).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    start_link/0,
    add_handler/2,
    delete_handler/2,

    up/0,
    changed/0,
    down/0,
    nodedown/1,
    nodeup/1,

    servicechanged/1,
    servicedown/2,
    serviceup/2
]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_event:start_link({local, ?SERVER}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).


up() ->
    gen_event:notify(?SERVER, up),
    nodeup(node()).

changed() ->
    gen_event:notify(?SERVER, changed).

down() ->
    nodedown(node()),
    gen_event:notify(?SERVER, down).

nodedown(Node) ->
    gen_event:notify(?SERVER, {nodedown, Node}).

nodeup(Node) ->
    gen_event:notify(?SERVER, {nodeup, Node}).

servicechanged(Services) ->
    gen_event:notify(?SERVER, {servicechanged, Services}).
    
servicedown(Site, Service) ->
    gen_event:notify(?SERVER, {servicedown, Site, Service}).

serviceup(Site, Service) ->
    gen_event:notify(?SERVER, {serviceup, Site, Service}).

