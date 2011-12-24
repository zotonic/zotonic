%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2011-12-24

%% @doc Nodes status overview

%% Copyright 2011 Arjan Scherpenisse
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

-module(scomp_zotonic_status_nodes_status).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include_lib("include/zotonic.hrl").
-include_lib("zynamo.hrl").

-export([vary/2, render/3, ring_status_html/1]).

vary(_Params, _Context) -> nocache.

render(_Params, _Vars, Context) ->
    spawn_link(fun() -> event_process(Context) end),
    {ok, ["<table id=\"nodes\">", ring_status_html(Context), "</table>"]}.


event_process(Context) ->
    zynamo_event:add_handler(zotonic_status_zynamo_page_events, Context),
    erlang:monitor(process, Context#context.page_pid),
    receive
        {'DOWN', _, process, _, _} ->
            zynamo_event:delete_handler(zotonic_status_zynamo_page_events, Context)
    end.


service_as_string({Site,Service}) ->
    io_lib:format("~p:~p", [Site, Service]).

ring_status_html(Context) ->
    {ok, Ring} = zynamo_manager:get_ring(),
    Nodes = zynamo_ring:nodes(Ring),
    Vars = [{nodes, Nodes},
            {node_up, [{Node, zynamo_ring:is_node_up(Node, Ring)} || Node <- Nodes]},
            {services, lists:map(fun service_as_string/1, zynamo_ring:list_known_services(Ring))},
            {node_services, [{N, lists:map(fun service_as_string/1, zynamo_ring:list_node_services(N, Ring))} || N <- Nodes]},
            {ring, []}],
    z_template:render("_nodes.tpl", Vars, Context).
