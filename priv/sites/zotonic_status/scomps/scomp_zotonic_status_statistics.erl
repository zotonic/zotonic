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

-module(scomp_zotonic_status_statistics).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include_lib("include/zotonic.hrl").
-include_lib("zynamo.hrl").

-export([vary/2, render/3, event_process/1]).

vary(_Params, _Context) -> nocache.

render(_Params, _Vars, Context) ->
    z_session_page:spawn_link(?MODULE, event_process, [Context], Context),
    %%spawn_link(fun() -> event_process(Context) end),
    {ok, statistics_html(Context)}.


event_process(Context) ->
    timer:sleep(1000),
    Script = z_template:render("_statistics_script.tpl", tplvars(), Context),
    Context1 = z_script:add_script(Script, Context),
    z_session_page:add_script(Context1),

    event_process(Context).


statistics_html(Context) ->
    z_template:render("_statistics.tpl", tplvars(), Context).


tplvars() ->
    Source = lists:sort(zynamo_request:get(zynamo, stats, {'zotonic', '_', '_'}, [{n,all}])),
    Nodes = [N || {N, _, _} <- Source],
    {_,_,S0} = hd(Source),
    Stats = [{Node, S} || {Node, _, S} <- Source],
    Types = [K || {K, _} <- S0],
    [{stats, Stats},
     {nodes, Nodes},
     {types, Types}].
    
