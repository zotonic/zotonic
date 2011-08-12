%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-07-15
%% @doc Add the tabs UI to an element

%% Copyright 2009 Marc Worrell
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

-module(scomp_base_tabs).
-behaviour(gen_scomp).

-export([vary/2, render/3]).

-include("zotonic.hrl").

vary(_Params, _Context) -> nocache.

render(Params, _Vars, Context) ->
    Id = proplists:get_value(id, Params),
    Script = case proplists:get_value(hash, Params) of
                true ->
                    [ "$('#", z_convert:to_list(Id), "').tabs({ select: function(event, ui) { window.location.hash = ui.tab.hash; }});" ];
                _ ->
                    [ "$('#", z_convert:to_list(Id), "').tabs();" ]
             end,
    {ok, z_render:wire({script, [{script, Script}]}, Context)}.

