%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2012 Marc Worrell
%% @doc Render the menu.  Add classes to highlight the current item.  The menu is always build as seen by the anonymous user.

%% Copyright 2009-2012 Marc Worrell
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

-module(scomp_menu_menu).
-behaviour(gen_scomp).

-export([vary/2, render/3]).

-include("zotonic.hrl").

% Menu structure is a like:
%
% <ul id="navigation" class="at-menu">
% 	<li id="nav-item-1" class="first">
% 		<a href="" class="home-page current">home</a>
% 	</li>
% 	<li id="nav-item-2">
% 		<a href="" class="about-page">about</a>
% 	</li>
% 	<li id="nav-item-3" class="last">
% 		<a href="" class="contact-page">contact</a>
% 	</li>
% </ul>


vary(_Params, _Context) -> default.

render(Params, _Vars, Context) ->
    MenuId = m_rsc:rid(proplists:get_value(menu_id, Params, main_menu), Context),
    Template = case proplists:get_value(is_superfish, Params, false) of
                    true -> proplists:get_value(template, Params, "_menu_superfish.tpl");
                    false -> proplists:get_value(template, Params, "_menu.tpl")
               end,
    MaxDepth = proplists:get_value(maxdepth, Params, 999),
    Menu = mod_menu:get_menu(MenuId, Context),
    Vars = [
        {menu, mod_menu:menu_flat(Menu, MaxDepth, Context)},
        {menu_id, MenuId}
        | Params
    ],
    {ok, z_template:render(Template, Vars, Context)}.
