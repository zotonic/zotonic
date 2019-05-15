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
-behaviour(zotonic_scomp).

-export([vary/2, render/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

% Menu structure is a like:
%
% <ul id="navigation" class="nav">
% 	<li>
% 		<a href="" class="home-page active">home</a>
% 	</li>
% 	<li>
% 		<a href="" class="about-page">about</a>
% 	</li>
% 	<li>
% 		<a href="" class="contact-page">contact</a>
% 	</li>
% </ul>


vary(_Params, _Context) -> default.

% Params:
% menu_id
% is_superfish
% maxdepth
% class (used in template)
% id_prefix (used in template)
render(Params, _Vars, Context) ->
    MenuId = m_rsc:rid(get_menu_id(Params, Context), Context),
    Template = case z_convert:to_bool(proplists:get_value(is_superfish, Params, false)) of
                    true -> proplists:get_value(template, Params, "_menu_superfish.tpl");
                    false -> proplists:get_value(template, Params, "_menu.tpl")
               end,
    IdPrefix = proplists:get_value(id_prefix, Params, ""),
    Class = proplists:get_value(class, Params, "nav navbar-nav"),
    MaxDepth = proplists:get_value(maxdepth, Params, 999),
    Menu = mod_menu:get_menu(MenuId, Context),
    Vars = [
        {menu, mod_menu:menu_flat(Menu, MaxDepth, Context)},
        {menu_id, MenuId},
        {id_prefix, IdPrefix},
        {class, Class}
        | Params
    ],
    {ok, z_template:render(Template, Vars, Context)}.

get_menu_id(Params, Context) ->
    case proplists:get_value(menu_id, Params) of
        undefined -> filter_menu_rsc:menu_rsc(proplists:get_value(id, Params), Context);
        MenuId -> MenuId
    end.
