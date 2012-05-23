%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%% @doc Get the sub tree of an id in a menu (if any)

%% Copyright 2012 Marc Worrell
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

-module(filter_menu_subtree).
-export([
    menu_subtree/2,
    menu_subtree/3,
    menu_subtree/4
]).

menu_subtree(BelowId, Context) ->
    menu_subtree(BelowId, main_menu, false, Context).

menu_subtree(BelowId, Menu, Context) ->
    menu_subtree(BelowId, Menu, false, Context).

menu_subtree(undefined, _, _, _) -> [];
menu_subtree(_, undefined, _, _) -> [];
menu_subtree(_, <<>>, _, _) -> [];
menu_subtree(_, [], _, _) -> [];
menu_subtree(BelowId, Menu, AddSiblings, Context) when is_integer(Menu) ->
    menu_subtree(BelowId, m_rsc:p(Menu, menu, Context), AddSiblings, Context);
menu_subtree(BelowId, [{MId,L}|_] = Menu, AddSiblings, Context) when is_integer(MId), is_list(L) ->
    mod_menu:menu_subtree(mod_menu:remove_invisible(Menu, Context), BelowId, z_convert:to_bool(AddSiblings), Context);
menu_subtree(BelowId, Menu, AddSiblings, Context) ->
    menu_subtree(BelowId, m_rsc:rid(Menu, Context), AddSiblings, Context).
