%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013 Marc Worrell
%% @doc Filter a list of menu items on visibility. Does not filter sub-menus.

%% Copyright 2013 Marc Worrell
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

-module(filter_menu_is_visible).
-export([menu_is_visible/2]).

menu_is_visible(Items, Context) when is_list(Items) ->
	lists:filter(fun(Item) -> is_visible(Item, Context) end, Items);
menu_is_visible(_, _Context) ->
	[].

is_visible(undefined, _Context) ->
	false;
is_visible({RscId, _Items}, Context) ->
	z_acl:rsc_visible(RscId, Context);
is_visible(RscId, Context) ->
	z_acl:rsc_visible(RscId, Context).
