%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse
%% @doc Get a "flat" of menu parents

%% Copyright 2010 Marc Worrell
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

-module(filter_menu_flat).
-export([menu_flat/2]).


menu_flat(Menu, Context) ->
    mod_menu:menu_flat(mod_menu:remove_invisible(Menu, Context), Context).
