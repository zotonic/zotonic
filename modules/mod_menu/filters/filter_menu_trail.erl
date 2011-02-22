%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse
%% @doc Get a "trail" of menu parents

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

-module(filter_menu_trail).
-export([menu_trail/2, menu_trail/3, test/0]).

menu_trail(undefined, _Context) ->
    undefined;
menu_trail(Id, Context) ->
    menu_trail(Id, main_menu, Context).

menu_trail(Id, MenuId, Context) ->
    Current = z_convert:to_integer(Id),
    Menu = mod_menu:get_menu(MenuId, Context),
    trail(Current, Menu).


trail(_Id, []) ->
    []; %% not found
trail(Id, [{Id, _}|_]) ->
    [Id]; %% found
trail(Id, [{MenuId, Children}|Rest]) ->
    case trail(Id, Children) of
        [] ->
            %% not found
            trail(Id, Rest);
        Path ->
            %% Found
            [MenuId|Path]
    end.



test() ->
    [1] = trail(1, [{1, []}]),
    [] = trail(1, [{33, []}]),
    [33,66] = trail(66, [{4, []}, {33, [{3, []}, {66, []}]}]).
