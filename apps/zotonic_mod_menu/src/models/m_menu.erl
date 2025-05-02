%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2024 Marc Worrell
%% @doc Menu support routines.
%% @end

%% Copyright 2024 Marc Worrell
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

-module(m_menu).

-export([
    m_get/3
]).

m_get([ <<"is_menu_item_delete_safe">>, RscId | Rest ], _Msg, Context) ->
    % Check if a rsc is safe to delete from the menu.
    case m_rsc:rid(RscId, Context) of
        undefined ->
            {ok, {true, Rest}};
        Id ->
            case z_convert:to_bool(m_rsc:p_no_acl(Id, <<"is_dependent">>, Context)) of
                true ->
                    case m_edge:subjects(Id, Context) of
                        [] ->
                            {ok, {false, Rest}};
                        [_] ->
                            {ok, {false, Rest}};
                        [_|_] ->
                            {ok, {true, Rest}}
                    end;
                false ->
                    {ok, {true, Rest}}
            end
    end.
