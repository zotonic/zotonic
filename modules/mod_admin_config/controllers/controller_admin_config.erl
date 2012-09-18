%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-08-07
%% @doc Overview of all config settings with string values.

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

-module(controller_admin_config).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_authorized/2
]).

-include_lib("html_controller.hrl").

is_authorized(ReqData, Context) ->
    z_acl:wm_is_authorized(use, mod_admin_config, ReqData, Context).


html(Context) ->
    All = m_config:all(Context),
    AllWithValue = lists:sort(lists:map(fun only_value_config/1, All)),
    Vars = [
        {page_admin_config, true},
        {config, AllWithValue}
    ],
	Html = z_template:render("admin_config.tpl", Vars, Context),
	z_context:output(Html, Context).


%% @doc Check if the config does not have a non-string setting.  We only edit string values.
%% @spec only_value_config({module, [{key,Value}]}) -> bool()
only_value_config({Module, Keys}) ->
    {Module, lists:filter(fun is_value_config_key/1, Keys)}.

    is_value_config_key({_Key, Props}) ->
        is_value_config_props(Props).

    is_value_config_props([]) ->
        true;
    is_value_config_props([{Prop,_}|Rest]) when Prop == created; Prop == modified; Prop == value; Prop == id; Prop == module; Prop == key ->
        is_value_config_props(Rest);
    is_value_config_props([{props,<<>>}|Rest]) ->
        is_value_config_props(Rest);
    is_value_config_props([{props,undefined}|Rest]) ->
        is_value_config_props(Rest);
    is_value_config_props(_X) ->
        false.

