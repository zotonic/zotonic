%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2023 Marc Worrell
%% @doc Overview of all config settings with string values.
%% @end

%% Copyright 2009-2023 Marc Worrell
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
    service_available/1,
    is_authorized/1,
    process/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

service_available(Context) ->
    Context1 = z_context:set_noindex_header(Context),
    Context2 = z_context:set_nocache_headers(Context1),
    {true, Context2}.

is_authorized(Context) ->
    z_controller_helper:is_authorized([ {use, z_context:get(acl_module, Context, mod_admin_config)} ], Context).

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    All = m_config:all(Context),
    AllWithValue = lists:sort(lists:map(fun only_value_config/1, All)),
    Vars = [
        {page_admin_config, true},
        {config, AllWithValue}
    ],
	Html = z_template:render("admin_config.tpl", Vars, Context),
	z_context:output(Html, Context).


%% @doc Check if the config does not have a non-string setting.  We only edit string values.
only_value_config({Module, Keys}) ->
    {Module, lists:filter(fun is_value_config_key/1, Keys)}.

is_value_config_key({_Key, Props}) ->
    is_value_config_props(Props).

is_value_config_props([]) ->
    true;
is_value_config_props([{Prop,_}|Rest]) when
        Prop =:= created;
        Prop =:= modified;
        Prop =:= is_secret;
        Prop =:= value;
        Prop =:= id;
        Prop =:= module;
        Prop =:= key ->
    is_value_config_props(Rest);
is_value_config_props([{props,<<>>}|Rest]) ->
    is_value_config_props(Rest);
is_value_config_props([{props,undefined}|Rest]) ->
    is_value_config_props(Rest);
is_value_config_props(_X) ->
    false.

