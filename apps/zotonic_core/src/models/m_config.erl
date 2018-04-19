%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2017 Marc Worrell
%% @doc Model for the zotonic config table. Performs a fallback to the site configuration when
%% a key is not defined in the configuration table.

%% Copyright 2009-2017 Marc Worrell
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

-module(m_config).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/2,
    all/1,
    get/2,
    get/3,
    get_value/3,
    get_value/4,

    get_boolean/3,
    get_boolean/4,

    set_value/4,
    set_prop/5,

    delete/3,
    get_id/3
]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), z:context()) -> {term(), list()}.
m_get([], Context) ->
    case z_acl:is_admin(Context) of
        true -> {all(Context), []};
        false -> {[], []}
    end;
m_get([ Module ], Context) ->
    case z_acl:is_admin(Context) of
        true -> {get(Module, Context), []};
        false -> {[], []}
    end;
m_get([ Module, Key | Rest ], Context) ->
    case z_acl:is_admin(Context) of
        true -> {get(Module, Key, Context), Rest};
        false -> {[], Rest}
    end;
m_get(Vs, _Context) ->
    lager:error("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {undefined, []}.


%% @doc Return all configurations from the configuration table. Returns a nested proplist (module, key)
all(Context) ->
    case z_depcache:get(config, Context) of
        {ok, Cs} ->
            Cs;
        undefined ->
            Cs = case z_db:has_connection(Context) of
                     true -> try
                                 z_db:assoc_props("select * from config order by module, key", Context)
                             catch
                                 %% When Zotonic has not yet been installed, there is no config table yet
                                 _:_ -> []
                             end;
                     false -> []
                 end,
            Indexed = [
                {M, z_utils:index_proplist(key, CMs)}
                || {M, CMs} <- z_utils:group_proplists(module, Cs)
            ],
            Indexed1 = [
                {z_convert:to_atom(M), [{z_convert:to_atom(K), Vs} || {K, Vs} <- CMs]}
                || {M, CMs} <- Indexed
            ],
            z_depcache:set(config, Indexed1, ?DAY, Context),
            Indexed
    end.


%% @doc Get the list of configuration key for the module.
get(Module, Context) when is_atom(Module) ->
    ConfigProps = case z_depcache:get_subkey(config, Module, Context) of
        {ok, undefined} ->
            [];
        {ok, Cs} ->
            Cs;
        undefined ->
            All = all(Context),
            proplists:get_value(Module, All, [])
    end,
    case m_site:get(Module, Context) of
        L when is_list(L) -> z_convert:to_list(ConfigProps) ++ L;
        _ -> ConfigProps
    end.

%% @doc Get a configuration value for the given module/key combination.
%% @spec get(Module::atom(), Key::atom(), #context{}) -> Value | undefined
get(zotonic, Key, _Context) when is_atom(Key) ->
    [{value, z_config:get(Key)}];
get(Module, Key, Context) when is_atom(Module) andalso is_atom(Key) ->
    Value = case z_depcache:get_subkey(config, Module, Context) of
        {ok, undefined} ->
            undefined;
        {ok, Cs} ->
            proplists:get_value(Key, Cs);
        undefined ->
            All = all(Context),
            case proplists:get_value(Module, All) of
                undefined -> undefined;
                Cs -> proplists:get_value(Key, Cs)
            end
    end,
    case Value of
        undefined ->
            case m_site:get(Module, Key, Context) of
                undefined -> Value;
                [H|_] = V when is_tuple(H) -> [{list, V}];
                V -> [{value, V}]
            end;
        _ ->
            Value
    end.


get_value(Module, Key, Context) when is_atom(Module) andalso is_atom(Key) ->
    Value = case get(Module, Key, Context) of
        undefined -> undefined;
        Cfg -> proplists:get_value(value, Cfg)
    end,
    case Value of
        undefined -> m_site:get(Module, Key, Context);
        _ -> Value
    end.


get_value(Module, Key, Default, Context) when is_atom(Module) andalso is_atom(Key) ->
    case get_value(Module, Key, Context) of
        undefined -> Default;
        Value -> Value
    end.

get_boolean(Module, Key, Context) ->
    z_convert:to_bool(get_value(Module, Key, Context)).

get_boolean(Module, Key, Default, Context) ->
    z_convert:to_bool(get_value(Module, Key, Default, Context)).

%% @doc Set a "simple" config value.
-spec set_value(atom(), atom(), term(), #context{}) -> ok.
set_value(Module, Key, Value, Context) ->
    case z_db:q(
        "update config set value = $1, modified = now() where module = $2 and key = $3",
        [Value, Module, Key],
        Context
    ) of
        0 ->
            z_db:insert(config, [{module, Module}, {key, Key}, {value, Value}], Context);
        [] -> ok;
        1 -> ok
    end,
    z_depcache:flush(config, Context),
    z_notifier:notify(#m_config_update{module = Module, key = Key, value = Value}, Context),
    ok.


%% @doc Set a "complex" config value.
-spec set_prop(atom(), atom(), atom(), any, z:context()) -> ok.
set_prop(Module, Key, Prop, PropValue, Context) ->
    case get_id(Module, Key, Context) of
        undefined ->
            z_db:insert(config, [{module, Module}, {key, Key}, {Prop, PropValue}], Context);
        Id ->
            z_db:update(config, Id, [{Prop, PropValue}], Context)
    end,
    z_depcache:flush(config, Context),
    z_notifier:notify(
        #m_config_update_prop{module = Module, key = Key, prop = Prop, value = PropValue},
        Context
    ),
    ok.


%% @doc Delete the specified module/key combination
%% @spec delete(Module::atom(), Key::atom(), #context{}) -> ok
delete(Module, Key, Context) ->
    z_db:q("delete from config where module = $1 and key = $2", [Module, Key], Context),
    z_depcache:flush(config, Context),
    z_notifier:notify(
        #m_config_update{module = Module, key = Key, value = undefined},
        Context
    ),
    ok.


%% @doc Lookup the unique id in the config table from the module/key combination.
get_id(Module, Key, Context) ->
    z_db:q1("select id from config where module = $1 and key = $2", [Module, Key], Context).


