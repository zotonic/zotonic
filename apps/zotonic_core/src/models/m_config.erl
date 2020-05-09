%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2020 Marc Worrell
%% @doc Model for the zotonic config table. Performs a fallback to the site configuration when
%% a key is not defined in the configuration table.

%% Copyright 2009-2020 Marc Worrell
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
    m_get/3,
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
-spec m_get( list(), zotonic_model:opt_msg(), z:context()) -> zotonic_model:return().
m_get([], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {all(Context), []}};
        false -> {ok, {[], []}}
    end;
m_get([ Module ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {get(Module, Context), []}};
        false -> {ok, {[], []}}
    end;
m_get([ Module, Key | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {get(Module, Key, Context), Rest}};
        false -> {ok, {[], Rest}}
    end;
m_get(Vs, _Msg, _Context) ->
    lager:error("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {error, unknown_path}.

%% @doc Return all configurations from the configuration table. Returns a nested proplist (module, key)
%% @todo Change proplists to maps
-spec all( z:context() ) -> list().
all(Context) ->
    case z_depcache:get(config, Context) of
        {ok, Cs} ->
            Cs;
        undefined ->
            Cs = case z_db:has_connection(Context) of
                     true ->
                        try
                            z_db:assoc_props("select * from config order by module, key", Context)
                        catch
                            %% When Zotonic has not yet been installed, there is no config table yet
                            _:_ -> []
                        end;
                     false ->
                        []
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
            Indexed1
    end.


%% @doc Get the list of configuration key for the module.
%%      Returns the empty list for non existing keys, otherwise
%%      a property list with all the module settings.
%% @todo Change proplists to maps.
-spec get( atom() | binary() | undefined, z:context() ) -> proplists:proplist().
get(undefined, _Context) ->
    [];
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
    end;
get(Module, Context) when is_binary(Module) ->
    try
        Module1 = binary_to_existing_atom(Module, utf8),
        get(Module1, Context)
    catch
        error:badarg ->
            []
    end.

%% @doc Get a configuration value for the given module/key combination.
-spec get( atom() | binary(), atom() | binary(), z:context() ) -> proplists:proplist() | undefined.
get(zotonic, Key, _Context) when is_atom(Key) ->
    [
        {value, z_config:get(Key)}
    ];
get(Module, Key, Context) when is_atom(Module), is_atom(Key) ->
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
                undefined -> undefined;
                [H|_] = V when is_tuple(H) -> [{list, V}];
                V -> [{value, V}]
            end;
        _ ->
            Value
    end;
get(Module, Key, Context) when is_binary(Module) ->
    try
        Module1 = binary_to_existing_atom(Module, utf8),
        get(Module1, Key, Context)
    catch
        error:badarg ->
            undefined
    end;
get(Module, Key, Context) when is_binary(Key) ->
    try
        Key1 = binary_to_existing_atom(Key, utf8),
        get(Module, Key1, Context)
    catch
        error:badarg ->
            undefined
    end.



-spec get_value( atom() | binary(), atom() | binary(), z:context() ) -> term() | undefined.
get_value(Module, Key, Context) when is_atom(Module), is_atom(Key) ->
    Value = case get(Module, Key, Context) of
        undefined -> undefined;
        Cfg -> proplists:get_value(value, Cfg)
    end,
    case Value of
        undefined -> m_site:get(Module, Key, Context);
        _ -> Value
    end;
get_value(Module, Key, Context) when is_binary(Module) ->
    try
        Module1 = binary_to_existing_atom(Module, utf8),
        get_value(Module1, Key, Context)
    catch
        error:badarg ->
            undefined
    end;
get_value(Module, Key, Context) when is_binary(Key) ->
    try
        Key1 = binary_to_existing_atom(Key, utf8),
        get_value(Module, Key1, Context)
    catch
        error:badarg ->
            undefined
    end.


-spec get_value( atom() | binary(), atom() | binary(), term(), z:context() ) -> term() | undefined.
get_value(Module, Key, Default, Context) ->
    case get_value(Module, Key, Context) of
        undefined -> Default;
        Value -> Value
    end.

-spec get_boolean( atom() | binary(), atom() | binary(), z:context() ) -> boolean().
get_boolean(Module, Key, Context) ->
    z_convert:to_bool(get_value(Module, Key, Context)).

-spec get_boolean( atom() | binary(), atom() | binary(), term(), z:context() ) -> boolean().
get_boolean(Module, Key, Default, Context) ->
    z_convert:to_bool(get_value(Module, Key, Default, Context)).

%% @doc Set a "simple" config value.
-spec set_value( atom() | binary(), atom() | binary(), string() | binary() | atom(), z:context() ) -> ok | {error, term()}.
set_value(Module, Key, Value, Context) ->
    case z_db:has_connection(Context) of
        true ->
            set_value_db(Module, Key, Value, Context);
        false ->
            m_site:put(Module, Key, Value, Context),
            z_depcache:flush(config, Context),
            z_notifier:notify(#m_config_update{module = Module, key = Key, value = Value}, Context),
            ok
    end.

-spec set_value_db( atom() | binary(), atom() | binary(), string() | binary() | atom(), z:context() ) -> ok | {error, term()}.
set_value_db(Module, Key, Value0, Context) ->
    ModuleAtom = z_convert:to_atom(Module),
    KeyAtom = z_convert:to_atom(Key),
    Value = z_convert:to_binary(Value0),
    Result = z_db:transaction(
        fun(Ctx) ->
            case z_db:q1("
                select value
                from config
                where module = $1
                  and key = $2",
                [ Module, Key ],
                Ctx)
            of
                Value ->
                    no_change;
                undefined ->
                    Props = #{
                        <<"module">> => ModuleAtom,
                        <<"key">> => KeyAtom,
                        <<"value">> => Value
                    },
                    {ok, _} = z_db:insert(config, Props, Ctx),
                    insert;
                OldValue ->
                    1 = z_db:q("
                        update config
                        set value = $1,
                            modified = now()
                        where module = $2
                          and key = $3",
                        [ Value, ModuleAtom, KeyAtom ],
                        Ctx),
                    {update, OldValue}
            end
        end,
        Context),
    case Result of
        no_change ->
            ok;
        insert ->
            z_depcache:flush(config, Context),
            z_notifier:notify(#m_config_update{module=Module, key=Key, value=Value}, Context),
            z:info(
                "Configuration key '~p.~p' inserted, new value: '~s'",
                [ ModuleAtom, KeyAtom, Value ],
                [ {module, ?MODULE}, {line, ?LINE} ],
                Context),
            ok;
        {update, OldV} ->
            z_depcache:flush(config, Context),
            z_notifier:notify(#m_config_update{module=Module, key=Key, value=Value}, Context),
            z:info(
                "Configuration key '~p.~p' changed, new value: '~s', old value '~s'",
                [ ModuleAtom, KeyAtom, Value, OldV ],
                [ {module, ?MODULE}, {line, ?LINE} ],
                Context),
            ok;
        {rollback,{no_database_connection, _Trace}} ->
            {error, no_database_connection};
        {rollback, {error, _} = Error} ->
            Error;
        {rollback, Error} ->
            {error, Error}
    end.

%% @doc Set a "complex" config value.
-spec set_prop(atom()|binary(), atom()|binary(), atom()|binary(), term(), z:context()) -> ok.
set_prop(Module, Key, Prop, PropValue, Context) ->
    z_db:transaction(
        fun(Ctx) ->
            case get_id(Module, Key, Ctx) of
                undefined ->
                    Ins = #{
                        <<"module">> => Module,
                        <<"key">> => Key,
                        z_convert:to_binary(Prop) => PropValue
                    },
                    z_db:insert(config, Ins, Ctx);
                Id ->
                    Upd = #{
                        z_convert:to_binary(Prop) => PropValue
                    },
                    z_db:update(config, Id, Upd, Ctx)
            end
        end,
        Context),
    z_depcache:flush(config, Context),
    z_notifier:notify(
        #m_config_update_prop{module = Module, key = Key, prop = Prop, value = PropValue},
        Context
    ),
    z:info(
        "Configuration key '~s.~s' changed, new property '~p' value: ~p",
        [ z_convert:to_binary(Module), z_convert:to_binary(Key), Prop, PropValue ],
        [ {module, ?MODULE}, {line, ?LINE} ],
        Context),
    ok.


%% @doc Delete the specified module/key combination
-spec delete( atom()|binary(), atom()|binary(), z:context() ) -> ok.
delete(Module, Key, Context) ->
    Module1 = z_convert:to_atom(Module),
    Key1 = z_convert:to_atom(Key),
    z_db:q("delete from config where module = $1 and key = $2", [Module1, Key1], Context),
    z_depcache:flush(config, Context),
    z_notifier:notify(
        #m_config_update{module = Module1, key = Key1, value = undefined},
        Context
    ),
    z:info(
        "Configuration key '~s.~s' deleted",
        [ z_convert:to_binary(Module1), z_convert:to_binary(Key1) ],
        [ {module, ?MODULE}, {line, ?LINE} ],
        Context),
    ok.


%% @doc Lookup the unique id in the config table from the module/key combination.
get_id(Module, Key, Context) ->
    z_db:q1("select id from config where module = $1 and key = $2", [Module, Key], Context).


