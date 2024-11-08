%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell
%% @doc Model for the zotonic config table. Performs a fallback to the site configuration when
%% a key is not defined in the configuration table.
%% @end

%% Copyright 2009-2024 Marc Worrell
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

    set_default_value/4,
    set_value/4,
    set_prop/5,
    get_prop/4,

    delete/3,
    get_id/3,

    is_public_config_key/1
]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context()) -> zotonic_model:return().
m_get([], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {all(Context), []}};
        false -> {error, eacces}
    end;
m_get([ Module ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {get(Module, Context), []}};
        false -> {error, eacces}
    end;
m_get([ Module, Key | Rest ], _Msg, Context) ->
    case is_public_config_key(Key) orelse z_acl:is_admin(Context) of
        true -> {ok, {get(Module, Key, Context), Rest}};
        false -> {error, eacces}
    end;
m_get(_Vs, _Msg, _Context) ->
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
            Indexed1 = lists:map(
                fun({M, CMs}) ->
                    CMs1 = lists:map(
                        fun({K, Vs}) ->
                            K1 = z_convert:to_atom(K),
                            Vs1 = set_is_secret(z_convert:to_binary(K), Vs),
                            {K1, Vs1}
                        end,
                        CMs),
                    {z_convert:to_atom(M), CMs1}
                end,
                Indexed),
            z_depcache:set(config, Indexed1, ?DAY, Context),
            Indexed1
    end.


%% @doc Get the list of configuration key for the module.
%%      Returns the empty list for non existing keys, otherwise
%%      a property list with all the module settings.
%% @todo Change proplists to maps.
-spec get(Module, Context) -> Configs when
    Module :: atom() | binary() | undefined,
    Context :: z:context(),
    Configs :: proplists:proplist().
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
-spec get(Module, Key, Context) -> Config when
    Module :: atom() | binary(),
    Key :: atom() | binary(),
    Context :: z:context(),
    Config :: proplists:proplist() | undefined.
get(zotonic, Key, _Context) when is_atom(Key) ->
    [ {value, z_config:get(Key)} ];
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
                [H|_] = V when not is_integer(H) -> [ {list, V} ];
                V -> [ {value, maybe_convert_to_binary(V)} ]
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

-spec get_value(Module, Key, Context) -> term() when
    Module :: atom() | binary(),
    Key :: atom() | binary(),
    Context :: z:context().
get_value(Module, Key, Context) when is_atom(Module), is_atom(Key) ->
    case get(Module, Key, Context) of
        undefined -> undefined;
        Cfg -> proplists:get_value(value, Cfg)
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

-spec get_value(Module, Key, Default, Context) -> term() when
    Module :: atom() | binary(),
    Key :: atom() | binary(),
    Default :: term(),
    Context :: z:context().
get_value(Module, Key, Default, Context) ->
    case get_value(Module, Key, Context) of
        undefined -> Default;
        Value -> Value
    end.

-spec get_boolean(Module, Key, Context) -> boolean() when
    Module :: atom() | binary(),
    Key :: atom() | binary(),
    Context :: z:context().
get_boolean(Module, Key, Context) ->
    z_convert:to_bool(get_value(Module, Key, Context)).

-spec get_boolean(Module, Key, Default, Context) -> boolean() when
    Module :: atom() | binary(),
    Key :: atom() | binary(),
    Default :: term(),
    Context :: z:context().
get_boolean(Module, Key, Default, Context) ->
    z_convert:to_bool(get_value(Module, Key, Default, Context)).


%% @doc Set the value of a config iff the current value is 'undefined'. Useful for initialization
%% of new module data schemas.
-spec set_default_value(Module, Key, Value, Context) -> ok | {error, Reason} when
    Module :: atom() | binary(),
    Key :: atom() | binary(),
    Value :: string() | binary() | atom(),
    Context :: z:context(),
    Reason :: term().
set_default_value(Module, Key, Value, Context) ->
    case get_value(Module, Key, Context) of
        undefined ->
            case z_db:has_connection(Context) of
                true ->
                    set_value_db(Module, Key, Value, false, Context);
                false ->
                    m_site:put(Module, Key, Value, Context),
                    z_depcache:flush(config, Context),
                    z_notifier:notify(#m_config_update{module = Module, key = Key, value = Value}, Context),
                    ok
            end;
        _Value ->
            ok
    end.

%% @doc Set a "simple" config value.
-spec set_value(Module, Key, Value, Context) -> ok | {error, Reason} when 
    Module :: atom() | binary(),
    Key :: atom() | binary(),
    Value :: string() | binary() | atom(),
    Context :: z:context(),
    Reason :: term().
set_value(Module, Key, Value, Context) ->
    case z_db:has_connection(Context) of
        true ->
            set_value_db(Module, Key, Value, true, Context);
        false ->
            m_site:put(Module, Key, Value, Context),
            z_depcache:flush(config, Context),
            z_notifier:notify(#m_config_update{module = Module, key = Key, value = Value}, Context),
            ok
    end.

-spec set_value_db(Module, Key, Value, IsAllowUpdate, Context) -> ok | {error, Reason} when
    Module :: atom() | binary(),
    Key :: atom() | binary(),
    Value :: string() | binary() | atom(),
    IsAllowUpdate :: boolean(),
    Context :: z:context(),
    Reason :: no_database_connection | term().
set_value_db(Module, Key, Value0, IsAllowUpdate, Context) ->
    ModuleAtom = z_convert:to_atom(Module),
    KeyAtom = z_convert:to_atom(Key),
    KeyBin = z_convert:to_binary(KeyAtom),
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
                        <<"value">> => Value,
                        <<"is_secret">> => is_secret_key(KeyBin)
                    },
                    {ok, _} = z_db:insert(config, Props, Ctx),
                    insert;
                OldValue when IsAllowUpdate ->
                    1 = z_db:q("
                        update config
                        set value = $1,
                            modified = now()
                        where module = $2
                          and key = $3",
                        [ Value, ModuleAtom, KeyAtom ],
                        Ctx),
                    {update, OldValue};
                _ ->
                    % Ignore
                    no_change
            end
        end,
        Context),
    case Result of
        no_change ->
            ok;
        insert ->
            z_depcache:flush(config, Context),
            z_notifier:notify(#m_config_update{module=Module, key=Key, value=Value}, Context),
            IsSecret = is_secret_key(ModuleAtom, KeyAtom, Context),
            z:info(
                "Configuration key '~p.~p' inserted, new value: '~s'",
                [ ModuleAtom, KeyAtom, safe_log_value(IsSecret, Key, Value) ],
                [ {module, ?MODULE}, {line, ?LINE} ],
                Context),
            ok;
        {update, OldV} ->
            z_depcache:flush(config, Context),
            z_notifier:notify(#m_config_update{module=Module, key=Key, value=Value}, Context),
            IsSecret = is_secret_key(ModuleAtom, KeyAtom, Context),
            z:info(
                "Configuration key '~p.~p' changed, new value: '~s', old value '~s'",
                [ ModuleAtom, KeyAtom, safe_log_value(IsSecret, Key, Value), safe_log_value(IsSecret, Key, OldV) ],
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
-spec set_prop(Module, Key, Prop, PropValue, Context) -> ok | {error, Reason} when
    Module :: atom() | binary(),
    Key :: atom() | binary(),
    Prop :: atom() | binary(),
    PropValue :: term(),
    Context :: z:context(),
    Reason :: term().
set_prop(Module, Key, Prop, PropValue, Context) ->
    Result = z_db:transaction(
        fun(Ctx) ->
            PropB = z_convert:to_binary(Prop),
            case z_db:qmap_props_row("
                select *
                from config
                where module = $1
                  and key = $2",
                [ Module, Key ],
                Ctx)
            of
                {error, enoent} ->
                    Ins = #{
                        <<"module">> => Module,
                        <<"key">> => Key,
                        z_convert:to_binary(Prop) => PropValue
                    },
                    z_db:insert(config, Ins, Ctx);
                {ok, #{ PropB := PropValue }} ->
                    {ok, no_change};
                {ok, #{ <<"id">> := Id } } ->
                    Upd = #{
                        PropB => PropValue
                    },
                    z_db:update(config, Id, Upd, Ctx)
            end
        end,
        Context),
    z_depcache:flush(config, Context),
    case Result of
        {ok, no_change} ->
            ok;
        {ok, _} ->
            z_notifier:notify(
                #m_config_update_prop{module = Module, key = Key, prop = Prop, value = PropValue},
                Context
            ),
            IsSecretKey = is_secret_key(Module, Key, Context),
            SafeLogValue = safe_log_value(IsSecretKey, Prop, PropValue),
            z:info(
                "Configuration key '~s.~s' changed, update property '~p' value: ~p",
                [ z_convert:to_binary(Module), z_convert:to_binary(Key), Prop, SafeLogValue ],
                [ {module, ?MODULE}, {line, ?LINE} ],
                Context),
            ok;
        {error, _} = Error ->
            Error
    end.

set_is_secret(Key, CMs) ->
    case proplists:get_value(is_secret, CMs) of
        undefined ->
            [ {is_secret, is_secret_key(Key)} | proplists:delete(is_secret, CMs) ];
        _IsSet ->
            CMs
    end.

is_secret_key(Module, Key, Context) ->
    case is_secret_key(z_convert:to_binary(Key)) of
        true -> true;
        false ->
            case m_config:get(Module, Key, Context) of
                undefined -> false;
                Ps -> proplists:get_value(is_secret, Ps, false)
            end
    end.

is_secret_key(<<"public_", _/binary>>) -> false;
is_secret_key(<<"password_min_length">>) -> false;
is_secret_key(<<"password_regex">>) -> false;
is_secret_key(<<"s3key">>) -> false;
is_secret_key(K) ->
    binary:match(K, <<"password">>) /= nomatch
    orelse binary:match(K, <<"secret">>) /= nomatch
    orelse binary:match(K, <<"key">>) /= nomatch.


%% @doc Get a "complex" config value.
-spec get_prop(Module, Key, Prop, Context) -> Value | undefined when
    Module :: atom() | binary(),
    Key :: atom() | binary(),
    Prop :: atom() | binary(),
    Context :: z:context(),
    Value :: term().
get_prop(Module, Key, Prop, Context) ->
    case m_config:get(Module, Key, Context) of
        undefined ->
            undefined;
        Config ->
            proplists:get_value(z_convert:to_atom(Prop), Config)
    end.

%% @doc Delete the specified module/key combination
-spec delete(Module, Key, Context) -> ok when
    Module :: atom() | binary(),
    Key :: atom() | binary(),
    Context :: z:context().
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

%% @doc Return true if the argument is a public readable configuration key "public_" prefix.
-spec is_public_config_key(Key) -> boolean() when
    Key :: atom() | binary().
is_public_config_key(Key) when is_atom(Key) ->
    is_public_config_key(atom_to_binary(Key, utf8));
is_public_config_key(<<"public_", _/binary>>) ->
    true;
is_public_config_key(_) ->
    false.

%% Create a safe value for the log. Secret values are abbreviated.
safe_log_value(true, _Key, Value) ->
    safe_log_value(Value);
safe_log_value(false, Key, Value) ->
    case is_secret_key(z_convert:to_binary(Key)) of
        true -> safe_log_value(Value);
        false -> Value
    end.

safe_log_value(Value) when is_binary(Value) andalso size(Value) > 6 ->
    <<First:3/binary, _/binary>> = Value,
    <<First/binary, "****">>;
safe_log_value(_) ->
    <<"****">>.

maybe_convert_to_binary(V) when is_list(V) ->
    unicode:characters_to_binary(V);
maybe_convert_to_binary(V) ->
    V.
