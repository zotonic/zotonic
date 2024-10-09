%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell
%% @doc Manage identities of users. An identity can be a username/password, openid, oauth credentials etc.
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

-module(m_identity).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

-export([
    m_get/3,

    is_user/2,
    user_types/1,
    get_username/1,
    get_username/2,
    get_user_info/1,
    get_user_info/2,
    delete_username/2,
    set_username/3,
    set_username_pw/4,
    reset_password/2,
    set_expired/3,
    set_identity_expired/3,
    set_visited/2,
    logon_history/2,
    cleanup_logon_history/1,
    ensure_username_pw/2,
    ensure_username_pw/3,
    check_username_pw/3,
    check_username_pw/4,
    hash/1,
    needs_rehash/1,
    hash_is_equal/2,
    get/2,
    get_rsc/2,
    get_rsc_by_type/3,
    get_rsc_by_type_key/4,
    get_rsc_by_type_keyprefix/4,
    get_rsc/3,

    is_email_verified/1,
    is_email_verified/2,

    is_valid_key/3,
    normalize_key/2,

    lookup_by_username/2,
    lookup_by_verify_key/2,
    lookup_by_type_and_key/3,
    lookup_by_type_and_key_multi/3,

    lookup_users_by_type_and_key/3,
    lookup_users_by_verified_type_and_key/3,

    set_by_type/4,
    set_by_type/5,
    delete_by_type/3,
    delete_by_type_and_key/4,
    delete_by_type_and_keyprefix/4,

    insert/4,
    insert/5,
    insert_single/4,
    insert_single/5,
    insert_unique/4,
    insert_unique/5,

    set_verify_key/2,
    set_verified/2,
    set_verified/4,
    is_verified/2,
    verify_primary_email/2,

    delete/2,
    merge/3,
    is_reserved_name/1,
    is_peer_allowed/1
]).

-export([
    generate_username/2
]).

-type password() :: iodata().
-type bcrypt_hash() :: {bcrypt, binary()}.
-type sha1_salted_hash() :: {hash, binary(), binary()}.
-type hash() :: bcrypt_hash() | sha1_salted_hash().

-type type() :: atom() | binary() | string().
-type key() :: atom() | binary() | string().

-type identity() :: proplists:proplist().

-export_type([
    type/0,
    key/0,
    password/0,
    identity/0,
    hash/0,
    bcrypt_hash/0,
    sha1_salted_hash/0
    ]).

-include_lib("zotonic.hrl").

-define(IDN_CACHE_TIME, 3600*12).
-define(IDN_LOG_TTL, 3600*24*30).

%% Default duration and random variance interval for password checks.
%% This prevents a timing difference between checks for existing and
%% non existing accounts.
-define(DEFAULT_PW_CHECK_DURATION, 280).
-define(DEFAULT_PW_CHECK_VARIANCE, 40).


%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context()) -> zotonic_model:return().
m_get([ <<"lookup">>, Type, Key | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            Idns = filter_idns(lookup_by_type_and_key_multi(Type, Key, Context)),
            {ok, {Idns, Rest}};
        false ->
            {error, eacces}
    end;
m_get([ <<"generate_password">> | Rest ], _Msg, _Context) ->
    {ok, {z_ids:password(), Rest}};
m_get([ <<"is_email_verified">> ], _Msg, Context) ->
    {ok, {is_email_verified(Context), []}};
m_get([ <<"is_email_verified">>, UserId | Rest ], _Msg, Context) ->
    case m_rsc:rid(UserId, Context) of
        undefined ->
            {error, enoent};
        Id ->
            case z_acl:rsc_editable(Id, Context) of
                true ->
                    {ok, {is_email_verified(Id, Context), Rest}};
                false ->
                    {error, eacces}
            end
    end;
m_get([ Id, <<"is_user">> | Rest ], _Msg, Context) ->
    case z_acl:rsc_visible(Id, Context) of
        true ->
            {ok, {is_user(Id, Context), Rest}};
        false ->
            {error, eacces}
    end;
m_get([ Id, <<"username">> | Rest ], _Msg, Context) ->
    case z_acl:rsc_editable(Id, Context) of
        true ->
            Username = get_username(Id, Context),
            {ok, {Username, Rest}};
        false ->
            {error, eacces}
    end;
m_get([ Id, <<"user_info">> | Rest ], _Msg, Context) ->
    case z_acl:rsc_editable(Id, Context) of
        true ->
            Info = get_user_info(Id, Context),
            {ok, {Info, Rest}};
        false ->
            {error, eacces}
    end;
m_get([ Id, <<"logon_history">> | Rest ], _Msg, Context) ->
    case logon_history(Id, Context) of
        {error, Error} -> {error, Error};
        {ok, LogonHistory} -> {ok, {LogonHistory, Rest}}
    end;
m_get([ Id, <<"all_types">> | Rest ], _Msg, Context) ->
    Idns = case z_acl:rsc_editable(Id, Context) of
        true -> get_rsc_types(Id, Context);
        false -> []
    end,
    {ok, {Idns, Rest}};
m_get([ Id, <<"all">> ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            Idns = filter_idns(get_rsc(Id, Context)),
            {ok, {Idns, []}};
        false ->
            {error, eacces}
    end;
m_get([ Id, <<"all">>, <<"email">> | Rest ], _Msg, Context) ->
    case z_acl:rsc_editable(Id, Context) of
        true ->
            Idns = filter_idns(get_rsc_by_type(Id, <<"email">>, Context)),
            {ok, {Idns, Rest}};
        false ->
            {error, eacces}
    end;
m_get([ Id, <<"all">>, Type | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            Idns = filter_idns(get_rsc_by_type(Id, Type, Context)),
            {ok, {filter_idns(Idns), Rest}};
        false ->
            {error, enoent}
    end;
m_get([ <<"get">>, IdnId | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            case get(IdnId, Context) of
                undefined ->
                    {error, enoent};
                Idn ->
                    {ok, {filter_idn(Idn), Rest}}
            end;
        false ->
            {error, eacces}
    end;
m_get([ <<"verify">>, IdnId, VerifyKey | Rest ], _Msg, Context) ->
    Idn1 = case get(IdnId, Context) of
        Idn when is_list(Idn), is_binary(VerifyKey), VerifyKey =/= <<>> ->
            IdnVerifyKey = z_convert:to_binary(proplists:get_value(verify_key, Idn, <<>>)),
            case is_equal(VerifyKey, IdnVerifyKey) of
                true -> Idn;
                false -> undefined
            end;
        _ ->
            undefined
    end,
    {ok, {Idn1, Rest}};
m_get([ Id, Type | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            case get_rsc(Id, Type, Context) of
                undefined ->
                    {error, enoent};
                Idn ->
                    {ok, {Idn, Rest}}
            end;
        false ->
            {error, eacces}
    end;
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.


%% @doc Filter an identity record to prevent leaking the information
%% in the propb and other prop fields.
-spec filter_idn(Identity | undefined) -> CleanIdentity | undefined when
    Identity :: proplists:proplist(),
    CleanIdentity :: proplists:proplist().
filter_idn(undefined) ->
    undefined;
filter_idn(Idn) ->
    Type = proplists:get_value(type, Idn),
    Key = case binary:match(Type, <<"_secret">>) of
        nomatch -> proplists:get_value(key, Idn);
        _ -> undefined
    end,
    [
        {id, proplists:get_value(id, Idn)},
        {rsc_id, proplists:get_value(rsc_id, Idn)},
        {type, Type},
        {key, Key},
        {is_verified, proplists:get_value(is_verified, Idn)},
        {is_unique, proplists:get_value(is_unique, Idn)},
        {modified, proplists:get_value(modified, Idn)},
        {created, proplists:get_value(modified, Idn)},
        {expires, proplists:get_value(expires, Idn)}
    ].

%% @doc Filter a list of identity records to prevent leaking the information
%% in the propb and other prop fields.
-spec filter_idns(Identities) -> CleanIdentities when
    Identities :: list( proplists:proplist() ),
    CleanIdentities :: list( proplists:proplist() ).
filter_idns(Idns) ->
    lists:map(fun filter_idn/1, Idns).


%% @doc Check if the resource has any credentials that will make them a user
-spec is_user(RscId, Context) -> boolean() when
    RscId :: m_rsc:resource(),
    Context :: z:context().
is_user(Id, Context) ->
    IdentityTypes = user_types(Context),
    IdentityTypes1 = [ z_convert:to_binary(Idn) || Idn <- lists:usort(IdentityTypes) ],
    case z_db:q1("
        select count(*)
        from identity
        where rsc_id = $1
          and type = any($2)",
        [ m_rsc:rid(Id, Context), IdentityTypes1 ],
        Context)
    of
        0 -> false;
        _ -> true
    end.

%% @doc Return the identity types that define if a resource is a user.\
-spec user_types(Context) -> [ Type ] when
    Context :: z:context(),
    Type :: atom().
user_types(Context) ->
    z_notifier:foldl(
        #auth_identity_types{ type = user },
        [ username_pw ],
        Context).

%% @doc Return the username of the current user
-spec get_username(Context) -> Username | undefined when
    Context :: z:context(),
    Username :: binary().
get_username(Context) ->
    case z_acl:user(Context) of
        undefined -> undefined;
        UserId -> get_username(UserId, Context)
    end.

%% @doc Return the username of the resource id, undefined if no username
-spec get_username(RscId, Context) -> Username | undefined when
    RscId :: m_rsc:resource(),
    Context :: z:context(),
    Username :: binary().
get_username(RscId, Context) ->
    F = fun() ->
        z_db:q1(
            "select key from identity where rsc_id = $1 and type = 'username_pw'",
            [m_rsc:rid(RscId, Context)],
            Context)
    end,
    z_depcache:memo(F, {username, RscId}, 3600, [ {idn, RscId} ], Context).


%% @doc Return the username and last login of the current user.
-spec get_user_info(Context) -> UserInfo when
    Context :: z:context(),
    UserInfo :: #{ binary() => term() }.
get_user_info(Context) ->
    get_user_info(z_acl:user(Context), Context).

%% @doc Return the user_id, username and last login of the resource id. Returns empty values
%% if the resource is not an user.
-spec get_user_info(RscId, Context) -> UserInfo when
    RscId :: m_rsc:resource(),
    Context :: z:context(),
    UserInfo :: #{ binary() => term() }.
get_user_info(undefined, _Context) ->
    empty_user_info(undefined);
get_user_info(Rsc, Context) ->
    case m_rsc:rid(Rsc, Context) of
        undefined ->
            empty_user_info(undefined);
        RscId ->
            Row = z_db:q_row("
                     select key, visited, modified,
                            coalesce(expires <= now(),false) as is_expired
                     from identity
                     where rsc_id = $1
                       and type = 'username_pw'",
                    [RscId],
                    Context),
            case Row of
                undefined ->
                    empty_user_info(RscId);
                {Key, Visited, Modified, IsExpired} ->
                    #{
                        <<"user_id">> => RscId,
                        <<"username">> => Key,
                        <<"visited">> => Visited,
                        <<"modified">> => Modified,
                        <<"is_expired">> => IsExpired
                    }
            end
    end.

empty_user_info(RscId) ->
    #{
        <<"user_id">> => RscId,
        <<"username">> => undefined,
        <<"visited">> => undefined,
        <<"modified">> => undefined,
        <<"is_expired">> => false
    }.

%% @doc Check if the user is allowed to change the username of a resource.
-spec is_allowed_set_username(UserId, Context) -> boolean() when
    UserId :: m_rsc:resource_id(),
    Context :: z:context().
is_allowed_set_username(Id, Context) when is_integer(Id) ->
    z_acl:is_admin(Context)
    orelse z_acl:is_allowed(use, mod_admin_identity, Context)
    orelse (z_acl:is_allowed(update, Id, Context) andalso Id =:= z_acl:user(Context)).


%% @doc Delete a username from a resource.
-spec delete_username(UserId, Context) -> ok | {error, Reason} when
    UserId :: m_rsc:resource() | undefined,
    Context :: z:context(),
    Reason :: eacces | enoent.
delete_username(undefined, _Context) ->
    {error, enoent};
delete_username(?ACL_ADMIN_USER_ID, Context) ->
    ?LOG_WARNING(#{
        text => <<"Trying to delete admin username (1)">>,
        in => zotonic_core,
        user_id => z_acl:user(Context)
    }),
    {error, eacces};
delete_username(RscId, Context) when is_integer(RscId) ->
    case is_allowed_set_username(RscId, Context)  of
        true ->
            z_db:q(
                "delete from identity where rsc_id = $1 and type = any('username_pw', 'auth_autologon_secret')",
                [RscId],
                Context
            ),
            notify(RscId, delete, <<"username_pw">>, undefined, undefined, Context),
            ok;
        false ->
            {error, eacces}
    end;
delete_username(Id, Context) ->
    delete_username( m_rsc:rid(Id, Context), Context ).


%% @doc Mark the username_pw identity of a user as 'expired', this forces a prompt
%%      for a password reset on the next authentication.
-spec set_expired(UserId, DateTime, Context) -> ok | {error, enoent} when
    UserId :: m_rsc:resource_id(),
    DateTime :: undefined | boolean() | calendar:datetime(),
    Context :: z:context().
set_expired(undefined, _DateTime, _Context) ->
    ok;
set_expired(?ACL_ADMIN_USER_ID, _DateTime, _Context) ->
    {error, enoent};
set_expired(UserId, true, Context) when is_integer(UserId) ->
    case z_db:q("
            update identity
            set expires = now(),
                modified = now()
            where rsc_id = $1
              and type = 'username_pw'",
            [UserId],
            Context)
    of
        1 ->
            ok;
        0 ->
            flush(UserId, Context),
            {error, enoent}
    end;
set_expired(UserId, false, Context) when is_integer(UserId) ->
    case z_db:q("
            update identity
            set expires = NULL
            where rsc_id = $1
              and type = 'username_pw'",
            [UserId],
            Context)
    of
        1 ->
            ok;
        0 ->
            flush(UserId, Context),
            {error, enoent}
    end;
set_expired(UserId, DateTime, Context) when is_integer(UserId) ->
    case z_db:q("
            update identity
            set expires = $2
            where rsc_id = $1
              and type = 'username_pw'",
            [UserId, DateTime],
            Context)
    of
        1 ->
            ok;
        0 ->
            flush(UserId, Context),
            {error, enoent}
    end.

%% @doc Mark the username_pw identity of a user as 'expired', this forces a prompt
%%      for a password reset on the next authentication.
-spec set_identity_expired(IdnId, DateTime, Context) -> ok | {error, enoent} when
    IdnId :: pos_integer(),
    DateTime :: undefined | boolean() | calendar:datetime(),
    Context :: z:context().
set_identity_expired(undefined, _DateTime, _Context) ->
    ok;
set_identity_expired(IdnId, true, Context) when is_integer(IdnId) ->
    case z_db:q("
            update identity
            set expires = now(),
                modified = now()
            where id = $1",
            [IdnId],
            Context)
    of
        1 ->
            ok;
        0 ->
            flush(IdnId, Context),
            {error, enoent}
    end;
set_identity_expired(IdnId, false, Context) when is_integer(IdnId) ->
    case z_db:q("
            update identity
            set expires = NULL
            where id = $1",
            [IdnId],
            Context)
    of
        1 ->
            ok;
        0 ->
            flush(IdnId, Context),
            {error, enoent}
    end;
set_identity_expired(IdnId, DateTime, Context) when is_integer(IdnId) ->
    case z_db:q("
            update identity
            set expires = $2
            where id = $1",
            [IdnId, DateTime],
            Context)
    of
        1 ->
            ok;
        0 ->
            flush(IdnId, Context),
            {error, enoent}
    end.


%% @doc Change the username of the resource id, only possible if there is
%% already a username/password set
-spec set_username(UserId, Username, Context) -> ok | {error, Reason} when
    UserId :: m_rsc:resource() | undefined,
    Username :: binary() | string(),
    Context :: z:context(),
    Reason :: eacces | enoent | eexist.
set_username(undefined, _Username, _Context) ->
    {error, enoent};
set_username(?ACL_ADMIN_USER_ID, _Username, Context) ->
    ?LOG_WARNING(#{
        text => <<"Trying to set admin username (1)">>,
        in => zotonic_core,
        result => error,
        reason => eacces,
        user_id => z_acl:user(Context)
    }),
    {error, eacces};
set_username(Id, Username, Context) when is_integer(Id) ->
    case is_allowed_set_username(Id, Context) of
        true ->
            Username1 = normalize_key(username_pw, Username),
            case is_reserved_name(Username1) of
                true ->
                    {error, eexist};
                false ->
                    F = fun(Ctx) ->
                        UniqueTest = z_db:q1("
                            select count(*)
                            from identity
                            where type = 'username_pw'
                              and rsc_id <> $1 and key = $2",
                            [Id, Username1],
                            Ctx
                        ),
                        case UniqueTest of
                            0 ->
                                case z_db:q("
                                        update identity
                                        set key = $2,
                                            modified = now()
                                        where rsc_id = $1
                                          and type = 'username_pw'",
                                        [Id, Username1],
                                        Ctx)
                                of
                                    1 -> ok;
                                    0 -> {error, enoent}
                                end;
                            _Other ->
                                {error, eexist}
                        end
                    end,
                    case z_db:transaction(F, Context) of
                        ok ->
                            z:info(
                                "Change of username for user ~p (~s)",
                                [ Id, Username1 ],
                                [ {module, ?MODULE} ],
                                Context),
                            notify(Id, update, <<"username_pw">>, Username1, true, Context),
                            ok;
                        {rollback, {error, _} = Error, _Trace} ->
                            Error;
                        {error, _} = Error ->
                            Error
                    end
            end;
        false ->
            {error, eacces}
    end;
set_username(Id, Username, Context) ->
    set_username( m_rsc:rid(Id, Context), Username, Context ).


%% @doc Set the username/password of a resource.  Replaces any existing username/password. If the
%% configuration "site.password_force_different" is set and the new password is the same as the old
%% password then the error password_match is returned.  The username is lowercased.
-spec set_username_pw(UserId, Username, Password, Context) -> ok | {error, Reason} when
    UserId :: m_rsc:resource() | undefined,
    Username :: binary() | string(),
    Password :: binary() | string(),
    Context :: z:context(),
    Reason :: eacces | enoent | eexist | password_match | term().
set_username_pw(undefined, _, _, _) ->
    {error, enoent};
set_username_pw(?ACL_ADMIN_USER_ID, _, _, Context) ->
    % The password of the admin is set in the priv/zotonic_site.config file.
    ?LOG_WARNING(#{
        text => <<"Trying to set admin username (1)">>,
        in => zotonic_core,
        user_id => z_acl:user(Context),
        result => error,
        reason => eacces
    }),
    {error, eacces};
set_username_pw(Id, Username, Password, Context)  when is_integer(Id) ->
    case is_allowed_set_username(Id, Context) of
        true ->
            Username1 = normalize_key(username_pw, Username),
            IsForceDifferent = z_convert:to_bool( m_config:get_value(site, password_force_different, Context) ),
            set_username_pw_1(IsForceDifferent, m_rsc:rid(Id, Context), Username1, Password, Context);
        false ->
            {error, eacces}
    end;
set_username_pw(Id, Username, Password, Context) ->
    set_username_pw(m_rsc:rid(Id, Context), Username, Password, Context).

set_username_pw_1(true, Id, Username, Password, Context) ->
    case check_username_pw_1(Username, Password, Context) of
        {ok, _} ->
            {error, password_match};
        {error, E} when E =:= nouser; E =:= password ->
            set_username_pw_2(Id, Username, Password, Context);
        {error, _} = Error ->
            Error
    end;
set_username_pw_1(false, Id, Username, Password, Context) when is_integer(Id) ->
    set_username_pw_2(Id, Username, Password, Context).


set_username_pw_2(Id, Username, Password, Context) when is_integer(Id) ->
    Hash = hash(Password),
    case z_db:transaction(fun(Ctx) -> set_username_pw_trans(Id, Username, Hash, Ctx) end, Context) of
        {ok, S} ->
            Action = case S of
                new ->
                    z:info(
                        "New username/password for user ~p (~s)",
                        [ Id, Username ],
                        [ {module, ?MODULE} ],
                        Context),
                    insert;
                exists ->
                    z:info(
                        "Change of username/password for user ~p (~s)",
                        [ Id, Username ],
                        [ {module, ?MODULE} ],
                        Context),
                    update
            end,
            reset_auth_tokens(Id, Context),
            notify(Id, Action, <<"username_pw">>, Username, true, Context),
            ok;
        {rollback, {{error, Reason} = Error, Trace}} ->
            ?LOG_ERROR(#{
                text => <<"Error setting username/password">>,
                in => zotonic_core,
                user_id => Id,
                username => Username,
                result => error,
                reason => Reason,
                stack => Trace
            }),
            Error;
        {error, Reason} = Error ->
            ?LOG_ERROR(#{
                text => <<"Error setting username/password">>,
                in => zotonic_core,
                result => error,
                reason => Reason,
                user_id => Id,
                username => Username
            }),
            Error
    end.

set_username_pw_trans(Id, Username, Hash, Context) ->
    case z_db:q("
                update identity
                set key = $2,
                    propb = $3,
                    prop1 = '',
                    expires = null,
                    is_verified = true,
                    modified = now()
                where type = 'username_pw'
                  and rsc_id = $1",
        [Id, Username, ?DB_PROPS(Hash)],
        Context)
    of
        0 ->
            case is_reserved_name(Username) of
                true ->
                    {rollback, {error, eexist}};
                false ->
                    UniqueTest = z_db:q1(
                        "select count(*) from identity where type = 'username_pw' and key = $1",
                        [Username],
                        Context
                    ),
                    case UniqueTest of
                        0 ->
                            1 = z_db:q(
                                "insert into identity (rsc_id, is_unique, is_verified, type, key, propb)
                                values ($1, true, true, 'username_pw', $2, $3)",
                                [Id, Username, ?DB_PROPS(Hash)],
                                Context
                            ),
                            z_db:q(
                                "update rsc set creator_id = id where id = $1 and creator_id <> id",
                                [Id],
                                Context
                            ),
                            {ok, new};
                        _Other ->
                            {error, eexist}
                    end
            end;
        1 ->
             {ok, exists}
    end.

%% @doc Flush the cached identity values fo the given resource (aka user) id.
-spec flush(RscId, Context) -> ok when
    RscId :: m_rsc:resource_id(),
    Context :: z:context().
flush(RscId, Context) ->
    z_depcache:flush(RscId, Context),
    z_depcache:flush({idn, RscId}, Context).


%% @doc Notify identity changes.
notify(RscId, Action, Type, Key, IsVerified, Context) ->
    flush(RscId, Context),
    Type1 = z_convert:to_binary(Type),
    z_mqtt:publish(
        [ <<"model">>, <<"identity">>, <<"event">>, RscId, Type1 ],
        #{
            id => RscId,
            type => Type
        },
        z_acl:sudo(Context)),
    z_notifier:notify(
        #identity_update_done{
            action = Action,
            rsc_id = RscId,
            type = Type1,
            key = Key,
            is_verified = IsVerified
        },
        Context).


%% @doc Ensure that the user has an associated username and password
-spec ensure_username_pw(UserId, Context) -> ok | {error, term()} when
    UserId :: m_rsc:resource(),
    Context :: z:context().
ensure_username_pw(UserId, Context) ->
    ensure_username_pw(UserId, undefined, Context).

%% @doc Ensure that the user has an associated username and password. If the username is
%% is undefined then a new username is generated using the name (or title) of the user.
-spec ensure_username_pw(UserId, Username, Context) -> ok | {error, term()} when
    UserId :: m_rsc:resource(),
    Username :: binary() | undefined,
    Context :: z:context().
ensure_username_pw(UserId, Username, Context) ->
    case m_rsc:rid(UserId, Context) of
        undefined ->
            {error, enoent};
        ?ACL_ADMIN_USER_ID ->
            % The password of the admin is set in the priv/zotonic_site.config file.
            {error, admin_password_cannot_be_set};
        RId ->
            ensure_username_pw_1(RId, Username, Context)
    end.

ensure_username_pw_1(Id, Username, Context) ->
    case z_acl:is_allowed(use, mod_admin_identity, Context) orelse z_acl:user(Context) =:= Id of
        true ->
            case z_db:q1(
                "select count(*) from identity where type = 'username_pw' and rsc_id = $1",
                [Id],
                Context
            ) of
                0 ->
                    Username1 = if
                        Username =:= undefined -> generate_username(Id, Context);
                        true -> Username
                    end,
                    Password = z_ids:password(),
                    set_username_pw(Id, Username1, Password, Context);
                _N ->
                    ok
            end;
        false ->
            {error, eacces}
    end.

%% @doc Reset the password of a user - the user will need to request a new password.
%% All authentication tokens will be reset by generating a new user secret.
-spec reset_password(UserId, Context) -> ok | {error, Reason} when
    UserId :: m_rsc:resource(),
    Context :: z:context(),
    Reason :: enoent | eacces | nouser.
reset_password(undefined, _Context) ->
    {error, enoent};
reset_password(?ACL_ADMIN_USER_ID, Context) ->
    % The password of the admin is set in the priv/zotonic_site.config file.
    ?LOG_WARNING(#{
        text => <<"Trying to reset admin password (1)">>,
        in => zotonic_core,
        user_id => z_acl:user(Context),
        result => error,
        reason => eacces
    }),
    {error, eacces};
reset_password(UserId, Context)  when is_integer(UserId) ->
    case is_allowed_set_username(UserId, Context) of
        true ->
            NewPassword = z_ids:password(),
            Hash = hash(NewPassword),
            case z_db:q("
                update identity
                set propb = $2
                where rsc_id = $1
                  and type = 'username_pw'",
                [ UserId, ?DB_PROPS(Hash) ],
                Context)
            of
                0 ->
                    ?LOG_WARNING(#{
                        in => zotonic_core,
                        text => <<"Password reset of user, but no username_pw identity">>,
                        result => error,
                        reason => nouser,
                        user => UserId
                    }),
                    {error, nouser};
                1 ->
                    reset_auth_tokens(UserId, Context),
                    notify(UserId, update, <<"username_pw">>, undefined, true, Context),
                    ?LOG_INFO(#{
                        in => zotonic_core,
                        text => <<"Password reset of user">>,
                        result => ok,
                        user => UserId
                    }),
                    ok
            end;
        false ->
            {error, eacces}
    end;
reset_password(UserId, Context) ->
    reset_password(m_rsc:rid(UserId, Context), Context).


generate_username(Id, Context) ->
    Username = base_username(Id, Context),
    username_unique(Username, Context).

username_unique(U, Context) ->
    case z_db:q1(
        "select count(*) from identity where type = 'username_pw' and key = $1",
        [U],
        Context
    ) of
        0 -> U;
        _ -> username_unique_x(U, 10, Context)
    end.

username_unique_x(U, X, Context) ->
    N = z_convert:to_binary(z_ids:number(X)),
    U1 = <<U/binary, $., N/binary>>,
    case z_db:q1(
        "select count(*) from identity where type = 'username_pw' and key = $1",
        [U1],
        Context
    ) of
        0 -> U1;
        _ -> username_unique_x(U, X * 10, Context)
    end.


base_username(Id, Context) ->
    T1 = iolist_to_binary([
        z_convert:to_binary(m_rsc:p_no_acl(Id, name_first, Context)),
        " ",
        z_convert:to_binary(m_rsc:p_no_acl(Id, name_surname, Context))
    ]),
    case nospace(z_string:trim(T1)) of
        <<>> ->
            case nospace(m_rsc:p_no_acl(Id, title, Context)) of
                <<>> -> z_ids:identifier(6);
                Title -> Title
            end;
        Name ->
            Name
    end.

nospace(undefined) ->
    <<>>;
nospace([]) ->
    <<>>;
nospace(<<>>) ->
    <<>>;
nospace(S) ->
    S1 = z_string:truncatechars(z_string:trim(S), 32, ""),
    S2 = z_string:to_slug(S1),
    nodash(binary:replace(S2, <<"-">>, <<".">>, [global])).

nodash(<<".">>) ->
    <<>>;
nodash(S) ->
    case binary:replace(S, <<"..">>, <<".">>, [global]) of
        S -> S;
        S1 -> nodash(S1)
    end.

%% @doc Return the rsc_id with the given username/password.
%% If succesful then updates the 'visited' timestamp of the entry.
-spec check_username_pw(Username, Password, Context) -> Result when
    Username :: binary() | string(),
    Password :: binary() | string(),
    Context :: z:context(),
    Result :: {ok, m_rsc:resource_id()} | {error, term()}.
check_username_pw(Username, Password, Context) ->
    check_username_pw(Username, Password, [], Context).

%% @doc Return the rsc_id with the given username/password.
%% If succesful then updates the 'visited' timestamp of the entry.
%% Uses a timer to level the time difference between existing and non existing accounts.
-spec check_username_pw(Username, Password, QueryArgs, Context) -> Result when
    Username :: binary() | string(),
    Password :: binary() | string(),
    QueryArgs :: list() | map(),
    Context :: z:context(),
    Result :: {ok, m_rsc:resource_id()} | {error, term()}.
check_username_pw(Username, Password, QueryArgs, Context) ->
    Timeout = ?DEFAULT_PW_CHECK_DURATION + z_ids:number(?DEFAULT_PW_CHECK_VARIANCE),
    Ref = erlang:make_ref(),
    erlang:send_after(Timeout, self(), {pw_done, Ref}),
    Result = check_username_pw_do(Username, Password, QueryArgs, Context),
    wait_message(Ref),
    Result.

wait_message(Ref) ->
    receive
        {pw_done, Ref} ->
            ok;
        {pw_done, _} ->
            wait_message(Ref)
    after ?DEFAULT_PW_CHECK_DURATION + ?DEFAULT_PW_CHECK_DURATION ->
        ?LOG_ERROR(#{
            text => <<"Timeout waiting for pw_done message.">>,
            in => zotonic_core,
            result => error,
            reason => timeout
        }),
        ok
    end.

%% @doc Return the rsc_id with the given username/password.
%% If succesful then updates the 'visited' timestamp of the entry.
-spec check_username_pw_do(Username, Password, QueryArgs, Context) -> Result when
    Username :: binary() | string(),
    Password :: binary() | string(),
    QueryArgs :: list() | map(),
    Context :: z:context(),
    Result :: {ok, m_rsc:resource_id()} | {error, term()}.
check_username_pw_do(Username, Password, QueryArgs, Context) when is_list(QueryArgs) ->
    check_username_pw_do(Username, Password, maps:from_list(QueryArgs), Context);
check_username_pw_do(Username, Password, QueryArgs, Context) when is_map(QueryArgs) ->
    NormalizedUsername = normalize_key(username_pw, Username),
    case z_notifier:first(#auth_precheck{ username =  NormalizedUsername }, Context) of
        Ok when Ok =:= ok; Ok =:= undefined ->
            case post_check( check_username_pw_1(NormalizedUsername, Password, Context), QueryArgs, Context ) of
                {ok, RscId} ->
                    z_notifier:notify_sync(
                        #auth_checked{
                            id = RscId,
                            username = NormalizedUsername,
                            is_accepted = true
                        },
                        Context),
                    {ok, RscId};
                {error, {expired, RscId}} ->
                    z_notifier:notify_sync(
                        #auth_checked{
                            id = RscId,
                            username = NormalizedUsername,
                            is_accepted = true
                        },
                        Context),
                    {error, {expired, RscId}};
                {error, need_passcode} = Error ->
                    Error;
                {error, set_passcode} = Error ->
                    Error;
                Error ->
                    z_notifier:notify_sync(
                        #auth_checked{
                            id = undefined,
                            username = NormalizedUsername,
                            is_accepted = false
                        },
                        Context),
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

post_check({ok, RscId}, QueryArgs, Context) ->
    case z_notifier:first(#auth_postcheck{ id = RscId, query_args = QueryArgs }, Context) of
        ok -> {ok, RscId};
        undefined -> {ok, RscId};
        Error -> Error
    end;
post_check(Error, _QueryArgs, _Context) ->
    Error.

check_username_pw_1(_Username, "", _Context) ->
    {error, password};
check_username_pw_1(_Username, <<>>, _Context) ->
    {error, password};
check_username_pw_1(<<"admin">>, Password, Context) ->
    Password1 = z_convert:to_binary(Password),
    case z_convert:to_binary( m_site:get(admin_password, Context) ) of
        <<"admin">> when Password1 =:= <<"admin">> ->
            % Only allow default password from allowed ip addresses
            case is_peer_allowed(Context) of
                true ->
                    {ok, 1};
                false ->
                    ?LOG_ERROR(#{
                        text => <<"admin login with default password from non allowed ip address">>,
                        in => zotonic_core,
                        ip_address => m_req:get(peer, Context),
                        username => <<"admin">>,
                        result => error,
                        reason => peer_not_allowed
                    }),
                    {error, peer_not_allowed}
            end;
        AdminPassword ->
            case is_equal(Password1, AdminPassword) of
                true ->
                    {ok, 1};
                false ->
                    {error, password}
            end
    end;
check_username_pw_1(Username, Password, Context) ->
    Password1 = z_convert:to_binary( Password ),
    case z_notifier:first( #auth_validate{ username = Username, password = Password1 }, Context) of
        {ok, _} = OK ->
            OK;
        {error, _} = Error ->
            Error;
        undefined ->
            Row = z_db:q_row("
                select rsc_id, propb, coalesce(expires <= now(),false) as is_expired
                from identity
                where type = 'username_pw'
                  and key = $1",
                [Username],
                Context),
            case Row of
                undefined ->
                    % If the Username looks like an e-mail address, try by Email & Password
                    case z_email_utils:is_email(Username) of
                        true -> check_email_pw(Username, Password, Context);
                        false -> {error, nouser}
                    end;
                {1, _Hash, _IsExpired} ->
                    {error, password};
                {RscId, Hash, true} ->
                    case check_hash(RscId, Username, Password, Hash, Context) of
                        {ok, UserId} ->
                            {error, {expired, UserId}};
                        {error, _} = Error ->
                            Error
                    end;
                {RscId, Hash, false} ->
                    check_hash(RscId, Username, Password, Hash, Context)
            end
    end.

%% @doc Check if the tcp/ip peer address is a allowed ip address
is_peer_allowed(Context) ->
    z_ip_address:ip_match(m_req:get(peer_ip, Context), ip_allowlist(Context)).

ip_allowlist(Context) ->
    SiteAllowlist = m_config:get_value(site, ip_allowlist, Context),
    case z_utils:is_empty(SiteAllowlist) of
        true -> z_config:get(ip_allowlist);
        false -> SiteAllowlist
    end.

%% @doc Check is the password belongs to a user with the given e-mail address.
%% Multiple users can have the same e-mail address, so multiple checks are needed.
%% If succesful then updates the 'visited' timestamp of the entry.
check_email_pw(Email, Password, Context) ->
    case lookup_by_type_and_key_multi(<<"email">>, Email, Context) of
        [] -> {error, nouser};
        Users -> check_email_pw1(Users, Email, Password, Context)
    end.

check_email_pw1([], _Email, _Password, _Context) ->
    {error, password};
check_email_pw1([Idn | Rest], Email, Password, Context) ->
    UserId = proplists:get_value(rsc_id, Idn),
    Row = z_db:q_row(
        "select rsc_id, key, propb from identity where type = 'username_pw' and rsc_id = $1",
        [UserId],
        Context
    ),
    case Row of
        undefined ->
            check_email_pw1(Rest, Email, Password, Context);
        {1, _Username, _Hash} ->
            check_email_pw1(Rest, Email, Password, Context);
        {RscId, Username, Hash} ->
            case check_hash(RscId, Username, Password, Hash, Context) of
                {ok, Id} -> {ok, Id};
                {error, password} ->
                    check_email_pw1(Rest, Email, Password, Context)
            end
    end.

%% @doc Reset the user's auth tokens - done on password reset.
%%      This invalidates all authentication cookies.
-spec reset_auth_tokens( m_rsc:resource_id(), z:context() )  -> ok.
reset_auth_tokens(UserId, Context) ->
    z_db:transaction(
        fun(Ctx) ->
            delete_by_type(UserId, auth_autologon_secret, Ctx),
            delete_by_type(UserId, auth_secret, Ctx),
            ok
        end,
        Context).


%% @doc Fetch a specific identity entry.
-spec get(IdnId, Context) -> Identity | undefined when
    IdnId :: non_neg_integer(),
    Context :: z:context(),
    Identity :: proplists:proplist().
get(IdnId, Context) ->
    z_db:assoc_row("select * from identity where id = $1", [IdnId], Context).

%% @doc Fetch all credentials belonging to the user "id"
-spec get_rsc(RscId, Context) -> Identities when
    RscId :: m_rsc:resource(),
    Context :: z:context(),
    Identities :: list( proplists:proplist() ).
get_rsc(Id, Context) ->
    z_db:assoc("select * from identity where rsc_id = $1", [m_rsc:rid(Id, Context)], Context).


%% @doc Fetch all different identity types of a user.
-spec get_rsc_types(RscId, Context) -> IdentityTypes when
    RscId :: m_rsc:resource(),
    Context :: z:context(),
    IdentityTypes :: [ binary() ].
get_rsc_types(Id, Context) ->
    Rs = z_db:q("select type from identity where rsc_id = $1", [m_rsc:rid(Id, Context)], Context),
    [R || {R} <- Rs].

%% @doc Fetch all identities belonging to the user "id" and of a certain type
-spec get_rsc_by_type(RscId, Type, Context) -> Identities when
    RscId :: m_rsc:resource(),
    Type :: type(),
    Context :: z:context(),
    Identities :: list( proplists:proplist() ).
get_rsc_by_type(Id, email, Context) ->
    get_rsc_by_type(Id, <<"email">>, Context);
get_rsc_by_type(Id, <<"email">>, Context) ->
    Idns = get_rsc_by_type_1(Id, <<"email">>, Context),
    case normalize_key(<<"email">>, m_rsc:p_no_acl(Id, email_raw, Context)) of
        undefined ->
            Idns;
        Email ->
            IsMissing = is_valid_key(<<"email">>, Email, Context)
                andalso not lists:any(fun(Idn) ->
                    proplists:get_value(key, Idn) =:= Email
                end,
                Idns),
            case IsMissing of
                true ->
                    insert(Id, <<"email">>, Email, Context),
                    get_rsc_by_type_1(Id, <<"email">>, Context);
                false ->
                    Idns
            end
    end;
get_rsc_by_type(Id, Type, Context) ->
    get_rsc_by_type_1(Id, Type, Context).

get_rsc_by_type_1(Id, Type, Context) ->
    z_db:assoc(
        "select * from identity where rsc_id = $1 and type = $2 order by is_verified desc, key asc",
        [m_rsc:rid(Id, Context), Type],
        Context
    ).

%% @doc Fetch all identities where the key equals some value.
-spec get_rsc_by_type_key(RscId, Type, Key, Context) -> Identities when
    RscId :: m_rsc:resource(),
    Type :: type(),
    Key :: key(),
    Context :: z:context(),
    Identities :: list( proplists:proplist() ).
get_rsc_by_type_key(Id, Type, Key, Context) ->
    z_db:assoc(
        "select *
         from identity
         where rsc_id = $1
           and type = $2
           and key = $3
         order by is_verified desc",
        [m_rsc:rid(Id, Context), Type, Key],
        Context).


%% @doc Fetch all identities where the key matches some prefix.
-spec get_rsc_by_type_keyprefix(RscId, Type, KeyPrefix, Context) -> Identities when
    RscId :: m_rsc:resource(),
    Type :: type(),
    KeyPrefix :: key(),
    Context :: z:context(),
    Identities :: list( proplists:proplist() ).
get_rsc_by_type_keyprefix(Id, Type, KeyPrefix, Context) ->
    z_db:assoc(
        "select *
         from identity
         where rsc_id = $1
           and type = $2
           and key like $3 || ':%'
         order by is_verified desc, key asc",
        [m_rsc:rid(Id, Context), Type, KeyPrefix],
        Context).


%% @doc Fetch an identity. Useful for fetching unique or single identities
%% of a resource.
-spec get_rsc(RscId, Type, Context) -> Identity | undefined when
    RscId :: m_rsc:resource_id(),
    Type :: type(),
    Context :: z:context(),
    Identity :: proplists:proplist().
get_rsc(Id, Type, Context) when is_integer(Id), is_atom(Type) ->
    get_rsc(Id, z_convert:to_binary(Type), Context);
get_rsc(Id, Type, Context) when is_integer(Id), is_binary(Type) ->
    F = fun() ->
        get_rsc_1(Id, Type, Context)
    end,
    z_depcache:memo(F, {idn, Id, Type}, ?IDN_CACHE_TIME, [ {idn, Id} ], Context).

get_rsc_1(Id, Type, Context) ->
    z_db:assoc_row(
        "select * from identity where rsc_id = $1 and type = $2",
        [m_rsc:rid(Id, Context), Type],
        Context
    ).


%% @doc Check if the primary email address of the user is verified.
is_email_verified(Context) ->
    is_email_verified(z_acl:user(Context), Context).

is_email_verified(UserId, Context) ->
    case m_rsc:p_no_acl(UserId, email_raw, Context) of
        undefined -> false;
        <<>> -> false;
        Email ->
            z_depcache:memo(
                fun() ->
                    E = normalize_key(<<"email">>, Email),
                    z_convert:to_bool(
                        z_db:q1("
                            select is_verified
                            from identity
                            where rsc_id = $1
                              and type = $2
                              and key = $3",
                           [UserId, <<"email">>, E],
                           Context) )
                end,
                {emaiL_verified, UserId},
                3600,
                [ UserId ],
                Context)
    end.

%% @doc Hash a password, using bcrypt
-spec hash(password()) -> bcrypt_hash().
hash(Pw) ->
    {bcrypt, erlpass:hash(Pw)}.

%% @doc Compare if a password is the same as a hash.
-spec hash_is_equal(password(), hash()) -> boolean().
hash_is_equal(Pw, {bcrypt, Hash}) ->
    erlpass:match(Pw, Hash);
hash_is_equal(Pw, {hash, Salt, Hash}) ->
    NewHash = crypto:hash(sha, [Salt, Pw]),
    is_equal(Hash, NewHash);
hash_is_equal(_, _) ->
    false.


%% @doc Check if the password hash needs to be rehashed.
-spec needs_rehash(hash()) -> boolean().
needs_rehash({bcrypt, _}) ->
    false;
needs_rehash({hash, _, _}) ->
    true.


%% @doc Create an identity record, this ensures that for this resource
%% there is only a single identity record with the given type. All existing
%% identity records with this type (for this resource) will be deleted.
-spec insert_single(m_rsc:resource(), type(), key(), z:context()) ->
    {ok, pos_integer()} | {error, invalid_key}.
insert_single(Rsc, Type, Key, Context) ->
    insert_single(Rsc, Type, Key, [], Context).

insert_single(Rsc, Type, Key, Props, Context) ->
    RscId = m_rsc:rid(Rsc, Context),
    case insert(RscId, Type, Key, Props, Context) of
        {ok, IdnId} ->
            z_db:q("
                delete from identity
                where rsc_id = $1
                  and type = $2
                  and id <> $3",
                [ RscId, Type, IdnId ],
                Context),
            flush(RscId, Context),
            {ok, IdnId};
        {error, _} = Error ->
            Error
    end.

%% @doc Create an identity record.
-spec insert(Rsc, Type, Key, Context) -> {ok, IdnId} | {error, Reason} when
    Rsc :: m_rsc:resource(),
    Type :: type(),
    Key :: key(),
    Context :: z:context(),
    IdnId :: pos_integer(),
    Reason :: invalid_key.
insert(Rsc, Type, Key, Context) ->
    insert(Rsc, Type, Key, [], Context).

%% @doc Create an identity record.
-spec insert(Rsc, Type, Key, Props, Context) -> {ok, IdnId} | {error, Reason} when
    Rsc :: m_rsc:resource(),
    Type :: type(),
    Key :: key(),
    Props :: proplists:proplist(),
    Context :: z:context(),
    IdnId :: pos_integer(),
    Reason :: invalid_key.
insert(Rsc, Type, Key, Props, Context) ->
    KeyNorm = normalize_key(Type, Key),
    case is_valid_key(Type, KeyNorm, Context) of
        true -> insert_1(Rsc, Type, KeyNorm, Props, Context);
        false -> {error, invalid_key}
    end.

insert_1(Rsc, Type, Key, Props, Context) ->
    RscId = m_rsc:rid(Rsc, Context),
    TypeB = z_convert:to_binary(Type),
    KeyB = z_convert:to_binary(Key),
    IsVerified = z_convert:to_bool(proplists:get_value(is_verified, Props)),
    case z_db:q1("select id
                  from identity
                  where rsc_id = $1
                    and type = $2
                    and key = $3",
        [RscId, TypeB, KeyB],
        Context)
    of
        undefined ->
            Props1 = [
                {rsc_id, RscId},
                {type, TypeB},
                {key, KeyB}
                | Props
            ],
            Result = z_db:insert(identity, Props1, Context),
            notify(RscId, insert, TypeB, KeyB, IsVerified, Context),
            Result;
        IdnId ->
            Props1 = case proplists:get_value(is_verified, Props, false) of
                true ->
                    [
                        {verify_key, undefined},
                        {modified, calendar:universal_time()}
                        | Props
                    ];
                false ->
                    [
                        {modified, calendar:universal_time()}
                        | Props
                    ]
            end,
            _ = z_db:update(identity, IdnId, Props1, Context),
            notify(RscId, update, TypeB, KeyB, IsVerified, Context),
            {ok, IdnId}
    end.

-spec is_valid_key( type(),  undefined | key(), z:context() ) -> boolean().
is_valid_key(_Type, undefined, _Context) -> false;
is_valid_key(email, Key, _Context) -> z_email_utils:is_email(Key);
is_valid_key(username_pw, Key, _Context) -> not is_reserved_name(Key);
is_valid_key(<<"email">>, Key, Context) -> is_valid_key(email, Key, Context);
is_valid_key(<<"username_pw">>, Key, Context) -> is_valid_key(username_pw, Key, Context);
is_valid_key(Type, _Key, _Context) when is_atom(Type); is_binary(Type) -> true.

-spec normalize_key(type(), key() | undefined) -> key() | undefined.
normalize_key(_Type, undefined) -> undefined;
normalize_key(username_pw, Key) -> z_string:trim(z_string:to_lower(unicode:characters_to_binary(Key)));
normalize_key(email, Key) -> z_string:trim(z_string:to_lower(unicode:characters_to_binary(Key)));
normalize_key("username_pw", Key) -> normalize_key(username_pw, Key);
normalize_key("email", Key) -> normalize_key(email, Key);
normalize_key(<<"username_pw">>, Key) -> normalize_key(username_pw, Key);
normalize_key(<<"email">>, Key) -> normalize_key(email, Key);
normalize_key(_Type, Key) -> Key.


%% @doc Create an unique identity record.
insert_unique(RscId, Type, Key, Context) ->
    insert(RscId, Type, Key, [{is_unique, true}], Context).
insert_unique(RscId, Type, Key, Props, Context) ->
    insert(RscId, Type, Key, [{is_unique, true} | Props], Context).


%% @doc Set the visited timestamp for the given user.
-spec set_visited(m_rsc:resource_id(), z:context()) -> ok | {error, enoent}.
set_visited(undefined, _Context) -> ok;
set_visited(UserId, Context) when is_integer(UserId) ->
    z_db:transaction(fun(Ctx) -> set_visited_trans(UserId, Ctx) end, Context);
set_visited(User, Context) ->
    set_visited(m_rsc:rid(User, Context), Context).

% Part of 'set_visited' to execute in one DB transaction
set_visited_trans(UserId, Context) when is_integer(UserId) ->
    case z_db:q1("select id from identity where rsc_id = $1 and type = 'username_pw'", [UserId], Context) of
        undefined -> {error, enoent};
        EntryId ->
            UserAgent = z_string:truncatechars(m_req:get(user_agent, Context), 240),
            IpAddress = case inet:ntoa(m_req:get(peer_ip, Context)) of
                {error, einval} -> undefined;
                IpAddressString -> z_convert:to_binary(IpAddressString)
            end,
            % Note: it may seem unnecessary to set both the 'visited' field of
            % the 'identity' table as well as adding a row to 'identity_log',
            % but the latter is periodically pruned and we still want to be able
            % to tell when was the last login of a user (if any).
            1 = z_db:q("update identity set visited = now() where id = $1", [EntryId], Context),
            1 = z_db:q(
                "insert into identity_log (identity_id, rsc_id, user_agent, ip_address)
                 values ($1,$2,$3, $4)",
                [EntryId, UserId, UserAgent, IpAddress],
                Context
            ),
            flush(UserId, Context)
    end.

-spec logon_history(m_rsc:resource_id(), z:context()) ->
        {ok, list({UserAgent, IpAddress, Created})} | {error, eacces} when
    UserAgent :: binary(),
    IpAddress :: binary(),
    Created :: calendar:datetime().
logon_history(undefined, _Context) -> [];
logon_history(UserId, Context) when is_integer(UserId) ->
    case UserId =:= z_acl:user(Context) orelse z_acl:is_admin(Context) of
        true ->
            Res = z_db:q(
                "SELECT user_agent, ip_address, created
                FROM identity_log
                WHERE rsc_id = $1
                ORDER BY created DESC",
                [UserId],
                Context
            ),
            {ok, Res};
        false ->
            {error, eacces}
    end;
logon_history(User, Context) ->
    logon_history(m_rsc:rid(User, Context), Context).

% Remove all logons from the 'identity_log' table older than 'IDN_LOG_TTL' seconds.
cleanup_logon_history(Context) ->
    z_db:q(
        "DELETE FROM identity_log WHERE created < (now() - INTERVAL '" ++
        z_convert:to_list(?IDN_LOG_TTL) ++
        " second')",
        Context
    ).


%% @doc Set the verified flag on a record by identity id.
-spec set_verified(IdnId, Context) -> ok | {error, notfound} when
    IdnId :: pos_integer(),
    Context :: z:context().
set_verified(IdnId, Context) ->
    case z_db:q_row("select rsc_id, type from identity where id = $1", [IdnId], Context) of
        {RscId, Type} ->
            case z_db:q("
                    update identity
                    set is_verified = true,
                        verify_key = null,
                        modified = now()
                    where id = $1",
                    [IdnId],
                    Context)
            of
                1 ->
                    notify(RscId, verify, Type, undefined, true, Context),
                    ok;
                0 ->
                    {error, notfound}
            end;
        undefined ->
            {error, notfound}
    end.


%% @doc Set the verified flag on a record by rescource id, identity type and
%% value (eg a user's email address).
-spec set_verified( m_rsc:resource_id(), type(), key(), z:context()) -> ok | {error, badarg}.
set_verified(RscId, Type, Key, Context)
    when is_integer(RscId),
         Type =/= undefined,
         Key =/= undefined, Key =/= <<>>, Key =/= "" ->
    KeyNorm = normalize_key(Type, Key),
    Action = z_db:transaction(fun(Ctx) -> set_verified_trans(RscId, Type, KeyNorm, Ctx) end, Context),
    notify(RscId, Action, Type, Key, true, Context),
    case Action of
        insert -> ok;
        update -> ok;
        Error -> Error
    end;
set_verified(_RscId, _Type, _Key, _Context) ->
    {error, badarg}.

set_verified_trans(RscId, Type, Key, Context) ->
    case z_db:q("update identity
                 set is_verified = true,
                     verify_key = null,
                     modified = now()
                 where rsc_id = $1
                   and type = $2
                   and key = $3",
                [RscId, Type, Key],
                Context)
    of
        0 ->
            1 = z_db:q("insert into identity (rsc_id, type, key, is_verified)
                        values ($1,$2,$3,true)",
                       [RscId, Type, Key],
                       Context),
            insert;
        N when N > 0 ->
            update
    end.

%% @doc Check if there is a verified identity for the user, beyond the username_pw
-spec is_verified( m_rsc:resource_id(), z:context() ) -> boolean().
is_verified(RscId, Context) ->
    case z_db:q1("select id from identity where rsc_id = $1 and is_verified = true and type <> 'username_pw'",
                [RscId], Context) of
        undefined -> false;
        _ -> true
    end.


%% @doc Send an email to the primary email address to verify the email address.
-spec verify_primary_email(RscId, Context) -> {ok, Status} | {error, Reason} when
    RscId :: m_rsc:resource(),
    Context :: z:context(),
    Status :: sent | verified,
    Reason :: term().
verify_primary_email(RscId, Context0) ->
    Context = z_acl:sudo(Context0),
    case m_rsc:rid(RscId, Context) of
        undefined ->
            {error, enoent};
        Id ->
            Email = normalize_key(email, m_rsc:p_no_acl(Id, <<"email_raw">>, Context)),
            Idns = get_rsc_by_type(Id, <<"email">>, Context),
            case email_find_verified(Email, Idns) of
                {true, _Idn} ->
                    {ok, verified};
                {false, Idn} ->
                    % Send the verfication e-mail
                    IdnId = proplists:get_value(id, Idn),
                    {ok, VerifyKey} = set_verify_key(IdnId, Context),
                    Vars = [
                        {idn, Idn},
                        {id, RscId},
                        {verify_key, VerifyKey}
                    ],
                    z_email:send_render(Email, "email_identity_verify.tpl", Vars, Context),
                    {ok, sent};
                none ->
                    {error, identity}
            end
    end.

email_find_verified(_Email, []) ->
    none;
email_find_verified(Email, [Idn|Idns]) ->
    case proplists:get_value(key, Idn) of
        Email ->
            {proplists:get_value(is_verified, Idn), Idn};
        _ ->
            email_find_verified(Email, Idns)
    end.


-spec set_by_type(m_rsc:resource_id(), type(), key(), z:context()) -> ok.
set_by_type(RscId, Type, Key, Context) ->
    set_by_type(RscId, Type, Key, [], Context).

-spec set_by_type(m_rsc:resource_id(), type(), key(), term(), z:context()) -> ok.
set_by_type(RscId, Type, Key, Props, Context) ->
    F = fun(Ctx) ->
        case z_db:q("
                update identity
                set key = $3,
                    propb = $4,
                    modified = now()
                where rsc_id = $1
                  and type = $2",
                [ m_rsc:rid(RscId, Context), Type, Key, ?DB_PROPS(Props) ],
                Ctx)
        of
            0 ->
                z_db:q("insert into identity (rsc_id, type, key, propb) values ($1,$2,$3,$4)",
                       [ m_rsc:rid(RscId, Context), Type, Key, ?DB_PROPS(Props) ],
                       Ctx),
                ok;
            N when N > 0 ->
                ok
        end
    end,
    z_db:transaction(F, Context).

delete(IdnId, Context) ->
    case z_db:q_row("select rsc_id, type, key from identity where id = $1", [IdnId], Context) of
        undefined ->
            {ok, 0};
        {RscId, Type, Key} ->
            case z_acl:rsc_editable(RscId, Context) of
                true ->
                    case z_db:delete(identity, IdnId, Context) of
                        {ok, 1} ->
                            notify(RscId, delete, Type, Key, undefined, Context),
                            maybe_reset_email_property(RscId, Type, Key, Context),
                            {ok, 1};
                        {ok, 0} ->
                            {ok, 0};
                        {error, _} = Error ->
                            Error
                    end;
                false ->
                    {error, eacces}
            end
    end.

%% @doc Move the identities of two resources, the identities are removed from the source id.
-spec merge(m_rsc:resource(), m_rsc:resource(), z:context()) -> ok | {error, term()}.
merge(WinnerId, LoserId, Context) ->
    case z_acl:rsc_editable(WinnerId, Context) andalso z_acl:rsc_editable(LoserId, Context) of
        true ->
            F = fun(Ctx) ->
                % Move all identities to the winner, except for duplicate type+key combinations
                LoserIdns = z_db:q("select type, key, id from identity where rsc_id = $1",
                    [m_rsc:rid(LoserId, Context)], Ctx),
                WinIdns = z_db:q("select type, key from identity where rsc_id = $1",
                    [m_rsc:rid(WinnerId, Context)], Ctx),
                AddIdns = lists:filter(
                    fun({Type, Key, _Id}) ->
                        case is_unique_identity_type(Type) of
                            true ->
                                not proplists:is_defined(Type, WinIdns);
                            false ->
                                not lists:member({Type, Key}, WinIdns)
                        end
                    end,
                    LoserIdns),
                lists:foreach(
                    fun({_Type, _Key, Id}) ->
                        z_db:q("update identity set rsc_id = $1 where id = $2",
                            [m_rsc:rid(WinnerId, Context), Id],
                            Ctx)
                    end,
                    AddIdns),
                case proplists:is_defined(<<"username_pw">>, AddIdns) of
                    true ->
                        z_db:q("update rsc set creator_id = id where id = $1 and creator_id <> id",
                            [m_rsc:rid(WinnerId, Context)], Context);
                    false ->
                        ok
                end
            end,
            z_db:transaction(F, Context),
            z_depcache:flush({idn, LoserId}, Context),
            z_depcache:flush({idn, WinnerId}, Context),
            z_mqtt:publish(
                [ <<"model">>, <<"identity">>, <<"event">>, LoserId ],
                #{
                    id => LoserId,
                    type => all
                },
                z_acl:sudo(Context)),
            z_mqtt:publish(
                [ <<"model">>, <<"identity">>, <<"event">>, WinnerId ],
                #{
                    id => WinnerId,
                    type => all
                },
                z_acl:sudo(Context)),
            ok;
        false ->
            {error, eacces}
    end.

is_unique_identity_type(<<"username_pw">>) -> true;
is_unique_identity_type(_) -> false.


%% @doc If an email identity is deleted, then ensure that the 'email' property is reset accordingly.
maybe_reset_email_property(Id, <<"email">>, Email, Context) when is_binary(Email) ->
    case normalize_key(<<"email">>, m_rsc:p_no_acl(Id, email_raw, Context)) of
        Email ->
            NewEmail = z_db:q1("
                    select key
                    from identity
                    where rsc_id = $1
                      and type = 'email'
                    order by is_verified desc, modified desc",
                [Id],
                Context),
            Context1 = z_context:set(is_m_identity_update, true, Context),
            {ok, _} = m_rsc:update(Id, [{email, NewEmail}], Context1),
            ok;
        _ ->
            ok
    end;
maybe_reset_email_property(_Id, _Type, _Key, _Context) ->
    ok.


-spec delete_by_type(m_rsc:resource(), type(), z:context()) -> ok.
delete_by_type(Rsc, Type, Context) ->
    RscId = m_rsc:rid(Rsc, Context),
    case z_db:q("delete from identity where rsc_id = $1 and type = $2", [RscId, Type], Context) of
        0 -> ok;
        _N ->
            notify(RscId, delete, Type, undefined, undefined, Context),
            ok
    end.

-spec delete_by_type_and_key(m_rsc:resource(), type(), key(), z:context()) -> ok.
delete_by_type_and_key(Rsc, Type, Key, Context) ->
    RscId = m_rsc:rid(Rsc, Context),
    case z_db:q("delete from identity where rsc_id = $1 and type = $2 and key = $3",
                [RscId, Type, Key], Context)
    of
        0 -> ok;
        _N ->
            notify(RscId, delete, Type, Key, undefined, Context),
            ok
    end.

-spec delete_by_type_and_keyprefix(m_rsc:resource(), type(), key(), z:context()) -> ok.
delete_by_type_and_keyprefix(Rsc, Type, Key, Context) ->
    RscId = m_rsc:rid(Rsc, Context),
    case z_db:q("delete from identity where rsc_id = $1 and type = $2 and key like $3 || ':%'",
                [RscId, Type, Key], Context)
    of
        0 -> ok;
        _N ->
            notify(RscId, delete, Type, Key, undefined, Context),
            ok
    end.

-spec lookup_by_username(key(), z:context()) -> identity() | undefined.
lookup_by_username(Key, Context) ->
    lookup_by_type_and_key(username_pw, Key, Context).

-spec lookup_by_type_and_key(type(), key(), z:context()) -> identity() | undefined.
lookup_by_type_and_key(Type, Key, Context) ->
    Key1 = normalize_key(Type, Key),
    z_db:assoc_row("select * from identity where type = $1 and key = $2", [Type, Key1], Context).

-spec lookup_by_type_and_key_multi(type(), key(), z:context()) -> list( identity() ).
lookup_by_type_and_key_multi(Type, Key, Context) ->
    Key1 = normalize_key(Type, Key),
    z_db:assoc("select * from identity where type = $1 and key = $2", [Type, Key1], Context).

-spec lookup_users_by_type_and_key(type(), key(), z:context()) -> list( identity() ).
lookup_users_by_type_and_key(Type, Key, Context) ->
    Key1 = normalize_key(Type, Key),
    z_db:assoc(
        "select usr.*
         from identity tp, identity usr
         where tp.rsc_id = usr.rsc_id
           and usr.type = any($3)
           and tp.type = $1
           and tp.key = $2",
        [Type, Key1, user_types(Context)],
        Context).

-spec lookup_users_by_verified_type_and_key(type(), key(), z:context()) -> list( identity() ).
lookup_users_by_verified_type_and_key(Type, Key, Context) ->
    Key1 = normalize_key(Type, Key),
    z_db:assoc(
        "select usr.*
         from identity tp, identity usr
         where tp.rsc_id = usr.rsc_id
           and usr.type = any($3)
           and tp.type = $1
           and tp.key = $2
           and tp.is_verified = true",
        [Type, Key1, user_types(Context)],
        Context).

-spec lookup_by_verify_key(key(), z:context()) -> identity() | undefined.
lookup_by_verify_key(Key, Context) ->
    z_db:assoc_row("select * from identity where verify_key = $1", [Key], Context).


-spec set_verify_key(IdnId, z:context()) -> {ok, VerifyKey} when
    IdnId :: pos_integer(),
    VerifyKey :: binary().
set_verify_key(Id, Context) ->
    VerifyKey = z_ids:id(10),
    case lookup_by_verify_key(VerifyKey, Context) of
        undefined ->
            z_db:q("update identity
                    set verify_key = $2,
                        modified = now()
                    where id = $1",
                    [Id, VerifyKey],
                    Context),
            {ok, VerifyKey};
        _ ->
            set_verify_key(Id, Context)
    end.


check_hash(RscId, Username, Password, Hash, Context) ->
    PwMatch = #identity_password_match{
        rsc_id = RscId,
        password = Password,
        hash = Hash
    },
    case z_notifier:first(PwMatch, Context) of
        {ok, rehash} ->
            %% OK but module says it needs rehashing; do that using
            %% the current hashing mechanism
            ok = set_username_pw(RscId, Username, Password, z_acl:sudo(Context)),
            {ok, RscId};
        ok ->
            {ok, RscId};
        {error, Reason} ->
            {error, Reason};
        undefined ->
            {error, nouser}
    end.

%% @doc Prevent insert of reserved usernames.
%% See: http://tools.ietf.org/html/rfc2142
%% See: https://arstechnica.com/security/2015/03/bogus-ssl-certificate
is_reserved_name(List) when is_list(List) ->
    is_reserved_name(z_convert:to_binary(List));
is_reserved_name(Name) when is_binary(Name) ->
    Name1 = normalize_key(username_pw, Name),
    is_reserved_name_1(Name1).

is_reserved_name_1(<<>>) -> true;
is_reserved_name_1(<<"admin">>) -> true;
is_reserved_name_1(<<"administrator">>) -> true;
is_reserved_name_1(<<"postmaster">>) -> true;
is_reserved_name_1(<<"hostmaster">>) -> true;
is_reserved_name_1(<<"webmaster">>) -> true;
is_reserved_name_1(<<"abuse">>) -> true;
is_reserved_name_1(<<"security">>) -> true;
is_reserved_name_1(<<"root">>) -> true;
is_reserved_name_1(<<"www">>) -> true;
is_reserved_name_1(<<"uucp">>) -> true;
is_reserved_name_1(<<"ftp">>) -> true;
is_reserved_name_1(<<"usenet">>) -> true;
is_reserved_name_1(<<"news">>) -> true;
is_reserved_name_1(<<"wwwadmin">>) -> true;
is_reserved_name_1(<<"webadmin">>) -> true;
is_reserved_name_1(<<"mail">>) -> true;
is_reserved_name_1(_) -> false.


% Constant time comparison.
-spec is_equal(Extern :: binary(), Secret :: binary()) -> boolean().
is_equal(A, B) -> is_equal(A, B, true).

is_equal(<<>>, <<>>, Eq) -> Eq;
is_equal(<<>>, _B, _Eq) -> false;
is_equal(<<_, A/binary>>, <<>>, _Eq) -> is_equal(A, <<>>, false);
is_equal(<<C, A/binary>>, <<C, B/binary>>, Eq) -> is_equal(A, B, Eq);
is_equal(<<_, A/binary>>, <<_, B/binary>>, _Eq) -> is_equal(A, B, false).
