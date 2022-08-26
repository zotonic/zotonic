%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019-2021 Marc Worrell
%% @doc Registry for authentication tokens giving access to users/resources on
%% the local site.

%% Copyright 2019-2021 Marc Worrell
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

-module(m_oauth2).

-behaviour(zotonic_model).

-include_lib("kernel/include/logger.hrl").

-export([
    m_get/3,

    client/3,

    encode_accept_code/4,
    exchange_accept_code/5,

    list_apps/1,
    get_app/2,
    insert_app/2,
    delete_app/2,
    update_app/3,
    update_app_secret/2,

    user_groups/1,
    list_tokens/2,
    get_token/2,

    insert_token/4,
    delete_token/2,

    encode_bearer_token/3,
    decode_bearer_token/2,

    get_token_access/2,
    oauth_key/1,

    is_equal/2,

    manage_schema/2
]).

-define(TOKEN_PREFIX, "oauth2-").
-define(APP_KEYLEN, 24).
-define(ACCEPT_CODE_TTL, 12000).
-define(BEARER_TOKEN_TTL, undefined).


m_get([ <<"client">>, ClientId, RedirectURL | Rest ], _Msg, Context) ->
    case client(ClientId, RedirectURL, Context) of
        {ok, App} ->
            {ok, {App, Rest}};
        {error, _} = Error ->
            Error
    end;
m_get([ <<"apps">> ], _Msg, Context) ->
    case list_apps(Context) of
        {ok, Apps} ->
            {ok, {Apps, []}};
        {error, _} = Error ->
            Error
    end;
m_get([ <<"apps">>, AppId | Rest ], _Msg, Context) ->
    case get_app(z_convert:to_integer(AppId), Context) of
        {ok, App} ->
            {ok, {App, Rest}};
        {error, _} = Error ->
            Error
    end;
m_get([ <<"user_groups">> | Rest ], _Msg, Context) ->
    {ok, {user_groups(Context), Rest}};
m_get([ <<"tokens">> ], Msg, Context) ->
    m_get([ <<"tokens">>, <<"list">>, <<"me">> ], Msg, Context);
m_get([ <<"tokens">>, <<"list">> ], Msg, Context) ->
    m_get([ <<"tokens">>, <<"list">>, <<"me">> ], Msg, Context);
m_get([ <<"tokens">>, <<"list">>, <<"me">> | Rest ], _Msg, Context) ->
    case list_tokens(z_acl:user(Context), Context) of
        {ok, List} -> {ok, {List, Rest}};
        {error, _} = Error -> Error
    end;
m_get([ <<"tokens">>, <<"list">>, UserId | Rest ], _Msg, Context) ->
    case list_tokens(m_rsc:rid(UserId, Context), Context) of
        {ok, List} -> {ok, {List, Rest}};
        {error, _} = Error -> Error
    end;
m_get([ <<"tokens">>, TokenId | Rest ], _Msg, Context) ->
    {ok, {get_token(TokenId, Context), Rest}};
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.



%% @doc Fetch the basic client details to show during OAuth authorization. The passed
%% redirect URL must have an exact match with the registered redirect URLs.
-spec client( ClientId :: binary(), RedirectURL :: binary(), z:context() ) ->
    {ok, map()} | {error, term()}.
client(ClientId, RedirectURL, Context) ->
    case z_utils:only_digits(ClientId) of
        true ->
            AppId = binary_to_integer(ClientId),
            case z_db:qmap_row("
                select id, redirect_urls, description
                from oauth2_app
                where id = $1
                  and is_enabled
                ",
                [ AppId ],
                Context)
            of
                {ok, #{ <<"redirect_urls">> := Allowed } = App} ->
                    case is_allowed_redirect_uri(RedirectURL, Allowed, Context) of
                        true ->
                            {ok, maps:without([ <<"redirect_urls">> ], App)};
                        false ->
                            {error, enoent}
                    end;
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, enoent}
    end.

is_allowed_redirect_uri(RedirectURL, Allowed, Context) ->
    [ URL1 | _ ] = binary:split(RedirectURL, [ <<"?">>, <<"#">> ]),
    AllowedUrls = binary:split(Allowed, [ <<10>>, <<13>> ], [ global, trim_all ]),
    Expanded = expand_domain_names(AllowedUrls, Context),
    lists:member(URL1, Expanded).

expand_domain_names(Names, Context) ->
    lists:map(
        fun
            (<<"https://", _/binary>> = Url) -> Url;
            (<<"http://", _/binary>> = Url) -> Url;
            (Domain) ->
                ContextNoLang = z_context:set_language('x-default', Context),
                iolist_to_binary([
                    <<"https://">>,
                    Domain,
                    z_dispatcher:url_for(oauth2_service_redirect, [], ContextNoLang)
                ])
        end,
        Names).

%% @doc Encode a 'code' for the authorization accepted redirect back to the requesting
%% party. This code will be decoded when the requesting party requests their access token.
%% The code validity must be limited to a very short period (seconds) as it will be
%% handled by the calling party.
-spec encode_accept_code( ClientId :: integer(), RedirectURL :: binary(), Scope :: binary(), z:context()) ->
    {ok, Code :: binary()} | {error, term()}.
encode_accept_code(ClientId, RedirectURL, Scope, Context) ->
    case z_db:qmap_row("
        select id, redirect_urls, app_sign_secret
        from oauth2_app
        where id = $1
          and is_enabled
        ",
        [ ClientId ],
        Context)
    of
        {ok, #{
                <<"redirect_urls">> := Allowed,
                <<"app_sign_secret">> := AppSignSecret
            }} ->
            case is_allowed_redirect_uri(RedirectURL, Allowed, Context) of
                true ->
                    SystemKey = oauth_key(Context),
                    Salt = z_ids:id(8),
                    SignKey = <<
                        Salt/binary, $:,
                        AppSignSecret/binary, $:,
                        SystemKey/binary
                        >>,
                    Term = #{
                        client_id => ClientId,
                        user_id => z_acl:user(Context),
                        redirect_uri => RedirectURL,
                        scope => Scope
                    },
                    Encoded = issue_token(Term, SignKey, ?ACCEPT_CODE_TTL),
                    {ok, <<Salt/binary, $-, Encoded/binary>>};
                false ->
                    {error, redirect_uri}
            end;
        {error, _} = Error ->
            Error
    end.



%% @doc Exchange an authorization 'code' for for an access token. Called by the endpoint
%% that exchanges the code for an access token to the user's account.
-spec exchange_accept_code(
        ClientId :: binary(),
        ClientSecret :: binary(),
        RedirectURL :: binary(),
        Code :: binary(),
        z:context()) ->
    {ok, {Code :: binary(), UserId :: m_rsc:resource_id()}} | {error, term()}.
exchange_accept_code(ClientIdBin, ClientSecret, RedirectURL, Code, Context) ->
    ClientId = try
        binary_to_integer(ClientIdBin)
    catch
        error:badarg -> 0
    end,
    case binary:split(Code, <<"-">>) of
        [ Salt, Encoded ] ->
            case z_db:qmap_row("
                select id, redirect_urls, app_sign_secret, app_secret
                from oauth2_app
                where id = $1
                  and is_enabled
                ",
                [ ClientId ],
                Context)
            of
                {ok, #{
                        <<"redirect_urls">> := _Allowed,
                        <<"app_sign_secret">> := AppSignSecret,
                        <<"app_secret">> := AppSecret
                    }} ->
                    case is_equal(ClientSecret, AppSecret) of
                        true ->
                            SystemKey = oauth_key(Context),
                            SignKey = <<
                                Salt/binary, $:,
                                AppSignSecret/binary, $:,
                                SystemKey/binary
                                >>,
                            case termit:verify_token(Encoded, SignKey) of
                                {ok, #{
                                        client_id := ReqClientId,
                                        user_id := UserId,
                                        redirect_uri := ReqRedirectURL,
                                        scope := _Scope
                                    }} when RedirectURL =:= ReqRedirectURL,
                                            ClientId =:= ReqClientId ->
                                    ContextSudo = z_acl:sudo(Context),
                                    {ok, TokenId} = insert_token(ClientId, UserId, #{}, ContextSudo),
                                    case encode_bearer_token(TokenId, ?BEARER_TOKEN_TTL, ContextSudo) of
                                        {ok, Token} ->
                                            {ok, {Token, UserId}};
                                        {error, _} = Error ->
                                            Error
                                    end;
                                {ok, _} ->
                                    {error, mismatch};
                                {error, _Reason} = Error ->
                                    Error
                            end;
                        false ->
                            {error, secret}
                    end;
                {error, _} = Error ->
                    Error
            end;
        [_] ->
            {error, code}
    end.


%% @doc Insert a new App. The map can contain the user_id, is_enabled flag
%% and the App description.
-spec insert_app( AppDetails :: map(), z:context() ) -> {ok, AppId :: integer()} | {error, term()}.
insert_app(Map, Context) ->
    case z_acl:is_admin_editable(Context) of
        true ->
            App = #{
                <<"user_id">> => maps:get(<<"user_id">>, Map, z_acl:user(Context)),
                <<"is_enabled">> => maps:get(<<"is_enabled">>, Map, true),
                <<"description">> => maps:get(<<"description">>, Map, <<"Untitled">>),
                <<"redirect_urls">> => maps:get(<<"redirect_urls">>, Map, <<>>),
                <<"app_secret">> => z_ids:id(?APP_KEYLEN),
                <<"app_sign_secret">> => z_ids:id(?APP_KEYLEN)
            },
            z_db:insert(oauth2_app, App, Context);
        false ->
            {error, eacces}
    end.

%% @doc Delete an App. All associated tokens are deleted as well.
-spec delete_app( AppId :: integer(), z:context() ) -> ok | {error, term()}.
delete_app(AppId, Context) ->
    case z_acl:is_admin_editable(Context) of
        true ->
            case z_db:delete(oauth2_app, AppId, Context) of
                {ok, 1} -> ok;
                {ok, 0} -> {error, enoent};
                {error, _} = Error -> Error
            end;
        false ->
            {error, eacces}
    end.

%% @doc Update an App's is_enabled flag and description.
-spec update_app( AppId :: integer(), map(), z:context() ) -> ok | {error, term()}.
update_app(AppId, Map, Context) ->
    case z_acl:is_admin_editable(Context) of
        true ->
            App = #{
                <<"is_enabled">> => maps:get(<<"is_enabled">>, Map, true),
                <<"description">> => maps:get(<<"description">>, Map, <<"Untitled">>),
                <<"redirect_urls">> => maps:get(<<"redirect_urls">>, Map, <<>>),
                <<"modified">> => calendar:universal_time()
            },
            case z_db:update(oauth2_app, AppId, App, Context) of
                {ok, 1} -> ok;
                {ok, 0} -> {error, enoent};
                {error, _} = Error -> Error
            end;
        false ->
            {error, eacces}
    end.

%% @doc Regenerate the secret and sign key of an App. All outstanding tokens are deleted.
-spec update_app_secret( AppId :: integer(), z:context() ) -> {ok, binary()}| {error, term()}.
update_app_secret(AppId, Context) ->
    case z_acl:is_admin_editable(Context) of
        true ->
            Secret = z_ids:id(?APP_KEYLEN),
            App = #{
                <<"app_secret">> => Secret,
                <<"app_sign_secret">> => z_ids:id(?APP_KEYLEN),
                <<"modified">> => calendar:universal_time()
            },
            z_db:q("delete from oauth2_token where app_id = $1", [ AppId ], Context),
            case z_db:update(oauth2_app, AppId, App, Context) of
                {ok, 1} -> {ok, Secret};
                {ok, 0} -> {error, enoent};
                {error, _} = Error -> Error
            end;
        false ->
            {error, eacces}
    end.


%% @doc List all apps, the secrets of these apps must be copied to the consumer site.
%% Tokens are always coupled to an app. Apps are coupled to an user, if the user
%% is deleted then all their apps and tokens are deleted. Apps should be registered by
%% an user with config rights, as such all admin users can see all apps.
-spec list_apps( z:context() ) -> {ok, list( map() )} | {error, eacces | term()}.
list_apps(Context) ->
    case z_acl:is_admin(Context) of
        true ->
            z_db:qmap("
                select a.id, a.is_enabled, a.user_id,
                       a.description, a.created, a.modified,
                       (select count(*) from oauth2_token t where a.id = t.app_id) as token_count
                from oauth2_app a
                order by created desc",
                Context);
        false ->
            {error, eacces}
    end.

%% @doc Get a specific app, return also the token count for the app.
-spec get_app( AppId :: integer(), z:context() ) -> {ok, map()} | {error, eacces | term()}.
get_app(AppId, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            z_db:qmap_row("
                select a.id, a.is_enabled, a.user_id, a.app_secret,
                       a.description, a.redirect_urls, a.created, a.modified,
                       (select count(*) from oauth2_token t where a.id = t.app_id) as token_count
                from oauth2_app a
                where a.id = $1",
                [ AppId ],
                Context);
        false ->
            {error, eacces}
    end.


%% @doc Return all user groups defined in the ACL.
-spec user_groups( z:context() ) -> {ok, [ m_rsc:resource_id() ]}.
user_groups(Context) ->
    {ok, z_acl:user_groups(Context)}.


%% @doc List all tokens for a certain user. The user must have access to the tokens.
-spec list_tokens( UserId :: m_rsc:resource_id() | undefined, z:context() ) -> {ok, [ map() ]} | {error, term()}.
list_tokens(UserId, Context) when is_integer(UserId) ->
    case is_allowed(UserId, Context) of
        true ->
            z_db:qmap("
                select t.id, t.user_id, t.is_read_only, t.ip_allowed, t.note,
                       t.valid_till, t.created, t.modified,
                       a.id as app_id, a.is_enabled as is_app_enabled,
                       a.description as app_description
                from oauth2_token t
                    left join oauth2_app a
                    on a.id = t.app_id
                where user_id = $1
                order by created desc",
                [ UserId ],
                [ {keys, binary} ],
                Context);
        false ->
            {error, eacces}
    end;
list_tokens(undefined, _Context) ->
    {ok, []}.


%% @doc Insert a secret for a token into the token table. The token itself is encoded
%% using the encode_bearer_token/3 function. This replaces the existing token for the
%% user/app combination.
-spec insert_token( AppId :: integer(), UserId :: m_rsc:resource_id(), TokenProps :: map(), z:context() ) ->
    {ok, TokenId :: integer()} | {error, term()}.
insert_token( AppId, UserId, Props, Context ) when is_map(Props), is_integer(AppId) ->
    case is_allowed(UserId, Context) and not z_acl:is_read_only(Context) of
        true ->
            z_db:transaction(
                fun(Ctx) ->
                    insert_trans(AppId, UserId, Props, Ctx)
                end,
                Context);
        false ->
            {error, eacces}
    end.

insert_trans(AppId, UserId, Props, Context) ->
    z_db:q("
        delete from oauth2_token
        where user_id = $1
          and app_id = $2
        ",
        [ UserId, AppId ],
        Context),
    Secret = z_ids:id(32),
    Props1 = #{
        <<"app_id">> => AppId,
        <<"user_id">> => UserId,
        <<"is_read_only">> => z_convert:to_bool( maps:get(<<"is_read_only">>, Props, true) ),
        <<"is_full_access">> => z_convert:to_bool( maps:get(<<"is_full_access">>, Props, false) ),
        <<"ip_allowed">> => z_string:trim( z_convert:to_binary( maps:get(<<"ip_allowed">>, Props, <<"*">>) ) ),
        <<"note">> => z_html:escape( z_string:trim( maps:get(<<"note">>, Props, <<>>) ) ),
        <<"valid_till">> => maps:get(<<"valid_till">>, Props, undefined),
        <<"secret">> => Secret
    },
    case z_db:insert(oauth2_token, Props1, Context) of
        {ok, TokenId} ->
            Groups = maps:get(<<"user_groups">>, Props, []),
            Groups1 = [ m_rsc:rid(GId, Context) || GId <- Groups ],
            Groups2 = filter_groups(Groups1, Context),
            lists:foreach(
                fun(GId) ->
                    z_db:q("
                        insert into oauth2_token_group (token_id, group_id)
                        values ($1, $2)",
                        [ TokenId, GId ],
                        Context)
                end,
                Groups2),
            {ok, TokenId};
        {error, _} = Error ->
            Error
    end.


-spec delete_token( TokenId :: integer(), z:context() ) -> ok | {error, enoent | eacces}.
delete_token( TokenId, Context ) when is_integer(TokenId) ->
    case z_db:q1("select user_id from oauth2_token where id = $1", [ TokenId ], Context) of
        undefined ->
            {error, enoent};
        UserId ->
            case is_allowed(UserId, Context) and not z_acl:is_read_only(Context) of
                true ->
                    z_db:q("delete from oauth2_token where id = $1", [ TokenId ], Context),
                    ok;
                false ->
                    {error, eacces}
            end
    end.

%% @doc Return the Bearer OAuth2 token for the given token-id. The token is encoded using the
%% the token secret, the app sign secret and the oauth2 system secret. An optional time-to-live
%% (in seconds) is added.
-spec encode_bearer_token( TokenId :: integer(), TTL :: undefined | integer(), z:context() ) ->
    {ok, binary()} | {error, enoent | eacces}.
encode_bearer_token( TokenId, TTL, Context ) ->
    case z_db:qmap_row("
        select t.user_id, t.secret, app.app_sign_secret
        from oauth2_token t
            join oauth2_app app on t.app_id = app.id
        where t.id = $1
          and (t.valid_till is null or t.valid_till > now())
          and app.is_enabled",
        [ z_convert:to_integer(TokenId) ],
        Context)
     of
        {ok, #{
                <<"user_id">> := UserId,
                <<"secret">> := TokenSecret,
                <<"app_sign_secret">> := AppSignSecret
            }} ->
            case is_allowed(UserId, Context) of
                true ->
                    SystemKey = oauth_key(Context),
                    SignKey = <<
                        TokenSecret/binary, $:,
                        AppSignSecret/binary, $:,
                        SystemKey/binary
                        >>,
                    Term = {v1, UserId},
                    Encoded = issue_token(Term, SignKey, TTL),
                    TokenIdBin = integer_to_binary(TokenId),
                    {ok, <<?TOKEN_PREFIX, TokenIdBin/binary, $-, Encoded/binary>>};
                false ->
                    {error, eacces}
            end;
        {error, _} = Error ->
            Error
    end.


%5 @doc Issue token with optional expiration.
issue_token(Term, Key, undefined) ->
    termit:issue_token(Term, Key);
issue_token(Term, Key, TTL) when is_integer(TTL) ->
    termit:issue_token(Term, Key, TTL).

%% @doc Decode an incoming Bearer token. All spaces should have been removed.
% The token is checked using the user_id, the token secret, the app sign secret, the
% oauth2 system secret and the TTL of the token.
-spec decode_bearer_token(binary(), z:context()) ->
    {ok, map()} | {error, unknown_token | expired | forged | badarg}.
decode_bearer_token(<<?TOKEN_PREFIX, Token/binary>>, Context) ->
    case binary:split(Token, <<"-">>) of
        [ TokenIdBin, Encoded ] ->
            try
                TokenId = binary_to_integer(TokenIdBin),
                case get_token_access(TokenId, Context) of
                    {ok, #{
                            <<"user_id">> := TokenUserId,
                            <<"secret">> := TokenSecret,
                            <<"app_sign_secret">> := AppSignSecret
                        } = TokenMap} ->
                        SystemKey = oauth_key(Context),
                        SignKey = <<
                            TokenSecret/binary, $:,
                            AppSignSecret/binary, $:,
                            SystemKey/binary
                            >>,
                        case termit:verify_token(Encoded, SignKey) of
                            {ok, {v1, UserId}} when UserId =:= TokenUserId ->
                                {ok, TokenMap};
                            {ok, _} ->
                                {error, forged};
                            {error, _Reason} = Error ->
                                Error
                        end;
                    {error, _} ->
                        % Assume token was known, but has been replaced with
                        % a new token.
                        {error, expired}
                end
            catch
                error:badarg ->
                    % The token id is not an integer
                    {error, unknown_token}
            end;
        _ ->
            % Wrong format
            {error, unknown_token}
    end;
decode_bearer_token(_Token, _Context) ->
    {error, unknown_token}.

-spec get_token( integer() | undefined, z:context() ) -> {ok, map()} | {error, enoent | eacces | term()}.
get_token( TokenId, Context ) when is_integer(TokenId) ->
    case z_db:qmap_row("
        select t.id, t.app_id, t.user_id, t.is_read_only, t.is_full_access,
               t.ip_allowed, t.note, t.created, t.modified,
               t.app_id, app.is_enabled as app_is_enabled, app.description as app_description
        from oauth2_token t
            join oauth2_app app on t.app_id = app.id
        where t.id = $1",
        [ z_convert:to_integer(TokenId) ],
        [ {keys, binary} ],
        Context)
    of
        {ok, #{ <<"user_id">> := UserId, <<"is_full_access">> := IsFulllAccess } = Token} ->
            case is_allowed(UserId, Context) of
                true when IsFulllAccess ->
                    % Full access token, no user group restrictions
                    {ok, Token};
                true ->
                    Groups = z_db:q("
                            select group_id
                            from oauth2_token_group
                            where token_id = $1",
                            [ TokenId ],
                            Context),
                    Groups1 = [ Id || {Id} <- Groups ],
                    {ok, Token#{
                        <<"user_groups">> => Groups1
                    }};
                false ->
                    {error, eacces}
            end;
        {error, _} = Error ->
            Error
    end;
get_token( undefined, _Context ) ->
    {error, enoent}.


%% @doc Get the token details for authentication checks and token usage.
%% @todo Cache this for speeding up api requests
-spec get_token_access( integer(), z:context() ) -> {ok, map()} | {error, enoent}.
get_token_access(TokenId, Context) when is_integer(TokenId) ->
    case z_db:qmap_row("
        select t.id, t.user_id, t.is_read_only, t.is_full_access, t.ip_allowed, t.secret,
               t.app_id, app.app_sign_secret
        from oauth2_token t
            join oauth2_app app on t.app_id = app.id
        where t.id = $1
          and (t.valid_till is null or t.valid_till > now())
          and app.is_enabled",
        [ z_convert:to_integer(TokenId) ],
        Context)
    of
        {ok, Token} ->
            Groups = z_db:q("
                    select group_id
                    from oauth2_token_group
                    where token_id = $1",
                    [ TokenId ],
                    Context),
            Groups1 = [ Id || {Id} <- Groups ],
            {ok, Token#{
                <<"user_groups">> => Groups1
            }};
        {error, enoent} ->
            {error, enoent}
    end.


%% @doc Check if the current user is allowed to see the tokens of the given user.
-spec is_allowed( m_rsc:resource_id(), z:context() ) -> boolean().
is_allowed(UserId, Context) when is_integer(UserId) ->
    z_acl:user(Context) =:= UserId orelse z_acl:is_admin(Context).

%% @doc Filter the user groups for access
-spec filter_groups( [ m_rsc:resource_id() ], z:context() ) -> [ m_rsc:resource_id() ].
filter_groups(GIds, Context) when is_list(GIds) ->
    case z_acl:is_admin(Context) of
        true ->
            GIds;
        false ->
            {ok, UserGroups} = user_groups(Context),
            lists:filter(
                fun(GId) -> lists:member(GId, UserGroups) end,
                GIds)
    end.

%% @doc Return the secret site key used for symmetrically encrypting OAuth2 tokens.
-spec oauth_key( z:context() ) -> binary().
oauth_key(Context) ->
    case m_config:get_value(mod_oauth2, oauth_key, Context) of
        undefined ->
            Key = z_ids:id(64),
            m_config:set_value(mod_oauth2, oauth_key, Key, Context),
            Key;
        SignKey ->
            SignKey
    end.


% Constant time comparison.
-spec is_equal(Extern :: binary(), Secret :: binary() ) -> boolean().
is_equal(A, B) -> is_equal(A, B, true).

is_equal(<<>>, <<>>, Eq) -> Eq;
is_equal(<<>>, _B, _Eq) -> false;
is_equal(<<_, A/binary>>, <<>>, _Eq) -> is_equal(A, <<>>, false);
is_equal(<<C, A/binary>>, <<C, B/binary>>, Eq) -> is_equal(A, B, Eq);
is_equal(<<_, A/binary>>, <<_, B/binary>>, _Eq) -> is_equal(A, B, false).



-spec manage_schema( z_module_manager:manage_schema(), z:context() ) -> ok.
manage_schema(_Version,  Context) ->
    % All tokens and their signature secret
    case z_db:table_exists(oauth2_token, Context) of
        false ->
            install_oauth2_app_table(Context),
            [] = z_db:q("
                CREATE TABLE oauth2_token (
                    id serial not null,
                    user_id int not null,
                    app_id int not null,
                    is_read_only boolean not null default true,
                    is_full_access boolean not null default false,
                    secret character varying(64) not null,
                    ip_allowed character varying(500) not null default '*',
                    valid_till timestamp with time zone,
                    note text,
                    created timestamp with time zone NOT NULL DEFAULT now(),
                    modified timestamp with time zone NOT NULL DEFAULT now(),

                    primary key (id),
                    constraint oauth2_token_user_id_app_id_key UNIQUE (user_id, app_id),

                    constraint fk_oauth2_token_user_id foreign key (user_id)
                        references rsc(id)
                        on update cascade
                        on delete cascade,

                    constraint fk_oauth2_token_app_id foreign key (app_id)
                        references oauth2_app(id)
                        on update cascade
                        on delete cascade
                )",
                Context),
            [] = z_db:q(
                "CREATE INDEX fki_oauth2_token_user_id ON oauth2_token (user_id)",
                Context),

            [] = z_db:q(
                "CREATE INDEX fki_oauth2_token_app_id ON oauth2_token (app_id)",
                Context),

            % ACL group resources the token gives access to
            [] = z_db:q("
                CREATE TABLE oauth2_token_group (
                    token_id int not null,
                    group_id int not null,

                    primary key (token_id, group_id),

                    constraint fk_oauth2_token_group_token_id foreign key (token_id)
                        references oauth2_token(id)
                        on update cascade
                        on delete cascade,
                    constraint fk_oauth2_token_group_group_id foreign key (group_id)
                        references rsc(id)
                        on update cascade
                        on delete cascade
                )",
                Context),
            [] = z_db:q(
                "CREATE INDEX fki_oauth2_token_group_token_id ON oauth2_token_group (token_id)",
                Context),
            [] = z_db:q(
                "CREATE INDEX fki_oauth2_token_group_group_id ON oauth2_token_group (group_id)",
                Context),

            z_db:flush(Context);
        true ->
            ok = install_oauth2_app(Context)
    end.

install_oauth2_app(Context) ->
    case z_db:table_exists(oauth2_app, Context) of
        false ->
            install_oauth2_app_table(Context),
            [] = z_db:q("
                alter table oauth2_token
                add column app_id int not null,
                add constraint oauth2_token_user_id_app_id_key UNIQUE (user_id, app_id),
                add constraint fk_oauth2_token_app_id foreign key (app_id)
                        references oauth2_app(id)
                        on update cascade
                        on delete cascade
                ",
                Context),
            [] = z_db:q(
                "CREATE INDEX fki_oauth2_token_app_id ON oauth2_token (app_id)",
                Context),
            z_db:q("drop table if exists oauth2_token_log cascade", Context),
            z_db:flush(Context);
        true ->
            case z_db:column_exists(oauth2_app, redirect_urls, Context) of
                true ->
                    ok;
                false ->
                    [] = z_db:q("alter table oauth2_app add column redirect_urls text not null default ''", Context),
                    z_db:flush(Context)
            end,
            ok
    end.


install_oauth2_app_table(Context) ->
    [] = z_db:q("
        CREATE TABLE oauth2_app (
            id serial not null,
            is_enabled boolean not null default true,
            user_id int not null,
            app_secret varchar(32),
            app_sign_secret varchar(32),
            description varchar(255),
            redirect_urls text not null default '',
            created timestamp with time zone not null default now(),
            modified timestamp with time zone not null default now(),

            primary key (id),

            constraint fk_oauth2_app_user_id foreign key (user_id)
                references rsc(id)
                on update cascade
                on delete cascade
        )",
        Context).
