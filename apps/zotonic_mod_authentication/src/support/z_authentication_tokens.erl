%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019-2024 Marc Worrell
%% @doc Authentication tokens and cookies.
%% @end

%% Copyright 2019-2024 Marc Worrell
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

-module(z_authentication_tokens).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    reset_cookies/1,

    req_auth_cookie/1,
    set_auth_cookie/3,
    set_auth_cookie/4,
    refresh_auth_cookie/2,
    reset_auth_cookie/1,
    ensure_auth_cookie/1,

    req_autologon_cookie/1,
    set_autologon_cookie/2,
    reset_autologon_cookie/1,

    encode_auth_token/4,
    decode_auth_token/2,

    encode_autologon_token/2,
    decode_autologon_token/2,

    encode_onetime_token/2,
    encode_onetime_token/3,
    encode_onetime_token/4,
    decode_onetime_token/2,

    session_expires/1,
    autologon_expires/1,

    regenerate_user_secret/2,
    regenerate_user_autologon_secret/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(AUTH_COOKIE, <<"z.auth">>).
-define(AUTH_SECRET_LENGTH, 32).
-define(SESSION_EXPIRE_INACTIVE, 3600*4).       % session is 4 hours valid

-define(AUTOLOGON_COOKIE, <<"z.autologon">>).
-define(AUTOLOGON_SECRET_LENGTH, 32).
-define(AUTOLOGON_EXPIRE, 3600*24*180).         % autologon is 180 days valid

-define(ONETIME_TOKEN_EXPIRE, 30).              % onetime tokens are valid for 30 secs


-spec reset_cookies( z:context() ) -> z:context().
reset_cookies(Context) ->
    z_auth:unpublish_user_session(Context),
    Context1 = reset_auth_cookie(Context),
    reset_autologon_cookie(Context1).


%% ---- Request Logon (with auth cookie)

%% @doc Check the request for an authentication cookie.
%%      If valid then logon the user in the user context.
-spec req_auth_cookie( z:context() ) -> z:context().
req_auth_cookie(Context) ->
    case z_context:get_cookie(?AUTH_COOKIE, Context) of
        undefined ->
            Context;
        AuthCookie ->
            case decode_auth_token(AuthCookie, Context) of
                {ok, {UserId, AuthOptions, Expires, ReplayToken}} ->
                    Context1 = z_acl:logon(UserId, AuthOptions, Context),
                    Context2 = z_context:set(auth_expires, Expires, Context1),
                    Context3 = z_context:set(auth_replay_token, ReplayToken, Context2),
                    z_auth:publish_user_session(Context3),
                    z_context:set(auth_options, AuthOptions, Context3);
                {error, _Reason} ->
                    reset_auth_cookie(Context)
            end
    end.

-spec set_auth_cookie(OptUserId, Options, Context) -> NewContext when
    OptUserId :: m_rsc:resource_id() | undefined,
    Options :: map(),
    Context :: z:context(),
    NewContext :: z:context().
set_auth_cookie(UserId, AuthOptions, Context) ->
    Context1 = invalidate_replay_token(Context),
    NewReplayToken = z_replay_token:new_token(),
    set_auth_cookie(UserId, AuthOptions, NewReplayToken, Context1).

-spec set_auth_cookie(OptUserId, Options, ReplayToken, Context) -> NewContext when
    OptUserId :: m_rsc:resource_id() | undefined,
    Options :: map(),
    ReplayToken :: binary() | undefined,
    Context :: z:context(),
    NewContext :: z:context().
set_auth_cookie(UserId, AuthOptions, undefined, Context) ->
    NewReplayToken = case replay_token(Context) of
        undefined -> z_replay_token:new_token();
        ReplayToken -> ReplayToken
    end,
    set_auth_cookie(UserId, AuthOptions, NewReplayToken, Context);
set_auth_cookie(UserId, AuthOptions, ReplayToken, Context) ->
    % Invalidate optional existing replay token, if it is different from the
    % replay token of the new auth cookie.
    case replay_token(Context) of
        undefined -> ok;
        ReplayToken -> ok;
        OtherReplayToken ->
            z_replay_token:invalidate_token(OtherReplayToken, session_expires(Context), Context)
    end,
    Cookie = encode_auth_token(UserId, AuthOptions, ReplayToken, Context),
    CookieOptions = [
        {path, <<"/">>},
        {http_only, true},
        {secure, true},
        {same_site, strict}
    ],
    Context1 = z_context:set_cookie(?AUTH_COOKIE, Cookie, CookieOptions, Context),
    Context2 = z_context:set(auth_expires, session_expires(Context1), Context1),
    Context3 = z_context:set(auth_replay_token, ReplayToken, Context2),
    z_context:set(auth_options, AuthOptions, Context3).

-spec refresh_auth_cookie( map(), z:context() ) -> z:context().
refresh_auth_cookie(RequestOptions, Context) ->
    case z_context:get_cookie(?AUTH_COOKIE, Context) of
        undefined when RequestOptions =:= #{} ->
            z_acl:logoff(Context);
        undefined ->
            NewAuthOptions = merge_options(RequestOptions, #{}, Context),
            set_auth_cookie(undefined, NewAuthOptions, z_replay_token:new_token(), Context);
        AuthCookie ->
            UserId = z_acl:user(Context),
            case decode_auth_token(AuthCookie, Context) of
                {ok, {UserId, AuthOptions, _Expires, ReplayToken}} ->
                    NewAuthOptions = merge_options(RequestOptions, AuthOptions, Context),
                    set_auth_cookie(UserId, NewAuthOptions, ReplayToken, Context);
                {error, _} ->
                    reset_auth_cookie(Context),
                    z_acl:logoff(Context)
            end
    end.

-spec merge_options( map(), map(), z:context() ) -> map().
merge_options(RequestOptions, AuthOptions, Context) ->
    z_notifier:foldl(
        #auth_options_update{
            request_options = RequestOptions
        },
        AuthOptions,
        Context).


-spec reset_auth_cookie( z:context() ) -> z:context().
reset_auth_cookie(Context) ->
    Context1 = invalidate_replay_token(Context),
    set_auth_cookie(undefined, #{}, z_replay_token:new_token(), Context1).

-spec ensure_auth_cookie( z:context() ) -> z:context().
ensure_auth_cookie(Context) ->
    case z_context:get_cookie(?AUTH_COOKIE, Context) of
        undefined ->
            AuthOptions = z_context:get(auth_options, Context, #{}),
            set_auth_cookie(undefined, AuthOptions, z_replay_token:new_token(), Context);
        _Cookie ->
            Context
    end.

-spec encode_auth_token(OptUserId, Options, ReplayToken, Context) -> AuthToken when
    OptUserId :: m_rsc:resource_id() | undefined,
    Options :: map(),
    ReplayToken :: binary(),
    Context :: z:context(),
    AuthToken :: binary().
encode_auth_token(UserId, Options, ReplayToken, Context) ->
    Term = {auth, UserId, Options, user_secret(UserId, Context), ReplayToken},
    ExpTerm = termit:expiring(Term, session_expires(Context)),
    termit:encode_base64(ExpTerm, auth_secret(Context)).

-spec decode_auth_token( binary(), z:context() ) ->
     {ok, {OptUserId, Options, ExpiresAt, ReplayToken}} | {error, Reason} when
   OptUserId :: m_rsc:resource_id() | undefined,
   Options :: map(),
   ExpiresAt :: integer(),
   ReplayToken :: binary(),
   Reason :: term().
decode_auth_token(AuthCookie, Context) ->
    case termit:decode_base64(AuthCookie, auth_secret(Context)) of
        {ok, ExpTerm} ->
            case termit:check_expired(ExpTerm) of
                {ok, {auth, UserId, Options, UserSecret, ReplayToken}} when is_binary(UserSecret) ->
                    case z_replay_token:is_spent_token(ReplayToken, Context) of
                        false ->
                            case user_secret(UserId, Context) of
                                UserSecret ->
                                    {ok, {UserId, Options, extract_expires(ExpTerm), ReplayToken}};
                                _ ->
                                    {error, user_secret}
                            end;
                        true ->
                            {error, replay}
                    end;
                {ok, _} ->
                    % Illegal or too old token - skip
                    {error, token};
                {error, _} = Error ->
                    Error
            end;
        {error, _}  = Error ->
            Error
    end.

extract_expires({expires, ExpiresAt, _Data}) ->
    ExpiresAt - z_datetime:timestamp().


%% @doc Register the auth cookie's replay token as invalidated for the coming period.
%% The z.auth cookie must be available in the request context.
-spec invalidate_replay_token(Context) -> NewContext when
    Context :: z:context(),
    NewContext :: z:context().
invalidate_replay_token(Context) ->
    case replay_token(Context) of
        undefined ->
            Context;
        Token ->
            z_replay_token:invalidate_token(Token, session_expires(Context), Context),
            z_context:set(replay_token, undefined, Context)
    end.

replay_token(Context) ->
    case z_context:get(auth_replay_cookie, Context) of
        undefined ->
            case z_context:get_cookie(?AUTH_COOKIE, Context) of
                undefined ->
                    undefined;
                AuthCookie ->
                    case termit:decode_base64(AuthCookie, auth_secret(Context)) of
                        {ok, {expires, _Time, Auth}} ->
                            case Auth of
                                {auth, _UserId, _Options, _UserSecret, ReplayToken} ->
                                    ReplayToken;
                                _ ->
                                    undefined
                            end;
                        _ ->
                            undefined
                    end
            end;
        Token when is_binary(Token) ->
            Token
    end.


%% ---- Automatic Logon (with autologon cookie)

%% @doc Check the request for an automatic logon cookie.
%%      If valid then logon the user in the user context and set an auth cookie.
-spec req_autologon_cookie( z:context() ) -> z:context().
req_autologon_cookie(Context) ->
    case z_context:get_cookie(?AUTOLOGON_COOKIE, Context) of
        undefined ->
            Context;
        AutoLogonCookie ->
            case decode_autologon_token(AutoLogonCookie, Context) of
                {ok, UserId} ->
                    case z_auth:logon(UserId, Context) of
                        {ok, Context1} ->
                            ReplayToken = z_replay_token:new_token(),
                            Options = #{
                                auth_method => <<"autologon_cookie">>
                            },
                            Context2 = set_auth_cookie(UserId, Options, ReplayToken, Context1),
                            z_context:set(auth_is_autologon, true, Context2);
                        {error, _} ->
                            reset_autologon_cookie(Context)
                    end;
                {error, _} ->
                    reset_autologon_cookie(Context)
            end
    end.

%% @doc Set a cookie for automatic logon of the user-agent.
-spec set_autologon_cookie( m_rsc:resource_id(), z:context() ) -> z:context().
set_autologon_cookie(UserId, Context) ->
    Cookie = encode_autologon_token(UserId, Context),
    CookieOptions = [
        {max_age, autologon_expires(Context)},
        {path, <<"/">>},
        {http_only, true},
        {secure, true},
        {same_site, strict}
    ],
    z_context:set_cookie(?AUTOLOGON_COOKIE, Cookie, CookieOptions, Context).

-spec reset_autologon_cookie( z:context() ) -> z:context().
reset_autologon_cookie(Context) ->
    CookieOptions = [
        {max_age, 0},
        {path, <<"/">>},
        {http_only, true},
        {secure, true},
        {same_site, strict}
    ],
    z_context:set_cookie(?AUTOLOGON_COOKIE, <<>>, CookieOptions, Context).

-spec encode_autologon_token( m_rsc:resource_id(), z:context() ) -> binary().
encode_autologon_token(UserId, Context) ->
    Term = {autologon, UserId, user_secret(UserId, Context), user_autologon_secret(UserId, Context)},
    ExpTerm = termit:expiring(Term, autologon_expires(Context)),
    termit:encode_base64(ExpTerm, autologon_secret(Context)).

-spec decode_autologon_token( binary(), z:context() ) -> {ok, m_rsc:resource_id()} | {error, term()}.
decode_autologon_token(AutoLogonCookie, Context) ->
    case termit:decode_base64(AutoLogonCookie, autologon_secret(Context)) of
        {ok, ExpTerm} ->
            case termit:check_expired(ExpTerm) of
                {ok, {autologon, UserId, UserSecret, AutoLogonSecret}} ->
                    US = user_secret(UserId, Context),
                    AS = user_autologon_secret(UserId, Context),
                    case {US, AS} of
                        {UserSecret, AutoLogonSecret} when is_binary(UserSecret), is_binary(AutoLogonSecret) ->
                            {ok, UserId};
                        _ ->
                            {error, user_secret}
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _}  = Error ->
            Error
    end.


-spec encode_onetime_token( m_rsc:resource_id(), z:context() ) -> {ok, binary()} | {error, no_session}.
encode_onetime_token(UserId, Context) ->
    case z_context:session_id(Context) of
        {ok, SId} ->
            encode_onetime_token(UserId, SId, Context);
        {error, _} = Error ->
            Error
    end.

-spec encode_onetime_token( m_rsc:resource_id(), binary() | undefined, z:context() ) -> {ok, binary()} | {error, no_session}.
encode_onetime_token(UserId, SId, Context) ->
    encode_onetime_token(UserId, SId, #{}, Context).

-spec encode_onetime_token(UserId, SId, AuthOptions, Context) -> {ok, OnetimeToken} | {error, no_session} when
    UserId :: m_rsc:resource_id(),
    SId :: binary() | undefined,
    AuthOptions :: map(),
    Context :: z:context(),
    OnetimeToken :: binary().
encode_onetime_token(_UserId, undefined, _AuthOptions, _Context) ->
    {error, no_session};
encode_onetime_token(UserId, SId, AuthOptions, Context) ->
    Term = {onetime, UserId, user_secret(UserId, Context), user_autologon_secret(UserId, Context), SId, AuthOptions},
    ExpTerm = termit:expiring(Term, ?ONETIME_TOKEN_EXPIRE),
    {ok, termit:encode_base64(ExpTerm, autologon_secret(Context))}.

-spec decode_onetime_token(OnetimeToken, Context) -> {ok, {UserId, AuthOptions}} | {error, Reason} when
    OnetimeToken :: binary(),
    Context :: z:context(),
    UserId :: m_rsc:resource_id(),
    AuthOptions :: map(),
    Reason :: term().
decode_onetime_token(OnetimeToken, Context) ->
    case termit:decode_base64(OnetimeToken, autologon_secret(Context)) of
        {ok, ExpTerm} ->
            {ok, SId} = z_context:session_id(Context),
            case termit:check_expired(ExpTerm) of
                {ok, {onetime, UserId, UserSecret, AutoLogonSecret, SId, AuthOptions}} ->
                    US = user_secret(UserId, Context),
                    AS = user_autologon_secret(UserId, Context),
                    case {US, AS} of
                        {UserSecret, AutoLogonSecret} when is_binary(UserSecret), is_binary(AutoLogonSecret) ->
                            {ok, {UserId, AuthOptions}};
                        _ ->
                            {error, user_secret}
                    end;
                {ok, Unexpected} ->
                    ?LOG_ERROR(#{
                        text => <<"authentication: token mismatch from sid">>,
                        in => zotonic_mod_authentication,
                        result => error,
                        reason => token_mismatch,
                        sid => SId,
                        unexpected => Unexpected
                    }),
                    {error, mismatch};
                {error, Reason} = Error ->
                    ?LOG_ERROR(#{
                        text => <<"authentication: token expired error">>,
                        in => zotonic_mod_authentication,
                        result => error,
                        reason => Reason
                    }),
                    Error
            end;
        {error, Reason}  = Error ->
            ?LOG_WARNING(#{
                text => <<"authentication: token decode error">>,
                in => zotonic_mod_authentication,
                result => error,
                reason => Reason
            }),
            Error
    end.


%% ---- Secrets, stored in site config and in user identities.

-spec auth_secret( z:context() ) -> binary().
auth_secret(Context) ->
    case m_config:get_value(mod_authentication, auth_secret, Context) of
        <<>> -> generate_auth_secret(Context);
        undefined -> generate_auth_secret(Context);
        Secret -> Secret
    end.

-spec generate_auth_secret( z:context() ) -> binary().
generate_auth_secret(Context) ->
    Secret = z_ids:id(?AUTH_SECRET_LENGTH),
    m_config:set_value(mod_authentication, auth_secret, Secret, Context),
    Secret.

-spec user_secret( m_rsc:resource_id() | undefined, z:context() ) -> binary() | error.
user_secret(undefined, Context) ->
    case m_config:get_value(mod_authentication, auth_anon_secret, Context) of
        <<>> -> generate_auth_anon_secret(Context);
        undefined -> generate_auth_anon_secret(Context);
        Secret -> Secret
    end;
user_secret(UserId, Context) ->
    user_secret_1(z_db:has_connection(Context), UserId, Context).

user_secret_1(false, 1, Context) ->
    case m_config:get_value(mod_authentication, auth_user_secret, Context) of
        undefined ->
            Secret = z_ids:id(?AUTH_SECRET_LENGTH),
            m_config:set_value(mod_authentication, auth_user_secret, Secret, Context),
            Secret;
        Secret ->
            Secret
    end;
user_secret_1(true, UserId, Context) ->
    case m_rsc:exists(UserId, Context) of
        true ->
            case m_identity:get_rsc(UserId, auth_secret, Context) of
                undefined -> generate_user_secret(UserId, Context);
                Idn -> proplists:get_value(prop1, Idn)
            end;
        false ->
            error
    end.

-spec generate_auth_anon_secret( z:context() ) -> binary().
generate_auth_anon_secret(Context) ->
    Secret = z_ids:id(?AUTH_SECRET_LENGTH),
    m_config:set_value(mod_authentication, auth_anon_secret, Secret, Context),
    Secret.

-spec generate_user_secret( m_rsc:resource_id(), z:context() ) -> binary().
generate_user_secret(UserId, Context) ->
    Secret = z_ids:id(?AUTH_SECRET_LENGTH),
    {ok, _} = m_identity:insert_single(UserId, auth_secret, <<>>, [{prop1, Secret}], Context),
    Secret.

-spec autologon_secret( z:context() ) -> binary().
autologon_secret(Context) ->
    case m_config:get_value(mod_authentication, auth_autologon_secret, Context) of
        <<>> -> generate_autologon_secret(Context);
        undefined -> generate_autologon_secret(Context);
        Secret -> Secret
    end.

-spec generate_autologon_secret( z:context() ) -> binary().
generate_autologon_secret(Context) ->
    Secret = z_ids:id(?AUTOLOGON_SECRET_LENGTH),
    m_config:set_value(mod_authentication, auth_autologon_secret, Secret, Context),
    Secret.

-spec user_autologon_secret( m_rsc:resource_id(), z:context() ) -> binary() | error.
user_autologon_secret(UserId, Context) ->
    case m_rsc:exists(UserId, Context) of
        true ->
            case m_identity:get_rsc(UserId, auth_autologon_secret, Context) of
                undefined -> generate_user_autologon_secret(UserId, Context);
                Idn -> proplists:get_value(prop1, Idn)
            end;
        false ->
            error
    end.

-spec generate_user_autologon_secret( m_rsc:resource_id(), z:context() ) -> binary().
generate_user_autologon_secret(UserId, Context) ->
    Secret = z_ids:id(?AUTOLOGON_SECRET_LENGTH),
    {ok, _} = m_identity:insert_single(UserId, auth_autologon_secret, <<>>, [{prop1, Secret}], Context),
    Secret.

% @doc Generate a new user secret and replace the old one (if any).
-spec regenerate_user_secret( m_rsc:resource_id(), z:context() ) -> ok | {error, enoent}.
regenerate_user_secret(undefined, _Context) -> ok;
regenerate_user_secret(UserId, Context) when is_integer(UserId) ->
    case z_db:has_connection(Context) of
        false ->
            ok;
        true ->
            case m_rsc:exists(UserId, Context) of
                true ->
                    generate_user_secret(UserId, Context),
                    ok;
                false ->
                    {error, enoent}
            end
    end.

% @doc Generate a new user autologon secret and replace the old one (if any).
-spec regenerate_user_autologon_secret( m_rsc:resource_id(), z:context() ) -> ok | {error, enoent}.
regenerate_user_autologon_secret(undefined, _Context) -> ok;
regenerate_user_autologon_secret(UserId, Context) when is_integer(UserId) ->
    case z_db:has_connection(Context) of
        false ->
            ok;
        true ->
            case m_rsc:exists(UserId, Context) of
                true ->
                    generate_user_autologon_secret(UserId, Context),
                    ok;
                false ->
                    {error, enoent}
            end
    end.

%% ---- Expirations

%% @doc Return the number of seconds an inactive session cookie stays valid.
-spec session_expires( z:context() ) -> integer().
session_expires(Context) ->
    z_convert:to_integer( m_config:get_value(site, session_expire_inactive, ?SESSION_EXPIRE_INACTIVE, Context) ).

%% @doc Return the number of seconds an autologon cookie is valid for.
-spec autologon_expires( z:context() ) -> integer().
autologon_expires(Context) ->
    z_convert:to_integer( m_config:get_value(site, autologon_expire, ?AUTOLOGON_EXPIRE, Context) ).
