%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc Authentication tokens and cookies.

%% Copyright 2019 Marc Worrell
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
    refresh_auth_cookie/2,
    reset_auth_cookie/1,

    req_autologon_cookie/1,
    set_autologon_cookie/2,
    reset_autologon_cookie/1,

    encode_auth_token/3,
    decode_auth_token/2,

    encode_autologon_token/2,
    decode_autologon_token/2,

    session_expires/1,
    autologon_expires/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(AUTH_COOKIE, <<"z.auth">>).
-define(AUTH_SECRET_LENGTH, 32).
-define(SESSION_EXPIRE_INACTIVE, 3600*4).       % session is 4 hours valid

-define(AUTOLOGON_COOKIE, <<"z.autologon">>).
-define(AUTOLOGON_SECRET_LENGTH, 32).
-define(AUTOLOGON_EXPIRE, 3600*8*180).          % autologon is 180 days valid


-spec reset_cookies( z:context() ) -> z:context().
reset_cookies(Context) ->
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
                {ok, {UserId, AuthOptions, Expires}} ->
                    Context1 = z_acl:logon(UserId, AuthOptions, Context),
                    Context2 = z_context:set(auth_expires, Expires, Context1),
                    z_context:set(auth_options, AuthOptions, Context2);
                {error, _} ->
                    reset_auth_cookie(Context)
            end
    end.

-spec set_auth_cookie( m_rsc:resource_id(), map(), z:context() ) -> z:context().
set_auth_cookie(UserId, AuthOptions, Context) ->
    Cookie = encode_auth_token(UserId, AuthOptions, Context),
    CookieOptions = [
        {path, <<"/">>},
        {http_only, true},
        {secure, true},
        {same_site, strict}
    ],
    Context1 = z_context:set_cookie(?AUTH_COOKIE, Cookie, CookieOptions, Context),
    Context2 = z_context:set(auth_expires, session_expires(Context1), Context1),
    z_context:set(auth_options, AuthOptions, Context2).


-spec refresh_auth_cookie( map(), z:context() ) -> z:context().
refresh_auth_cookie(RequestOptions, Context) ->
    case z_context:get_cookie(?AUTH_COOKIE, Context) of
        undefined when RequestOptions =:= #{} ->
            z_acl:logoff(Context);
        undefined when RequestOptions =/= #{} ->
            NewAuthOptions = merge_options(RequestOptions, #{}, Context),
            set_auth_cookie(undefined, NewAuthOptions, Context);
        AuthCookie ->
            UserId = z_acl:user(Context),
            case decode_auth_token(AuthCookie, Context) of
                {ok, {UserId, AuthOptions, _Expires}} ->
                    NewAuthOptions = merge_options(RequestOptions, AuthOptions, Context),
                    set_auth_cookie(UserId, NewAuthOptions, Context);
                {error, _} ->
                    reset_auth_cookie(Context),
                    z_acl:logoff(Context)
            end
    end.

merge_options(RequestOptions, AuthOptions, Context) ->
    z_notifier:foldl(
        #auth_options_update{
            request_options = RequestOptions
        },
        AuthOptions,
        Context).


-spec reset_auth_cookie( z:context() ) -> z:context().
reset_auth_cookie(Context) ->
    CookieOptions = [
        {max_age, 0},
        {path, <<"/">>},
        {http_only, true},
        {secure, true},
        {same_site, strict}
    ],
    z_context:set_cookie(?AUTH_COOKIE, <<>>, CookieOptions, Context).

-spec encode_auth_token( m_rsc:resource_id(), map(), z:context() ) -> binary().
encode_auth_token(UserId, Options, Context) ->
    Term = {auth, UserId, Options, user_secret(UserId, Context)},
    ExpTerm = termit:expiring(Term, session_expires(Context)),
    termit:encode_base64(ExpTerm, auth_secret(Context)).

-spec decode_auth_token( binary(), z:context() ) -> {ok, {m_rsc:resource_id(), map(), integer()}} | {error, term()}.
decode_auth_token(AuthCookie, Context) ->
    case termit:decode_base64(AuthCookie, auth_secret(Context)) of
        {ok, ExpTerm} ->
            case termit:check_expired(ExpTerm) of
                {ok, {auth, UserId, Options, UserSecret}} ->
                    case user_secret(UserId, Context) of
                        UserSecret when is_binary(UserSecret) ->
                            {ok, {UserId, Options, extract_expires(ExpTerm)}};
                        _ ->
                            {error, user_secret}
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _}  = Error ->
            Error
    end.

extract_expires({expires, ExpiresAt, _Data}) ->
    ExpiresAt - z_datetime:timestamp().


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
                            Context2 = set_auth_cookie(UserId, #{}, Context1),
                            z_context:set(auth_is_autologon, true, Context2);
                        {error, _} ->
                            reset_autologon_cookie(Context)
                    end;
                {error, _} ->
                    reset_autologon_cookie(Context)
            end
    end.

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

-spec user_secret( m_rsc:resource_id(), z:context() ) -> binary().
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
    case m_identity:get_rsc(UserId, auth_secret, Context) of
        undefined -> generate_user_secret(UserId, Context);
        Idn -> proplists:get_value(prop1, Idn)
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

-spec user_autologon_secret( m_rsc:resource_id(), z:context() ) -> binary().
user_autologon_secret(UserId, Context) ->
    case m_identity:get_rsc(UserId, auth_autologon_secret, Context) of
        undefined -> generate_user_autologon_secret(UserId, Context);
        Idn -> proplists:get_value(prop1, Idn)
    end.

-spec generate_user_autologon_secret( m_rsc:resource_id(), z:context() ) -> binary().
generate_user_autologon_secret(UserId, Context) ->
    Secret = z_ids:id(?AUTOLOGON_SECRET_LENGTH),
    {ok, _} = m_identity:insert_single(UserId, auth_autologon_secret, <<>>, [{prop1, Secret}], Context),
    Secret.

%% ---- Expirations

-spec session_expires( z:context() ) -> integer().
session_expires(Context) ->
    z_convert:to_integer( m_config:get_value(site, session_expire_inactive, ?SESSION_EXPIRE_INACTIVE, Context) ).

-spec autologon_expires( z:context() ) -> integer().
autologon_expires(Context) ->
    z_convert:to_integer( m_config:get_value(site, autologon_expire, ?AUTOLOGON_EXPIRE, Context) ).
