%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017-2024 Marc Worrell
%% @doc Model for mod_authentication
%% @end

%% Copyright 2017-2024 Marc Worrell
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

-module(m_authentication).

-behaviour(zotonic_model).

-export([
    m_get/3,
    m_post/3,

    acceptable_password/2,
    is_valid_password/2,
    is_powned/1,

    send_reminder/2,
    set_reminder_secret/2,

    auth_token/2,
    cookie_token/2,

    site_auth_key/1,
    user_auth_key/2,

    decode_token/2,

    auth_tokens/2,
    cookie_url/1,

    handle_auth_confirm/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(DEFAULT_PASSWORD_MIN_LENGTH, 8).

-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"authenticate">>, <<"password">> | Rest ], #{ payload := Payload }, Context) when is_map(Payload) ->
    case auth_tokens(Payload, Context) of
        {ok, Tk} -> {ok, {Tk, Rest}};
        {error, _} = Error -> Error
    end;
m_get([ <<"password_min_length">> | Rest ], _Msg, Context) ->
    Len = case m_config:get_value(mod_authentication, password_min_length, Context) of
        undefined -> ?DEFAULT_PASSWORD_MIN_LENGTH;
        <<>> -> ?DEFAULT_PASSWORD_MIN_LENGTH;
        N -> z_convert:to_integer(N)
    end,
    {ok, {Len, Rest}};
m_get([ <<"is_one_step_logon">> | Rest ], _Msg, Context) ->
    IsOneStep = m_config:get_boolean(mod_authentication, is_one_step_logon, Context),
    {ok, {IsOneStep, Rest}};
m_get([ <<"is_supported">>, <<"rememberme">> | Rest ], _Msg, Context) ->
    IsSupported = z_db:has_connection(Context),
    {ok, {IsSupported, Rest}};
m_get([ <<"status">> | Rest ], _Msg, Context) ->
    % Status similar to the one returned by controller_authentication
    Status = #{
        <<"status">> => <<"ok">>,
        <<"is_authenticated">> => z_auth:is_auth(Context),
        <<"user_id">> => z_acl:user(Context),
        <<"username">> => m_identity:get_username(Context),
        <<"preferences">> => #{
            <<"language">> => z_context:language(Context),
            <<"timezone">> => z_context:tz(Context)
        },
        <<"options">> => z_context:get(auth_options, Context, #{})
    },
    {ok, {Status, Rest}};
m_get([ <<"is_rememberme">> | Rest ], _Msg, Context) ->
    RememberMe = m_config:get_boolean(mod_authentication, is_rememberme, Context),
    {ok, {RememberMe, Rest}};
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.

-spec m_post( list( binary() ), zotonic_model:opt_msg(), z:context() ) -> {ok, term()} | {error, term()}.
m_post([ <<"request-reminder">> ], #{ payload := Payload }, Context) when is_map(Payload) ->
    request_reminder(Payload, Context);
m_post([ <<"service-confirm">> ], #{ payload := Payload }, Context) when is_map(Payload) ->
    case maps:get(<<"value">>, Payload, undefined) of
        #{ <<"auth">> := AuthEncoded, <<"url">> := Url } ->
            UrlSafe = z_sanitize:uri(Url),
            Secret = z_context:state_cookie_secret(Context),
            case termit:decode_base64(AuthEncoded, Secret) of
                {ok, AuthExp} ->
                    case termit:check_expired(AuthExp) of
                        {ok, Auth} ->
                            handle_auth_confirm(Auth, UrlSafe, Context);
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} ->
                    {error, illegal_auth}
            end;
        _ ->
            {error, missing_auth}
    end;
m_post([ <<"service-confirm-passcode">> ], #{ payload := Payload }, Context) when is_map(Payload) ->
    case maps:get(<<"value">>, Payload, undefined) of
        #{ <<"authuser">> := AuthUserEncoded, <<"url">> := Url, <<"passcode">> := Passcode } ->
            UrlSafe = z_sanitize:uri(Url),
            Secret = z_context:state_cookie_secret(Context),
            case termit:decode_base64(AuthUserEncoded, Secret) of
                {ok, AuthExp} ->
                    case termit:check_expired(AuthExp) of
                        {ok, #{ auth := Auth, user_id := UserId }} ->
                            handle_auth_confirm_passcode(UserId, Passcode, Auth, UrlSafe, Context);
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} ->
                    {error, illegal_auth}
            end;
        _ ->
            {error, missing_auth}
    end;
m_post([ <<"send-verification-message">> ], #{ payload := Payload }, Context) when is_map(Payload) ->
    case Payload of
        #{ <<"token">> := Token } ->
            case catch z_crypto:depickle(Token, Context) of
                #{ timestamp := {{_,_,_}, {_,_,_}}=Ts,
                   user_id := UserId } ->
                    case z_datetime:next_hour(Ts) > calendar:universal_time() of
                        true ->
                            case z_notifier:first(#identity_verification{user_id=UserId}, Context) of
                                ok -> {ok, verification_sent};
                                {error, _} ->
                                    %% Hide the real error
                                    {error, error_sending_verification}
                            end;
                        false -> {error, expired_token}
                    end;
                _ ->
                    {error, invalid_token}
            end;
        _ -> {error, missing_token}
    end;
m_post([ <<"acceptable-password">> ], #{ payload := Payload }, Context) when is_map(Payload) ->
    case Payload of
        #{ <<"password">> := Password } when is_binary(Password) ->
            acceptable_password(Password, Context);
        _ ->
            {error, missing_password}
    end;
m_post(Vs, _Msg, _Context) ->
    ?LOG_INFO("Unknown ~p post: ~p", [?MODULE, Vs]),
    {error, unknown_path}.

%% @doc Check if the password matches the criteria of this site.
-spec acceptable_password(Password, Context) -> ok | {error, Reason} when
    Password :: binary(),
    Context :: z:context(),
    Reason :: tooshort | dataleak.
acceptable_password(Password, Context) ->
    case is_valid_password(Password, Context) of
        true ->
            case not m_config:get_boolean(mod_authentication, password_disable_leak_check, Context)
                andalso m_authentication:is_powned(Password)
            of
                true ->
                    {error, dataleak};
                false ->
                    ok
            end;
        false ->
            {error, tooshort}
    end.


%% @doc Check if the password matches the criteria of the minimum length
%% and the (optional) password regexp.
is_valid_password(Password, Context) ->
    PasswordMinLength = z_convert:to_integer(
        m_config:get_value(mod_authentication, password_min_length, ?DEFAULT_PASSWORD_MIN_LENGTH, Context)),
    if
        size(Password) < PasswordMinLength ->
            false;
        true ->
            case z_convert:to_binary(m_config:get_value(mod_admin_identity, password_regex, Context)) of
                <<>> ->
                    true;
                RegExp ->
                    case re:run(Password, RegExp) of
                        nomatch -> false;
                        {match, _} -> true
                    end
            end
    end.

%% @doc Check is a password has been registerd with the service at https://haveibeenpwned.com
%% They keep a list of passwords, any match is reported.
is_powned(Password) ->
    <<Pre:5/binary, Post/binary>>  = z_string:to_upper(z_utils:hex_sha(Password)),
    Url = <<"https://api.pwnedpasswords.com/range/", Pre/binary>>,
    case z_url_fetch:fetch(Url, []) of
        {ok, {_Url, _Hs, _Sz, Body}} ->
            case binary:match(Body, <<Post/binary, ":">>) of
                {_, _} -> true;
                nomatch -> false
            end;
        {error, {404, _Url, _Hs, _Sz, _Body}} ->
            false;
        {error, {Code, _Url, _Hs, _Sz, _Body}} ->
            {error, Code};
        {error, _} = Error ->
            Error
    end.

handle_auth_confirm(Auth, Url, Context) ->
    Auth1 = Auth#auth_validated{ is_signup_confirmed = true },
    case z_notifier:first(Auth1, Context) of
        undefined ->
            ?LOG_WARNING(#{
                text => <<"mod_authentication: 'undefined' return for auth">>,
                in => zotonic_mod_authentication,
                result => error,
                reason => no_auth,
                auth => Auth
            }),
            {error, nohandler};
        {ok, UserId} ->
            case z_authentication_tokens:encode_onetime_token(UserId, Context) of
                {ok, Token} ->
                    {ok, #{
                        result => token,
                        token => Token,
                        url => Url
                    }};
                {error, Reason} = Err ->
                    ?LOG_WARNING(#{
                        text => <<"mod_authentication: error return for auth">>,
                        in => zotonic_mod_authentication,
                        result => error,
                        reason => Reason,
                        auth => Auth
                    }),
                    Err
            end;
        {error, Reason} ->
            ?LOG_WARNING(#{
                text => <<"mod_authentication: Error return for auth">>,
                in => zotonic_mod_authentication,
                result => error,
                reason => Reason,
                auth => Auth
            }),
            {error, signup}
    end.

%% @doc Check if the passcode is correct. Use auth_precheck and auth_checked to
%% handle rate limiting.
handle_auth_confirm_passcode(UserId, Passcode, Auth, Url, Context) ->
    Username = z_convert:to_binary(m_identity:get_username(UserId, Context)),
    case auth_precheck(Username, Context) of
        ok ->
            PostCheck = #auth_postcheck{
                id = UserId,
                query_args = #{ <<"passcode">> => Passcode }
            },
            case z_notifier:first(PostCheck, Context) of
                undefined ->
                    auth_checked(Username, true, Context),
                    handle_auth_confirm(Auth, Url, Context);
                {error, _} = Error ->
                    auth_checked(Username, false, Context),
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

auth_precheck(Username, Context) when is_binary(Username) ->
    case z_notifier:first(#auth_precheck{ username = Username }, Context) of
        undefined -> ok;
        ok -> ok;
        Error -> Error
    end.

auth_checked(Username, IsAccepted, Context) when is_binary(Username) ->
    z_notifier:first(#auth_checked{ username = Username, is_accepted = IsAccepted }, Context).


%% @doc Send password reminders to everybody with the given email address in one of their
%% registered email-identities. The email is sent to the primary email address of the user.
-spec request_reminder( map(), z:context() ) -> {ok, map()} | {error, email}.
request_reminder(Payload, Context) ->
    case maps:find(<<"email">>, Payload) of
        {ok, Email} when is_binary(Email) ->
            EmailNorm = m_identity:normalize_key(email, Email),
            case z_email_utils:is_email(EmailNorm) of
                true ->
                    case z_notifier:first( #auth_reset{ username = EmailNorm }, Context) of
                        Ok when Ok =:= undefined; Ok =:= ok ->
                            case lookup_email_identities(EmailNorm, Context) of
                                [] ->
                                    case m_config:get_boolean(mod_authentication, email_reminder_if_nomatch, Context) of
                                        true ->
                                            send_reminder(undefined, EmailNorm, Context);
                                        false ->
                                            nop
                                    end;
                                UserIds ->
                                    lists:foreach(
                                        fun(RscId) ->
                                            send_reminder(RscId, EmailNorm, Context)
                                        end,
                                        UserIds)
                            end,
                            {ok, #{ email => EmailNorm }};
                        {error, _Reason} = Error ->
                            Error
                    end;
                false ->
                    {error, email}
            end;
        _Other ->
            {error, email}
    end.

%% @doc Find all users with a certain email address or username identity. The email
%% address is already normalized.
-spec lookup_email_identities(EmailOrUsername, Context) -> UserIds when
    EmailOrUsername :: binary(),
    Context :: z:context(),
    UserIds :: [ m_rsc:resource_id() ].
lookup_email_identities(<<>>, _Context) ->
    [];
lookup_email_identities(EmailOrUsername, Context) ->
    Es = m_identity:lookup_by_type_and_key_multi(email, EmailOrUsername, Context),
    Us = m_identity:lookup_by_type_and_key_multi(username_pw, EmailOrUsername, Context),
    RscIds = lists:usort([ proplists:get_value(rsc_id, Row) || Row <- Es ++ Us ]),
    lists:filter(
        fun (RscId) ->
            case m_identity:get_username(RscId, Context) of
                undefined -> false;
                <<"admin">> -> false;
                _ -> true
            end
        end,
        RscIds).


%% @doc Email a reset code to the user.
send_reminder(Id, Context) ->
    Email = m_rsc:p_no_acl(Id, email_raw, Context),
    send_reminder(Id, Email, Context).

send_reminder(_Id, undefined, _Context) ->
    {error, noemail};
send_reminder(1, _Email, _Context) ->
    ?LOG_INFO(#{
        text => <<"Ignoring password reminder request for 'admin' (user 1)">>,
        in => zotonic_mod_authentication,
        user_id => 1,
        username => <<"admin">>
    }),
    {error, admin};
send_reminder(undefined, Email, Context) ->
    send_password_reset_email(Email, <<>>, undefined, [], Context);
send_reminder(Id, Email, Context) ->
    PrefEmail = case m_rsc:p_no_acl(Id, email_raw, Context) of
        undefined -> Email;
        <<>> -> Email;
        E -> m_identity:normalize_key(email, E)
    end,
    case m_identity:get_username(Id, Context) of
        undefined ->
            send_reminder(undefined, Email, Context);
        Username when Username =/= <<"admin">> ->
            Vars = [
                {recipient_id, Id},
                {id, Id},
                {secret, set_reminder_secret(Id, Context)},
                {username, Username},
                {email, PrefEmail}
            ],
            ContextUser = z_acl:logon(Id, Context),
            send_password_reset_email(Email, Username, Id, Vars, ContextUser),
            case Email of
                PrefEmail -> ok;
                _ ->
                    send_password_reset_email(PrefEmail, Username, Id, Vars, ContextUser)
            end
    end.

%% Send and log sending the password reset email
send_password_reset_email(Email, Username, Id, Vars, Context) ->
    z:info(
      "Sending password reset email for user ~s(~p) to ~s", [Username, Id, Email],
      [ {module, ?MODULE} ],
      Context),
    z_email:send_render(Email, "email_password_reset.tpl", Vars, Context).

%% @doc Set the unique reminder code for the account.
-spec set_reminder_secret( m_rsc:resource_id(), z:context() ) -> binary().
set_reminder_secret(Id, Context) ->
    Code = z_ids:id(24),
    ok = m_identity:set_by_type(Id, <<"logon_reminder_secret">>, Code, Context),
    Code.


%% @doc If authentication was possible, then return auth tokens and cookie url.
-spec auth_tokens( map(), z:context() ) -> {ok, map()} | {error, term()}.
auth_tokens( #{ <<"username">> := Username, <<"password">> := Password } = QArgs, Context) ->
    case m_identity:check_username_pw(Username, Password, QArgs, Context) of
        {ok, UserId} ->
            UserSecret = user_auth_key(UserId, Context),
            {ok, #{
                <<"auth">> => #{
                    <<"token">> => auth_token(UserId, UserSecret, Context)
                },
                <<"cookie">> => #{
                    <<"token">> => cookie_token(UserId, UserSecret, Context),
                    <<"url">> => cookie_url(Context)
                }
            }};
        {error, _} = Error ->
            Error
    end.


%% @doc Fetch the secret user key. This key is used to add an extra hash of the tokens.
-spec user_auth_key( m_rsc:resource_id(), z:context() ) -> binary().
user_auth_key(UserId, Context) ->
    case m_identity:get_rsc(UserId, auth_key, Context) of
        undefined ->
            Key = z_ids:id(64),
            Props = [
                {prop1, Key},
                {is_verified, true}
            ],
            {ok, _} = m_identity:insert(UserId, auth_key, <<>>, Props, Context),
            Key;
        Idn when is_list(Idn) ->
            {prop1, Key} = proplists:lookup(prop1, Idn),
            Key
    end.

%% @doc Return the secret site key used for symmetrically encrypting tokens.
-spec site_auth_key( z:context() ) -> binary().
site_auth_key(Context) ->
    case m_config:get_value(mod_authentication, site_auth_key, Context) of
        undefined ->
            Key = z_ids:id(64),
            m_config:set_value(mod_authentication, site_auth_key, Key, Context),
            Key;
        SignKey ->
            SignKey
    end.

%% @doc Decode a token, check the hashes.
-spec decode_token( binary(), z:context() ) -> {ok, {cookie|auth, UserId :: m_rsc:resource_id(), Timestamp::pos_integer()}} | {error, illegal}.
decode_token(Token, Context) ->
    try
        <<1, Hash:32/binary, Payload/binary>> = base64:decode(Token),
        {Type, UserId, Timestamp} = decode_payload(Payload),
        SiteSecret = site_auth_key(Context),
        UserSecret = user_auth_key(UserId, Context),
        HashCheck = z_crypto:auth_hash(SiteSecret, UserSecret, Payload),
        true = equal(Hash, HashCheck),
        {ok, {Type, UserId, Timestamp}}
    catch
        error:_ ->
            {error, illegal}
    end.

decode_payload(<<"cookie", 1, UserId:32, Timestamp:64/big-unsigned-integer, _Nonce1:64>>) ->
    {cookie, UserId, Timestamp};
decode_payload(<<"auth", 1, UserId:32, Timestamp:64/big-unsigned-integer, _Nonce1:64>>) ->
    {auth, UserId, Timestamp}.



%%
%% -----------------------------------------------------------------------------
%% 'Constant' time =:= operator for binaries, to mitigate timing attacks
%% -----------------------------------------------------------------------------
%%

-spec equal( binary(), binary() ) -> boolean().
equal(A, B) ->
  equal(A, B, 0).

equal(<< A, As/binary >>, << B, Bs/binary >>, Acc) ->
  equal(As, Bs, Acc bor (A bxor B));
equal(<<>>, <<>>, 0) ->
  true;
equal(_As, _Bs, _Acc) ->
  false.



%% @doc Url where to exchange the cookie token for a real cookie. Handled by a special controller.
%%      The token must be posted to the url as-is.
cookie_url(Context) ->
    z_context:abs_url(
        z_dispatcher:url_for(authentication_cookie, Context),
        Context).

%% @doc Return a token that can be used to logon a user. The token is only valid
%%      for a short period and can be used for a MQTT authentication.
cookie_token(UserId, Context) when is_integer(UserId) ->
    cookie_token(UserId, user_auth_key(UserId, Context), Context).


%% @doc Return a token that can be used to exchange for an authentication cookie.
%%      The authentication cookie will have a limited lifetime and must be refreshed
%%      periodically.
cookie_token(UserId, UserSecret, Context) when is_integer(UserId) ->
    Timestamp = z_datetime:timestamp(),
    Nonce1 = z_ids:number(),
    Payload = <<"cookie", 1, UserId:32, Timestamp:64/big-unsigned-integer, Nonce1:64>>,
    encode_payload_v1(Payload, UserSecret, Context).

%% @doc Return a token that can be used to logon a user. The token is only valid
%%      for a short period and can be used for a MQTT authentication.
auth_token(UserId, Context) when is_integer(UserId) ->
    auth_token(UserId, user_auth_key(UserId, Context), Context).

%% @doc Return a token that can be used to logon a user. The token is only valid
%%      for a short period and can be used for a MQTT authentication.
auth_token(UserId, UserSecret, Context) when is_integer(UserId) ->
    Timestamp = z_datetime:timestamp(),
    Nonce1 = z_ids:number(),
    Payload = <<"auth", 1, UserId:32, Timestamp:64/big-unsigned-integer, Nonce1:64>>,
    encode_payload_v1(Payload, UserSecret, Context).

%% @doc Encode the payload to base64 string that can be included in the returned value.
%%      This needs to be replaced with some encryption of the payload:
%%      For example: encrypt( [ hash(payload, UserSecret), payload ], server-secret )
encode_payload_v1(Payload, UserSecret, Context) ->
    SiteSecret = site_auth_key(Context),
    Hash = z_crypto:auth_hash(SiteSecret, UserSecret, Payload),
    FinalPayload = <<1, Hash:32/binary, Payload/binary>>,
    base64:encode(FinalPayload).
