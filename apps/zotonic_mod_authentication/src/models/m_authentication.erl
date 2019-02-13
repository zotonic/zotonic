%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017 Marc Worrell
%% @doc Model for mod_authentication

%% Copyright 2017 Marc Worrell
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

    auth_token/2,
    cookie_token/2,

    site_auth_key/1,
    user_auth_key/2,

    decode_token/2,

    auth_tokens/2,
    cookie_url/1
]).

-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ authenticate, password | Rest ], #{ payload := Payload }, Context) when is_map(Payload) ->
    case auth_tokens(Payload, Context) of
        {ok, Tk} -> {ok, {Tk, Rest}};
        {error, _} = Error -> Error
    end;
m_get([ password_min_length | Rest ], _Msg, Context) ->
    Len = case m_config:get_value(mod_authenticaton, password_min_length, Context) of
        undefined -> 6;
        <<>> -> 6;
        N -> z_convert:to_integer(N)
    end,
    {ok, {Len, Rest}};
m_get(Vs, _Msg, _Context) ->
    lager:debug("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {error, unknown_path}.


%% @doc If authentication was possible, then return auth tokens and cookie url.
-spec auth_tokens( map(), z:context() ) -> {ok, map()} | {error, term()}.
auth_tokens( #{ <<"username">> := Username, <<"password">> := Password }, Context) ->
    case m_identity:check_username_pw(Username, Password, Context) of
        {ok, UserId} ->
            {ok, UserSecret} = user_auth_key(UserId, Context),
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


%% @spec Fetch the secret user key. This key is used to add an extra hash of the tokens.
-spec user_auth_key( m_rsc:rid(), z:context() ) -> binary().
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

%% @spec Return the secret site key used for symmetrically encrypting tokens.
-spec site_auth_key( z:context() ) -> binary().
site_auth_key(Context) ->
    case m_config:get_value(mod_authenticaton, site_auth_key, Context) of
        undefined ->
            Key = z_ids:id(64),
            m_config:set_value(mod_authenticaton, site_auth_key, Key, Context),
            Key;
        SignKey ->
            SignKey
    end.

%% @doc Decode a token, check the hashes.
-spec decode_token( binary(), z:context() ) -> {ok, {cookie|auth, UserId :: m_rsc:rid(), Timestamp::pos_integer()}} | {error, illegal}.
decode_token(Token, Context) ->
    try
        <<1, Hash:32/binary, Payload/binary>> = base64:decode(Token),
        {Type, UserId, Timestamp} = decode_payload(Payload),
        SiteSecret = site_auth_key(Context),
        UserSecret = user_auth_key(UserId, Context),
        HashCheck = crypto:hmac(sha256, <<SiteSecret/binary, UserSecret/binary>>, Payload),
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



%% @spec Url where to exchange the cookie token for a real cookie. Handled by a special controller.
%%       The token must be posted to the url as-is.
cookie_url(Context) ->
    z_context:abs_url(
        z_dispatcher:url_for(authentication_cookie, Context),
        Context).

%% @spec Return a token that can be used to logon an user. The token is only valid
%%       for a short period and can be used for a MQTT authentication.
cookie_token(UserId, Context) when is_integer(UserId) ->
    cookie_token(UserId, user_auth_key(UserId, Context), Context).


%% @spec Return a token that can be used to exchange for an authentication cookie.
%%       The authentication cookie will have a limited lifetime and must be refreshed
%%       periodically.
cookie_token(UserId, UserSecret, Context) when is_integer(UserId) ->
    Timestamp = z_datetime:timestamp(),
    Nonce1 = z_ids:number(),
    Payload = <<"cookie", 1, UserId:32, Timestamp:64/big-unsigned-integer, Nonce1:64>>,
    encode_payload_v1(Payload, UserSecret, Context).

%% @spec Return a token that can be used to logon an user. The token is only valid
%%       for a short period and can be used for a MQTT authentication.
auth_token(UserId, Context) when is_integer(UserId) ->
    auth_token(UserId, user_auth_key(UserId, Context), Context).

%% @spec Return a token that can be used to logon an user. The token is only valid
%%       for a short period and can be used for a MQTT authentication.
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
    Hash = crypto:hmac(sha256, <<SiteSecret/binary, UserSecret/binary>>, Payload),
    FinalPayload = <<1, Hash:32/binary, Payload/binary>>,
    base64:encode(FinalPayload).
