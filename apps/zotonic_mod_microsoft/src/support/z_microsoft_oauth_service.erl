%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc Support routines for using Microsoft as an external identity provider.
%%
%% See: https://docs.microsoft.com/en-us/azure/active-directory/develop/active-directory-v2-protocols#endpoints
%% See: https://docs.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-auth-code-flow

%% Copyright 2021 Marc Worrell
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

-module(z_microsoft_oauth_service).

-export([
    title/1,
    oauth_version/0,
    authorize_url/3,
    fetch_access_token/5,
    auth_validated/3

    , fetch_user_data/1
    , fetch_user_photo/1
    , decode_jwt/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Return the service title for display in templates
-spec title( z:context() ) -> binary().
title(_Context) ->
    <<"Microsoft">>.

%% @doc Return the major OAuth version being used
-spec oauth_version() -> pos_integer().
oauth_version() ->
    2.

%% @doc Return the authorization url for the OAuth permission dialog.
-spec authorize_url( binary(), binary(), z:context() ) -> {ok, map()}.
authorize_url(RedirectUrl, StateId, Context) ->
    {AppId, _AppSecret, Scope, Tenant} = mod_microsoft:get_config(Context),
    {ok, #{
        url => iolist_to_binary([
            <<"https://login.microsoftonline.com/">>, z_url:url_encode(Tenant), <<"/oauth2/v2.0/authorize">>,
            "?client_id=", z_url:url_encode(AppId),
            "&response_type=code",
            "&redirect_uri=", z_url:url_encode(RedirectUrl),
            "&response_mode=query",
            "&scope=openid%20", z_url:url_encode(Scope),
            "&state=", StateId
        ]),
        data => undefined
    }}.

%% @doc Exchange the code for an access token
-spec fetch_access_token( binary(), term(), list(), map(), z:context() ) -> {ok, map()} | {error, term()}.
fetch_access_token(Code, _AuthData, _Args, _QArgs, Context) ->
    {AppId, AppSecret, Scope, Tenant} = mod_microsoft:get_config(Context),
    RedirectUrl = m_oauth2_service:redirect_url(Context),
    MicrosoftUrl = iolist_to_binary([
        <<"https://login.microsoftonline.com/">>, z_url:url_encode(Tenant), <<"/oauth2/v2.0/token">>
    ]),
    Request = iolist_to_binary([
        "client_id=", z_url:url_encode(AppId),
        "&scope=openid%20", z_url:url_encode(Scope),
        "&code=", z_url:url_encode(Code),
        "&redirect_uri=", z_url:url_encode(RedirectUrl),
        "&grant_type=authorization_code",
        "&client_secret=", z_url:url_encode(AppSecret)
    ]),
    case httpc:request(
            post,
            {binary_to_list(MicrosoftUrl), [], "application/x-www-form-urlencoded", Request},
            httpc_http_options(),
            httpc_options())
    of
        {ok, {{_, 200, _}, _Headers, Payload}} ->
            AccessData = #{
                <<"access_token">> := _,
                <<"expires_in">> := _,
                <<"token_type">> := <<"Bearer">>
            } = z_json:decode(Payload),
            {ok, AccessData};
        Other ->
            lager:error("[microsoft] error fetching access token [code ~p] ~p", [Code, Other]),
            {error, {http_error, MicrosoftUrl, Other}}
    end.

%% @doc Fetch the validated user data from the id_token
%% See https://docs.microsoft.com/en-us/azure/active-directory/develop/id-tokens
auth_validated(#{
        <<"access_token">> := AccessToken,
        <<"id_token">> := JWT
    } = AccessData, Args, _Context) ->
    case fetch_user_data(AccessToken) of
        {ok, UserProps} ->
            JWTProps = decode_jwt(JWT),
            MSUserId = maps:get(<<"id">>, UserProps),
            lager:debug("[microsoft] Authenticating ~p ~p", [MSUserId, UserProps]),
            PersonProps = #{
                <<"title">> => maps:get(<<"displayName">>, UserProps, undefined),
                <<"name_first">> => maps:get(<<"givenName">>, UserProps, undefined),
                <<"name_surname">> => maps:get(<<"surname">>, UserProps, undefined),
                <<"email">> => case maps:get(<<"mail">>, UserProps, <<>>) of
                    <<>> -> maps:get(<<"email">>, JWTProps, undefined);
                    undefined -> maps:get(<<"email">>, JWTProps, undefined);
                    Email -> Email
                end,
                <<"phone_mobile">> => maps:get(<<"mobilePhone">>, UserProps, undefined),
                <<"phone">> => case maps:get(<<"businessPhones">>, UserProps, []) of
                    [ Phone | _ ] when is_binary(Phone) -> Phone;
                    _ -> undefined
                end,
                <<"depiction_url">> => fetch_user_photo(AccessToken)
            },
            {ok, #auth_validated{
                service = microsoft,
                service_uid = MSUserId,
                service_props = AccessData,
                props = PersonProps,
                is_connect = z_convert:to_bool(proplists:get_value(<<"is_connect">>, Args))
            }};
        {error, _} = Error ->
            Error
    end.


% Given the access token, fetch data about the user
fetch_user_data(AccessToken) ->
    GraphUrl = "https://graph.microsoft.com/v1.0/users/me"
            ++ "?$select=displayName,givenName,postalCode",
    Hs = [
        {"Authorization", "Bearer " ++ z_convert:to_list(AccessToken)}
    ],
    case httpc:request(get, {GraphUrl, Hs}, httpc_http_options(), httpc_options()) of
        {ok, {{_, 200, _}, _Headers, Payload}} ->
            Props = z_json:decode(Payload),
            {ok, Props};
        Other ->
            lager:error("[microsoft] error fetching user data: ~p", [Other]),
            {error, {http_error, GraphUrl, Other}}
    end.

% Give the access token, fetch a data: url for the user'photo
fetch_user_photo(AccessToken) ->
    GraphUrl = "https://graph.microsoft.com/v1.0/me/photo/$value",
    Hs = [
        {"Authorization", "Bearer " ++ z_convert:to_list(AccessToken)}
    ],
    case httpc:request(get, {GraphUrl, Hs}, httpc_http_options(), httpc_options()) of
        {ok, {{_, 200, _}, Headers, Data}} ->
            case proplists:get_value("content-type", Headers) of
                Mime when Mime =:= "image/jpeg";
                          Mime =:= "image/png";
                          Mime =:= "image/gif" ->
                    iolist_to_binary([ <<"data:">>, Mime, <<";base64,">>, base64:encode(Data) ]);
                _ ->
                    undefined
            end;
        {ok, {{_, 401, _}, _Headers, _Data}} ->
            undefined;
        {ok, {{_, 404, _}, _Headers, _Data}} ->
            undefined;
        Other ->
            lager:info("[microsoft] error fetching user photo: ~p", [Other]),
            undefined
    end.


httpc_options() ->
    [
        {sync, true},
        {body_format, binary}
    ].

httpc_http_options() ->
    [
        {timeout, 10000},
        {connect_timeout, 10000},
        {autoredirect, true},
        {relaxed, true}
    ].




% Decode the JWT token. This is a dumb and insecure decode, as we don't take the
% effort of verifying the token. This is ok because we did receive the token from
% Microsoft when fetching the access token.
%
% This code is extracted from https://github.com/G-Corp/jwerl/blob/master/src/jwerl.erl
decode_jwt( JWT ) ->
    [ _Header64, Claims64, _Signature ] = binary:split(JWT, <<".">>, [global]),
    jsx:decode( base64_decode(Claims64) ).

base64_decode(Data) ->
    Data1 = << << (urldecode_digit(D)) >> || <<D>> <= Data >>,
    Data2 = case byte_size(Data1) rem 4 of
        2 -> <<Data1/binary, "==">>;
        3 -> <<Data1/binary, "=">>;
        _ -> Data1
    end,
    base64:decode(Data2).

urldecode_digit($_) -> $/;
urldecode_digit($-) -> $+;
urldecode_digit(D)  -> D.


