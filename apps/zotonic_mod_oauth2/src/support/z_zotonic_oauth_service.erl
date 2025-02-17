%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021-2025 Marc Worrell
%% @doc Support routines for using Zotonic OAuth2 consumers as an external identity provider.
%% @end

%% Copyright 2021-2025 Marc Worrell
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

-module(z_zotonic_oauth_service).

-export([
    title/1,
    oauth_version/0,
    authorize_url/3,
    fetch_access_token/5,
    auth_validated/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Return the service title for display in templates
-spec title( z:context() ) -> binary().
title(Context) ->
    ConsumerId = binary_to_integer( z_context:get_q(<<"consumer_id">>, Context) ),
    case m_oauth2_consumer:get_consumer(ConsumerId, z_acl:sudo(Context)) of
        {ok, #{
            <<"description">> := Description
        }} ->
            Description;
        {error, _} ->
            <<"Unknown">>
    end.

%% @doc Return the major OAuth version being used
-spec oauth_version() -> pos_integer().
oauth_version() ->
    2.

%% @doc Return the authorization url for the OAuth permission dialog on the remote site.
-spec authorize_url( binary(), binary(), z:context() ) -> {ok, map()} | {error, term()}.
authorize_url(RedirectUrl, StateId, Context) ->
    ConsumerId = binary_to_integer( z_context:get_q(<<"consumer_id">>, Context) ),
    case m_oauth2_consumer:get_consumer(ConsumerId, z_acl:sudo(Context)) of
        {ok, #{
            <<"authorize_url">> := AuthorizeUrl,
            <<"domain">> := Domain,
            <<"app_code">> := AppCode
        }} ->
            AuthorizeUrl1 = case AuthorizeUrl of
                <<>> ->
                    % Default to the URL of a Zotonic site.
                    ContextNoLang = z_context:set_language('x-default', Context),
                    Args = [
                        {response_type, code},
                        {client_id, AppCode},
                        {redirect_uri, RedirectUrl},
                        {state, StateId},
                        {scope, <<"email">>}
                    ],
                    z_html:unescape(
                        iolist_to_binary([
                            <<"https://">>, Domain,
                            z_dispatcher:url_for(oauth2_server_authorize, Args, ContextNoLang)
                        ]));
                _ ->
                    iolist_to_binary([
                        AuthorizeUrl, <<"?response_type=code">>,
                        "&client_id=", z_url:url_encode(AppCode),
                        "&redirect_uri=", z_url:url_encode(RedirectUrl),
                        "&state=", StateId,
                        "&scope=email"
                    ])
            end,
            {ok, #{
                url => AuthorizeUrl1,
                data => #{
                    <<"consumer_id">> => ConsumerId
                }
            }};
        {error, _} = Error ->
            Error
    end.


%% @doc Exchange the received code for an access token to the remote site.
-spec fetch_access_token( binary(), term(), list(), map(), z:context() ) -> {ok, map()} | {error, term()}.
fetch_access_token(Code, AuthData, Args, _QArgs, Context) ->
    ConsumerId = maps:get(<<"consumer_id">>, AuthData),
    ConsumerIdBin = integer_to_binary(ConsumerId),
    RedirectUrl = m_oauth2_service:redirect_url(Context),
    case proplists:get_value(<<"consumer_id">>, Args) of
        ConsumerIdBin ->
            case m_oauth2_consumer:get_consumer_oauth_service(ConsumerId, Context) of
                {ok, #{
                    <<"access_token_url">> := AccessTokenUrl,
                    <<"domain">> := Domain,
                    <<"app_code">> := AppCode,
                    <<"app_secret">> := AppSecret
                }} ->
                    AccessTokenUrl1 = case AccessTokenUrl of
                        <<>> ->
                            % Default to the URL of a Zotonic site.
                            ContextNoLang = z_context:set_language('x-default', Context),
                            QueryArgs = [
                                {client_id, AppCode},
                                {client_secret, AppSecret},
                                {code, Code},
                                {grant_type, <<"authorization_code">>},
                                {redirect_uri, RedirectUrl}
                            ],
                            z_html:unescape(
                                iolist_to_binary([
                                    <<"https://">>, Domain,
                                    z_dispatcher:url_for(oauth2_server_access_token, QueryArgs, ContextNoLang)
                                ]));
                        _ ->
                            iolist_to_binary([
                                AccessTokenUrl,
                                "?client_id=", z_url:url_encode(AppCode),
                                "&client_secret=", z_url:url_encode(AppSecret),
                                "&code=", z_url:url_encode(Code),
                                "&grant_type=authorization_code",
                                "&redirect_uri=", z_url:url_encode(RedirectUrl)
                            ])
                    end,
                    case z_fetch:fetch(AccessTokenUrl1, [], Context) of
                        {ok, {_Final, _Hs, _Length, Body}} ->
                            try
                                {ok, jsxrecord:decode(Body)}
                            catch
                                _:_ ->
                                    ?LOG_ERROR(#{
                                        text => <<"OAuth2: could not access token payload">>,
                                        in => zotonic_mod_oauth2,
                                        result => error,
                                        reason => json,
                                        payload => Body
                                    }),
                                    {error, json}
                            end;
                        {error, Reason} ->
                            ?LOG_ERROR(#{
                                text => <<"OAuth2: could not fetch access token">>,
                                in => zotonic_mod_oauth2,
                                result => error,
                                reason => Reason
                            }),
                            {error, http}
                    end;
                {error, Reason} = Error ->
                    ?LOG_ERROR(#{
                        text => <<"OAuth2: could not fetch consumer for access token">>,
                        in => zotonic_mod_oauth2,
                        result => error,
                        reason => Reason
                    }),
                    Error
            end;
        Found ->
            ?LOG_ERROR(#{
                text => <<"OAuth2: consumer_id mismatch between AuthData and Args">>,
                in => zotonic_mod_oauth2,
                result => error,
                reason => consumer_id,
                expected => ConsumerIdBin,
                found => Found
            }),
            {error, consumer_id}
    end.

%% @doc Return a auth_validated record with the validated user and identity information.
%% The identity will be of type 'mod_oauth2' and the key 'consumer:serviceuid'
-spec auth_validated( AccessTokenData :: map(), Args :: list(), z:context() ) ->
    {ok, #auth_validated{}} | {error, term()}.
auth_validated(#{
        <<"access_token">> := AccessToken,
        <<"user">> := User,
        <<"user_id">> := ServiceUid
    }, Args, Context) ->
    User1 = maps:without([ <<"uri">>, <<"is_authoritative">>, <<"name">> ], User),
    {<<"consumer_id">>, ConsumerIdBin} = proplists:lookup(<<"consumer_id">>, Args),
    ConsumerId = binary_to_integer(ConsumerIdBin),
    case m_oauth2_consumer:get_consumer_oauth_service(ConsumerId, Context) of
        {ok, #{
            <<"name">> := ConsumerName
        }} ->
            ServiceUidBin = z_convert:to_binary(ServiceUid),
            ServiceUidPrefixed = <<ConsumerName/binary, $:, ServiceUidBin/binary>>,
            % Ensure we don't overflow the identity key.
            ServiceUidPrefixed1 = z_string:truncatechars(ServiceUidPrefixed, 200, <<>>),
            {ok, #auth_validated{
                service = mod_oauth2,
                service_uid = ServiceUidPrefixed1,
                service_props = #{ <<"access_token">> => AccessToken },
                props = User1,
                is_connect = z_convert:to_bool(proplists:get_value(<<"is_connect">>, Args))
            }};
        {error, _} = Error ->
            Error
    end.
