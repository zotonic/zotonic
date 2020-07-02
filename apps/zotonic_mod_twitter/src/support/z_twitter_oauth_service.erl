%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2020 Arjan Scherpenisse, Marc Worrell
%% @doc Support routines for using Twitter as an external identity provider.

%% Copyright 2011-2020 Arjan Scherpenisse, Marc Worrell
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

-module(z_twitter_oauth_service).

-export([
    oauth_version/0,
    authorize_url/3,
    fetch_access_token/3,
    auth_validated/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").


%% @doc Return the major OAuth version being used
-spec oauth_version() -> pos_integer().
oauth_version() ->
    1.

%% @doc Return the authorization url for the OAuth permission dialog.
-spec authorize_url( binary(), binary(), z:context() ) -> binary().
authorize_url(RedirectUrl, _StateId, Context) ->
    {ok, RequestToken = {Token, _Secret}} = oauth_twitter_client:get_request_token(Context),
    m_server_storage:secure_store(twitter_access_token, RequestToken, Context),
    % Context1 = z_context:set_state_cookie({twitter, RequestToken, z_context:get_q_all_noz(Context)}, Context),
    RedirectUrl = z_context:abs_url(
                            z_dispatcher:url_for(twitter_redirect, Context),
                            Context),
    Lang = z_context:get_q(<<"lang">>, Context, z_context:language(Context)),
    iolist_to_binary([
        oauth_twitter_client:authorize_url(Token),
        "&oauth_callback=", z_url:url_encode(RedirectUrl),
        "&lang=", z_url:url_encode(Lang)
    ]).

%% @doc Exchange the code for an access token
-spec fetch_access_token( binary(), list(), z:context() ) -> {ok, map()} | {error, term()}.
fetch_access_token(_Code, _Args, Context) ->
    case z_utils:is_empty(z_context:get_q(<<"denied">>, Context)) of
        true ->
            case m_server_storage:secure_lookup(twitter_access_token, Context) of
                {ok, {_, _} = RequestToken} ->
                    m_server_storage:secure_delete(twitter_access_token, Context),
                    case oauth_twitter_client:get_access_token(RequestToken, Context) of
                        {ok, {Token, Secret}} ->
                            {ok, #{
                                <<"access_token">> => z_convert:to_binary(Token),
                                <<"token_secret">> => z_convert:to_binary(Secret)
                            }};
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} ->
                    {error, request_token}
            end;
        false ->
            {error, denied}
    end.

%% @doc Fetch the validated user data using the AccessToken
auth_validated(#{ <<"access_token">> := Access, <<"token_secret">> := Secret } = AT, Args, Context) ->
    Token = {Access, Secret},
    user_data(fetch_user_data(Token, Context), AT, Args).

user_data({ok, UserProps}, AccessToken, Args) ->
    {ok, auth_user(UserProps, AccessToken, Args)};
user_data({error, _} = Error, _AccessToken, _Args) ->
    Error.

auth_user(TWProps, AccessToken, Args) ->
    TwitterUserId = unicode:characters_to_binary(proplists:get_value(id_str, TWProps)),
    TwitterUserName = unicode:characters_to_binary(proplists:get_value(screen_name, TWProps)),
    lager:debug("[twitter] Authenticating ~p ~p", [TwitterUserId, TWProps]),
    PersonProps = #{
        <<"title">> => proplists:get_value(name, TWProps),
        <<"website">> => <<"https://twitter.com/", TwitterUserName/binary>>,
        <<"summary">> => proplists:get_value(description, TWProps),
        <<"depiction_url">> => proplists:get_value(profile_image_url_https, TWProps,
                                proplists:get_value(profile_image_url, TWProps))
    },
    AccessTokenData = [
        {access_token, AccessToken},
        {screen_name, TwitterUserName}
    ],
    #auth_validated{
        service = twitter,
        service_uid = TwitterUserId,
        service_props = AccessTokenData,
        props = PersonProps,
        is_connect = z_convert:to_bool(proplists:get_value(<<"is_connect">>, Args))
    }.

% Given the access token, fetch data about the user
fetch_user_data(Token, Context) ->
    oauth_twitter_client:request(get, "account/verify_credentials", Token, Context).
