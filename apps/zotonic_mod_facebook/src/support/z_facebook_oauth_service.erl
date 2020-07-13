%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2020 Marc Worrell
%% @doc Support routines for using Facebook as an external identity provider.
%%
%% See: http://developers.facebook.com/docs/authentication/

%% Copyright 2010-2020 Marc Worrell
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

-module(z_facebook_oauth_service).

-export([
    title/1,
    oauth_version/0,
    authorize_url/3,
    fetch_access_token/4,
    auth_validated/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Return the service title for display in templates
-spec title( z:context() ) -> binary().
title(_Context) ->
    <<"Facebook">>.

%% @doc Return the major OAuth version being used
-spec oauth_version() -> pos_integer().
oauth_version() ->
    2.

%% @doc Return the authorization url for the OAuth permission dialog.
-spec authorize_url( binary(), binary(), z:context() ) -> {ok, map()}.
authorize_url(RedirectUrl, StateId, Context) ->
    {AppId, _AppSecret, Scope} = mod_facebook:get_config(Context),
    {ok, #{
        url => iolist_to_binary([
            <<"https://www.facebook.com/v2.9/dialog/oauth?client_id=">>,
            z_url:url_encode(AppId),
            "&redirect_uri=", z_url:url_encode(RedirectUrl),
            "&display=popup",
            "&response_type=code",
            "&scope=", z_url:url_encode(Scope),
            "&state=", StateId
        ]),
        data => undefined
    }}.

%% @doc Exchange the code for an access token
-spec fetch_access_token( binary(), term(), list(), z:context() ) -> {ok, map()} | {error, term()}.
fetch_access_token(Code, _AuthData, _Args, Context) ->
    {AppId, AppSecret, _Scope} = mod_facebook:get_config(Context),
    RedirectUrl = z_context:abs_url(z_dispatcher:url_for(facebook_redirect, Context), Context),
    FacebookUrl = "https://graph.facebook.com/v2.9/oauth/access_token?client_id="
                ++ z_url:url_encode(AppId)
                ++ "&redirect_uri=" ++ z_convert:to_list(z_url:url_encode(RedirectUrl))
                ++ "&client_secret=" ++ z_url:url_encode(AppSecret)
                ++ "&code=" ++ z_url:url_encode(Code),
    case httpc:request(FacebookUrl) of
        {ok, {{_, 200, _}, Headers, Payload}} ->
            #{
                <<"access_token">> := AccessToken,
                <<"expires">> := Expires
            } = decode_access_token(proplists:get_value("content-type", Headers), Payload),
            {ok, AccessToken, z_convert:to_integer(Expires)};
        Other ->
            lager:error("[facebook] error fetching access token [code ~p] ~p", [Code, Other]),
            {error, {http_error, FacebookUrl, Other}}
    end.

decode_access_token("application/json"++_, Payload) ->
    z_json:decode(Payload);
decode_access_token(_ContentType, Payload) ->
    maps:from_list(mochiweb_util:parse_qs(Payload)).


%% @doc Fetch the validated user data using the AccessToken
auth_validated(#{ <<"access_token">> := AccessToken } = AccessData, Args, _Context) ->
    case fetch_user_data(AccessToken) of
        {ok, FBProps} ->
            FacebookUserId = proplists:get_value(<<"id">>, FBProps),
            lager:debug("[facebook] Authenticating ~p ~p", [FacebookUserId, FBProps]),
            PersonProps = #{
                <<"title">> => maps:get(<<"name">>, FBProps, undefined),
                <<"name_first">> => maps:get(<<"first_name">>, FBProps, undefined),
                <<"name_surname">> => maps:get(<<"last_name">>, FBProps, undefined),
                <<"website">> => maps:get(<<"link">>, FBProps, undefined),
                <<"email">> => maps:get(<<"email">>, FBProps, <<>>),
                <<"depiction_url">> => iolist_to_binary([
                        <<"https://graph.facebook.com/">>,
                        FacebookUserId,
                        <<"/picture?type=large">>
                    ])
            },
            {ok, #auth_validated{
                service = facebook,
                service_uid = FacebookUserId,
                service_props = AccessData,
                props = PersonProps,
                is_connect = z_convert:to_bool(proplists:get_value(<<"is_connect">>, Args))
            }};
        {error, _} = Error ->
            Error
    end.


% Given the access token, fetch data about the user
fetch_user_data(AccessToken) ->
    FacebookUrl = "https://graph.facebook.com/v2.9/me?fields=id,name,first_name,last_name,email&access_token="
                    ++ z_url:url_encode(AccessToken),
    case httpc:request(FacebookUrl) of
        {ok, {{_, 200, _}, _Headers, Payload}} ->
            Props = z_json:decode(Payload),
            {ok, Props};
        Other ->
            lager:error("[facebook] error fetching user data [token ~p] ~p", [AccessToken, Other]),
            {error, {http_error, FacebookUrl, Other}}
    end.
