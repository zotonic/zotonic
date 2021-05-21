%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2021 Marc Worrell
%% @doc Support routines for using LinkedIn as an external identity provider.

%% Copyright 2014-2021 Marc Worrell
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

-module(z_linkedin_oauth_service).

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
title(_Context) ->
    <<"LinkedIn">>.

%% @doc Return the major OAuth version being used
-spec oauth_version() -> pos_integer().
oauth_version() ->
    2.

%% @doc Return the authorization url for the OAuth permission dialog.
-spec authorize_url( binary(), binary(), z:context() ) -> {ok, map()}.
authorize_url(RedirectUrl, StateId, Context) ->
    {AppId, _AppSecret, Scope} = mod_linkedin:get_config(Context),
    {ok, #{
        url => iolist_to_binary([
            <<"https://www.linkedin.com/uas/oauth2/authorization?response_type=code">>,
            "&client_id=", z_url:url_encode(AppId),
            "&redirect_uri=", z_url:url_encode(RedirectUrl),
            "&state=", StateId,
            "&scope=", z_url:url_encode(Scope)
        ]),
        data => undefined
    }}.


%% @doc Exchange the code for an access token
-spec fetch_access_token( binary(), term(), list(), map(), z:context() ) -> {ok, map()} | {error, term()}.
fetch_access_token(Code, _AuthData, _Args, _QArgs, Context) ->
    {AppId, AppSecret, _Scope} = mod_linkedin:get_config(Context),
    RedirectUrl = m_oauth2_service:redirect_url(Context),
    LinkedInUrl = "https://www.linkedin.com/uas/oauth2/accessToken",
    Body = iolist_to_binary([
            "grant_type=authorization_code",
            "&client_id=", z_url:url_encode(AppId),
            "&redirect_uri=", z_url:url_encode(RedirectUrl),
            "&client_secret=", z_url:url_encode(AppSecret),
            "&code=", z_url:url_encode(Code)
        ]),
    case httpc:request(
            post,
            {LinkedInUrl, [], "application/x-www-form-urlencoded", Body},
            httpc_http_options(),
            httpc_options())
    of
        {ok, {{_, 200, _}, _Headers, Payload}} ->
            AccessData = #{
                <<"access_token">> := _,
                <<"expires_in">> := _
            } = z_json:decode(Payload),
            {ok, AccessData};
        Other ->
            lager:error("[linkedin] error fetching access token [code ~p] ~p", [Code, Other]),
            {error, {http_error, LinkedInUrl, Other}}
    end.

%% @doc Fetch the validated user data using the AccessToken
auth_validated(#{ <<"access_token">> := AccessToken } = AccessData, Args, Context) ->
    case fetch_user_data(AccessToken)  of
        {ok, UserData} ->
            case fetch_email_address(AccessToken) of
                {ok, Email} ->
                    auth_user(UserData, Email, AccessData, Args, Context);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.


auth_user(#{<<"id">> := LinkedInUserId} = Profile, Email, AccessTokenData, Args, _Context) ->
    lager:debug("[linkedin] Authenticating ~p ~p", [LinkedInUserId, Profile]),
    PersonProps = #{
        <<"title">> => iolist_to_binary([
                z_convert:to_binary(get_localized_value(<<"firstName">>, Profile)), " ",
                z_convert:to_binary(get_localized_value(<<"lastName">>, Profile))
            ]),
        <<"name_first">> => get_localized_value(<<"firstName">>, Profile),
        <<"name_surname">> => get_localized_value(<<"lastName">>, Profile),
        <<"summary">> => get_localized_value(<<"headline">>, Profile),
        <<"email">> => Email,
        <<"depiction_url">> => get_user_photo(Profile)
    },
    {ok, #auth_validated{
        service = linkedin,
        service_uid = LinkedInUserId,
        service_props = AccessTokenData,
        props = PersonProps,
        is_connect = z_convert:to_bool(proplists:get_value(<<"is_connect">>, Args))
    }}.

get_localized_value(Prop, JSON) ->
    case maps:get(Prop, JSON, undefined) of
        Localized when is_map(Localized) ->
            case maps:get(<<"localized">>, Localized, undefined) of
                Locs when is_map(Locs) ->
                    [ {_, V} | _ ] = maps:to_list(Locs),
                    maps:get(<<"en_US">>, Locs, V);
                undefined ->
                    undefined
            end;
        Bin when is_binary(Bin) ->
            Bin;
        null ->
            undefined;
        undefined ->
            undefined
    end.

% Given the access token, fetch data about the user
fetch_user_data(AccessToken) ->
    LinkedInUrl = "https://api.linkedin.com/v2/me"
                ++ "?oauth2_access_token=" ++ z_convert:to_list(AccessToken)
                ++ "&projection=(id,firstName,lastName,headline,profilePicture(displayImage~digitalmediaAsset:playableStreams))",
    case httpc:request(get, {LinkedInUrl, []}, httpc_http_options(), httpc_options()) of
        {ok, {{_, 200, _}, _Headers, Payload}} ->
            {ok, z_json:decode(Payload)};
        {ok, {{_, 401, _}, _Headers, Payload}} = Other ->
            lager:error("[linkedin] 401 error fetching user data [token ~p] will not retry ~p", [AccessToken, Payload]),
            {error, {http_error, LinkedInUrl, Other}};
        Other ->
            lager:error("[linkedin] error fetching user data [token ~p] ~p", [AccessToken, Other]),
            {error, {http_error, LinkedInUrl, Other}}
    end.

% Dig up the largest still image from the returned profile pictures
get_user_photo(#{
        <<"profilePicture">> := #{
            <<"displayImage~">> := #{
                <<"elements">> := Elements
            }
        }
    }) ->
    largest_picture(Elements, {0, undefined});
get_user_photo(_) ->
    undefined.

largest_picture([], {_, Url}) ->
    Url;
largest_picture([ #{ <<"data">> := Data, <<"identifiers">> := Idns } | Elts ], {Sz, Url}) ->
    case Data of
        #{
            <<"com.linkedin.digitalmedia.mediaartifact.StillImage">> := #{
                <<"storageSize">> := #{ <<"height">> := H, <<"width">> := W }
            }
        } when W * H > Sz ->
            case lists:filtermap(
                fun
                    (#{ <<"identifierType">> := <<"EXTERNAL_URL">>, <<"identifier">> := ExtUrl}) ->
                        {true, ExtUrl};
                    (_) ->
                        false
                end,
                Idns)
            of
                [ NewUrl | _ ] ->
                    largest_picture(Elts, {W*H, NewUrl});
                [] ->
                    largest_picture(Elts, {Sz, Url})
            end;
        _ ->
            largest_picture(Elts, {Sz, Url})
    end.


% [{<<"elements">>,
%       [{struct,[{<<"handle">>,<<"urn:li:emailAddress:109707987">>},
%                 {<<"handle~">>,
%                  {struct,[{<<"emailAddress">>,<<"piet@example.com">>}]}}]}]}]}
fetch_email_address(AccessToken) ->
    LinkedInUrl = "https://api.linkedin.com/v2/emailAddress?q=members&projection=(elements*(handle~))"
                ++ "&oauth2_access_token=" ++ z_convert:to_list(AccessToken),
    case httpc:request(get, {LinkedInUrl, []}, httpc_http_options(), httpc_options()) of
        {ok, {{_, 200, _}, _Headers, Payload}} ->
            Props = z_json:decode(Payload),
            case maps:get(<<"elements">>, Props, []) of
                [ Hs | _ ] when is_map(Hs) ->
                    case maps:get(<<"handle~">>, Hs, undefined) of
                        Es when is_map(Es) ->
                            case maps:get(<<"emailAddress">>, Es, undefined) of
                                Email when is_binary(Email) ->
                                    {ok, Email};
                                _ ->
                                    {error, noemail}
                            end;
                        _ ->
                            {error, noemail}
                    end;
                _ ->
                    {error, noemail}
            end;
        {ok, {{_, 401, _}, _Headers, Payload}} = Other ->
            lager:error("[linkedin] 401 error fetching user email [token ~p] will not retry ~p", [AccessToken, Payload]),
            {error, {http_error, LinkedInUrl, Other}};
        Other ->
            lager:error("[linkedin] error fetching user email [token ~p] ~p", [AccessToken, Other]),
            {error, {http_error, LinkedInUrl, Other}}
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
