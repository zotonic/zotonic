%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
%% @doc Handle the OAuth callback from the Twitter
%% handshake. Exchanges request token (from session) with access
%% token.

%% Copyright 2011 Arjan Scherpenisse
%% Copyright 2014 Marc Worrell
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

-module(controller_twitter_redirect).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
    process/4
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").


process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    Context1 = z_context:reset_state_cookie(Context),
    case z_utils:is_empty(z_context:get_q(<<"denied">>, Context)) of
        true ->
            case z_context:get_state_cookie(Context) of
                {ok, {twitter, RequestToken, Args}} ->
                    access_token(
                        oauth_twitter_client:get_access_token(RequestToken, Context1),
                        Args,
                        Context1);
                _ ->
                    lager:warning("[twitter] Twitter OAuth redirect without cookie request token"),
                    html_error(request_token, Context1)
            end;
        false ->
            Context2 = z_render:wire({script, [{script, "window.close();"}]}, Context1),
            html_error(denied, Context2)
    end.


access_token({ok, AccessToken}, Args, Context) ->
    user_data(fetch_user_data(AccessToken, Context), AccessToken, Args, Context);
access_token({error, _Reason}, _Args, Context) ->
    html_error(access_token, Context).

user_data({ok, UserProps}, AccessToken, Args, Context) ->
    Auth = auth_user(UserProps, AccessToken, Args),
    do_auth_user(Auth, Context);
user_data({error, _}, _AccessToken, _Args, Context) ->
    html_error(user_props, Context).

do_auth_user(Auth, Context) ->
    case z_notifier:first(Auth, Context) of
        undefined ->
            % No handler for signups, or signup not accepted
            lager:warning("[twitter] Undefined auth_user return for user with props ~p", [Auth]),
            html_error(auth_user_undefined, Context);
        {error, duplicate} ->
            lager:info("[twitter] Duplicate connection for user with props ~p", [Auth]),
            html_error(duplicate, Context);
        {error, {duplicate_email, Email}} ->
            lager:info("[facebook] User with email \"~s\" already exists", [Email]),
            html_error(duplicate_email, Email, Context);
        {error, signup_confirm} ->
            % We need a confirmation from the user before we add a new account
            html_error(signup_confirm, {auth, Auth}, Context);
        {error, _} = Err ->
            lager:warning("[twitter] Error return ~p for user with props ~p", [Err, Auth]),
            html_error(auth_user_error, Context);
        {ok, Context1} ->
            html_ok(Context1)
    end.


html_ok(Context) ->
    Html = z_template:render("logon_service_done.tpl", [{service, "Twitter"} | z_context:get_all(Context)], Context),
    z_context:output(Html, Context).

html_error(Error, Context) ->
    html_error(Error, undefined, Context).

html_error(Error, What, Context) ->
    Vars = [
        {service, "Twitter"},
        {what, What},
        {error, Error}
    ],
    Html = z_template:render("logon_service_error.tpl", Vars, Context),
    z_context:output(Html, Context).


auth_user(TWProps, AccessToken, Args) ->
    TwitterUserId = unicode:characters_to_binary(proplists:get_value(id_str, TWProps)),
    TwitterUserName = unicode:characters_to_binary(proplists:get_value(screen_name, TWProps)),
    lager:debug("[twitter] Authenticating ~p ~p", [TwitterUserId, TWProps]),
    PersonProps = [
        {title, proplists:get_value(name, TWProps)},
        {website, <<"https://twitter.com/", TwitterUserName/binary>>},
        {summary, proplists:get_value(description, TWProps)},
        {depiction_url, proplists:get_value(profile_image_url_https, TWProps,
                            proplists:get_value(profile_image_url, TWProps))}
    ],
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
fetch_user_data(AccessToken, Context) ->
    oauth_twitter_client:request(get, "account/verify_credentials", AccessToken, Context).


