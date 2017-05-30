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
    html/1
    ]).

-include_lib("zotonic_core/include/controller_html_helper.hrl").


html(Context) ->
    case z_utils:is_empty(z_context:get_q(<<"denied">>, Context)) of
        true ->
            RequestToken = z_context:get_session(twitter_request_token, Context),
            z_context:set_session(twitter_request_token, undefined, Context),
            request_token(RequestToken, Context);
        false ->
            Context1 = z_render:wire({script, [{script, "window.close();"}]}, Context),
            html_error(denied, Context1)
    end.

request_token({_, _} = RequestToken, Context) ->
    access_token(oauth_twitter_client:get_access_token(RequestToken, Context), Context);
request_token(undefined, Context) ->
    lager:warning("[twitter] Twitter OAuth redirect without session request token"),
    Context1 = z_render:wire({script, [{script, "window.close();"}]}, Context),
    html_error(request_token, Context1).

access_token({ok, AccessToken}, Context) ->
    user_data(fetch_user_data(AccessToken, Context), AccessToken, Context);
access_token({error, _Reason}, Context) ->
    html_error(access_token, Context).

user_data({ok, UserProps}, AccessToken, Context) ->
    case auth_user(UserProps, AccessToken, Context) of
        undefined ->
            % No handler for signups, or signup not accepted
            lager:warning("[twitter] Undefined auth_user return for user with props ~p", [UserProps]),
            html_error(auth_user_undefined, Context);
        {error, duplicate} ->
            lager:info("[twitter] Duplicate connection for user with props ~p", [UserProps]),
            html_error(duplicate, Context);
        {error, _} = Err ->
            lager:warning("[twitter] Error return ~p for user with props ~p", [Err, UserProps]),
            html_error(auth_user_error, Context);
        {ok, Context1} ->
            html_ok(Context1)
    end;
user_data({error, _}, _AccessToken, Context) ->
    html_error(user_props, Context).


html_ok(Context) ->
    Html = z_template:render("logon_service_done.tpl", [{service, "Twitter"} | z_context:get_all(Context)], Context),
    z_context:output(Html, Context).

html_error(Error, Context) ->
    Vars = [
        {service, "Twitter"},
        {error, Error}
    ],
    Html = z_template:render("logon_service_error.tpl", Vars, Context),
    z_context:output(Html, Context).



auth_user(TWProps, AccessToken, Context) ->
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
    Args = controller_twitter_authorize:get_args(Context),
    z_notifier:first(#auth_validated{
            service=twitter,
            service_uid=TwitterUserId,
            service_props=AccessTokenData,
            props=PersonProps,
            is_connect=z_convert:to_bool(proplists:get_value("is_connect", Args))
        },
        Context).

% Given the access token, fetch data about the user
fetch_user_data(AccessToken, Context) ->
    oauth_twitter_client:request(get, "account/verify_credentials", AccessToken, Context).


