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

-include_lib("controller_html_helper.hrl").


html(Context) ->
    RequestToken = z_context:get_session(twitter_request_token, Context),
    z_context:set_session(twitter_request_token, undefined, Context),
    request_token(RequestToken, Context).

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
        {error, _} = Err ->
            lager:warning("[twitter] Error return ~p for user with props ~p", [Err, UserProps]),
            html_error(auth_user_error, Context);
        {ok, Context1} ->
            html_ok(Context1)
    end;
user_data({error, _}, _AccessToken, Context) ->
    html_error(user_props, Context).


html_ok(Context) ->
    Html = z_template:render("logon_service_done.tpl", [{service, "Twitter"}], Context),
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
        {thumbnail_url, proplists:get_value(profile_image_url_https, TWProps)}
    ],
    AccessTokenData = [
        {access_token, AccessToken},
        {screen_name, TwitterUserName}
    ],
    z_notifier:first(#auth_validated{
            service=twitter,
            service_uid=TwitterUserId,
            service_props=AccessTokenData,
            props=PersonProps,
            id=undefined
        },
        Context).


% moved_temporarily(ReqData, Context) ->
%     Context1 = ?WM_REQ(ReqData, Context),
%     Token = {_, _} = z_context:get_session(twitter_request_token, Context),
%     case oauth_twitter_client:get_access_token(Token, Context) of
%         {ok, AccessToken} ->
%             z_context:set_session(twitter_logon, true, Context1),
%             z_context:set_session(twitter_access_token, AccessToken, Context1),

%             case fetch_user_data(AccessToken, Context1) of
%                 {ok, UserProps} ->
%                     Page = z_context:get_session(twitter_ready_page, Context1),
%                     logon_twitter_user(UserProps, Page, Context1);
%                 {error, Reason} ->
%                     redirect_error(Reason, Context1)
%             end;
%         {error, Reason} ->
%             redirect_error(Reason, Context1)
%     end.


% %% @doc Redirect user to a signup failure URL, or to the logon page.
% redirect_error(Reason, Context) ->
%     z_context:set_session(twitter_logon, false, Context),
%     z_context:set_session(twitter_access_token, undefined, Context),
%     Location = case z_notifier:first(#signup_failed_url{reason=Reason}, Context) of
%                    {ok, L} -> L;
%                    undefined ->
%                        z_context:abs_url(z_dispatcher:url_for(logon, Context), Context)
%                end,
%     ?WM_REPLY({true, z_convert:to_list(Location)}, Context).



% Given the access token, fetch data about the user
fetch_user_data(AccessToken, Context) ->
    oauth_twitter_client:request(get, "account/verify_credentials", AccessToken, Context).



% %% @doc Check if the user exists, if not then hand over control to the auth_signup resource.
% logon_twitter_user(TwitterProps, LocationAfterSignup0, Context) ->
%     LocationAfterSignup = case z_utils:is_empty(LocationAfterSignup0) of
%                               true -> undefined;
%                               false -> LocationAfterSignup0
%                           end,
%     Props = [
%         {title, unicode:characters_to_binary(proplists:get_value(name, TwitterProps))}
%     ],
%     UID = proplists:get_value(id_str, TwitterProps),
%     case m_identity:lookup_by_type_and_key("twitter", UID, Context) of
%         undefined ->
%             % Register the Twitter identities as verified
%             SignupProps = [
%                 {identity, {username_pw, {z_utils:generate_username(Props, Context), z_ids:id(10)}, true, true}},
%                 {identity, {twitter, UID, true, true}},
%                 {identity, {twitter_screenname, proplists:get_value(screen_name, TwitterProps), true, true}},
%                 {ready_page, LocationAfterSignup}
%             ],
%             case z_notifier:first(#signup_url{props=Props, signup_props=SignupProps}, Context) of
%                 {ok, Location} ->
%                     ?WM_REPLY({true, Location}, Context);
%                 undefined ->
%                     throw({error, {?MODULE, "No result from signup_url notification handler"}})
%             end;
%         Row ->
%             UserId = proplists:get_value(rsc_id, Row),
%             {Location,Context1} = case z_auth:logon(UserId, Context) of
%                                                   {ok, ContextUser} ->
%                                                       case z_notifier:first(#logon_ready_page{request_page=LocationAfterSignup}, ContextUser) of
%                                                           undefined ->
%                                                               case LocationAfterSignup of
%                                                                   undefined -> 
%                                                                       {m_rsc:p(UserId, page_url, ContextUser), ContextUser};
%                                                                   _ ->
%                                                                       {LocationAfterSignup, ContextUser}
%                                                               end;
%                                                           Url -> {Url, ContextUser}
%                                                       end;
%                                                   {error, _Reason} ->
%                                                       {z_dispatcher:url_for(logon, [{error_uid,UserId}], Context), Context}
%             end,
%             LocationAbs = z_convert:to_list(z_context:abs_url(Location, Context1)),
%             ?WM_REPLY({true, LocationAbs}, Context1)
%     end.
    
