%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2011-09-22
%% @doc Handle the OAuth callback from the Twitter
%% handshake. Exchanges request token (from session) with access
%% token.

%% Copyright 2011 Arjan Scherpenisse
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

-module(resource_twitter_redirect).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([init/1, service_available/2, charsets_provided/2, content_types_provided/2]).
-export([resource_exists/2, previously_existed/2, moved_temporarily/2]).

-include_lib("webmachine_resource.hrl").
-include_lib("include/zotonic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    Context2 = z_context:ensure_all(Context1),
    ?WM_REPLY(true, Context2).

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", provide_content}], ReqData, Context}.

resource_exists(ReqData, Context) ->
    {false, ReqData, Context}.

previously_existed(ReqData, Context) ->
    {true, ReqData, Context}.

moved_temporarily(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Token = {_, _} = z_context:get_session(twitter_request_token, Context),
    case oauth_twitter_client:get_access_token(Token, Context) of
        {ok, AccessToken} ->
            z_context:set_session(twitter_logon, true, Context1),
            z_context:set_session(twitter_access_token, AccessToken, Context1),

            case fetch_user_data(AccessToken, Context1) of
                {ok, UserProps} ->
                    Page = z_context:get_session(twitter_ready_page, Context1),
                    logon_twitter_user(UserProps, Page, Context1);
                {error, Reason} ->
                    redirect_error(Reason, Context1)
            end;
        {error, Reason} ->
            redirect_error(Reason, Context1)
    end.


%% @doc Redirect user to a signup failure URL, or to the logon page.
redirect_error(Reason, Context) ->
    ?DEBUG({?MODULE, Reason}),
    z_context:set_session(twitter_logon, false, Context),
    z_context:set_session(twitter_access_token, undefined, Context),
    Location = case z_notifier:first(#signup_failed_url{reason=Reason}, Context) of
                   {ok, L} -> L;
                   undefined ->
                       z_context:abs_url(z_dispatcher:url_for(logon, Context), Context)
               end,
    ?WM_REPLY({true, Location}, Context).



% Given the access token, fetch data about the user
fetch_user_data(AccessToken, Context) ->
    oauth_twitter_client:request(get, "account/verify_credentials", AccessToken, Context).


%% @doc Check if the user exists, if not then hand over control to the auth_signup resource.
logon_twitter_user(TwitterProps, LocationAfterSignup0, Context) ->
    LocationAfterSignup = case z_utils:is_empty(LocationAfterSignup0) of
                              true -> undefined;
                              false -> LocationAfterSignup0
                          end,
    Props = [
        {title, unicode:characters_to_binary(proplists:get_value(name, TwitterProps))}
    ],
    UID = proplists:get_value(id_str, TwitterProps),
    case m_identity:lookup_by_type_and_key("twitter", UID, Context) of
        undefined ->
            % Register the Twitter identities as verified
            SignupProps = [
                {identity, {username_pw, {z_utils:generate_username(Props, Context), z_ids:id(6)}, true, true}},
                {identity, {twitter, UID, true, true}},
                {identity, {twitter_screenname, proplists:get_value(screen_name, TwitterProps), true, true}},
                {ready_page, LocationAfterSignup}
            ],
            case z_notifier:first(#signup_url{props=Props, signup_props=SignupProps}, Context) of
                {ok, Location} ->
                    use_see_other(Location, Context);
                    %?WM_REPLY({true, Location}, Context);
                undefined ->
                    throw({error, {?MODULE, "No result from signup_url notification handler"}})
            end;
        Row ->
            UserId = proplists:get_value(rsc_id, Row),
			{Location,Context1} = case z_auth:logon(UserId, Context) of
                                                  {ok, ContextUser} ->
                                                      case z_notifier:first(#logon_ready_page{request_page=LocationAfterSignup}, ContextUser) of
                                                          undefined ->
                                                              case LocationAfterSignup of
                                                                  undefined -> 
                                                                      {m_rsc:p(UserId, page_url, ContextUser), ContextUser};
                                                                  _ ->
                                                                      {LocationAfterSignup, ContextUser}
                                                              end;
                                                          Url -> {Url, ContextUser}
                                                      end;
                                                  {error, _Reason} ->
                                                      {z_dispatcher:url_for(logon, [{error_uid,UserId}], Context), Context}
			end,
            LocationAbs = lists:flatten(z_context:abs_url(Location, Context1)),
            use_see_other(LocationAbs, Context1)
    end.
    
use_see_other(Location, Context) ->
    ContextLoc = z_context:set_resp_header("Location", Location, Context),
    ?WM_REPLY({halt, 303}, ContextLoc).
