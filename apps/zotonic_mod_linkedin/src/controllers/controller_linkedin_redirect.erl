%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014 Marc Worrell
%% @doc Handle the OAuth redirect of the LinkedIn logon handshake.
%% See https://developer.linkedin.com/documents/authentication

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

-module(controller_linkedin_redirect).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    html/1,
    fetch_user_data/1
    ]).

-include_lib("zotonic_core/include/controller_html_helper.hrl").

html(Context) ->
    QState = z_context:get_q("state", Context),
    case z_context:get_session(linkedin_state, Context) of
        undefined ->
            lager:warning("LinkedIn OAuth redirect with missing session state"),
            html_error(missing_secret, Context);
        QState ->
            case z_context:get_q("code", Context) of
                undefined ->
                    Context1 = z_render:wire({script, [{script, "window.close();"}]}, Context),
                    html_error(cancel, Context1);
                Code ->
                    access_token(fetch_access_token(Code, Context), Context)
            end;
        SessionState ->
            lager:warning("LinkedIn OAuth redirect with state mismatch, expected ~p, got ~p",
                          [SessionState, QState]),
            Context1 = z_render:wire({script, [{script, "window.close();"}]}, Context),
            html_error(wrong_secret, Context1)
    end.

access_token({ok, AccessToken, Expires}, Context) ->
    Data = [
        {access_token, AccessToken},
        {expires, Expires}
    ],
    user_data(fetch_user_data(AccessToken), Data, Context);
access_token({error, _Reason}, Context) ->
    html_error(access_token, Context).

user_data({ok, UserProps}, AccessData, Context) ->
    Auth = auth_user(UserProps, AccessData, Context),
    do_auth_user(Auth, Context);
user_data({error, _Reason}, _AccessData, Context) ->
    html_error(service_user_data, Context).

do_auth_user(Auth, Context) ->
    case z_notifier:first(Auth, Context) of
        undefined ->
            % No handler for signups, or signup not accepted
            lager:warning("[linkedin] Undefined auth_user return for user with props ~p", [Auth]),
            html_error(auth_user_undefined, Context);
        {error, duplicate} ->
            lager:info("[linkedin] Duplicate connection for user with props ~p", [Auth]),
            html_error(duplicate, Context);
        {error, {duplicate_email, Email}} ->
            lager:info("[facebook] User with email \"~s\" already exists", [Email]),
            html_error(duplicate_email, Email, Context);
        {error, signup_confirm} ->
            % We need a confirmation from the user before we add a new account
            html_error(signup_confirm, {auth, Auth}, Context);
        {error, _} = Err ->
            lager:warning("[linkedin] Error return ~p for user with props ~p", [Err, Auth]),
            html_error(auth_user_error, Context);
        {ok, Context1} ->
            html_ok(Context1)
    end.


html_ok(Context) ->
    Html = z_template:render("logon_service_done.tpl", [{service, "LinkedIn"} | z_context:get_all(Context)], Context),
    z_context:output(Html, Context).

html_error(Error, Context) ->
    html_error(Error, undefined, Context).

html_error(Error, What, Context) ->
    Vars = [
        {service, "LinkedIn"},
        {is_safari8problem, is_safari8problem(Context)},
        {what, What},
        {error, Error}
    ],
    Html = z_template:render("logon_service_error.tpl", Vars, Context),
    z_context:output(Html, Context).

%% @doc There is a problem here with Safari 8.0.x which (in its default setting) does not pass any
%%      cookies after the redirect from LinkedIn (and other OAuth redirects).
%%      See also this issue: https://github.com/drone/drone/issues/663#issuecomment-61565820
is_safari8problem(Context) ->
    Hs = m_req:get(headers, Context),
    HasCookies = proplists:is_defined(<<"cookie">>, Hs),
    UA = m_req:get(user_agent, Context),
    IsVersion8 = binary:match(UA, <<"Version/8.0.">>) =/= nomatch,
    IsSafari = binary:match(UA, <<"Safari/6">>) =/= nomatch,
    not HasCookies andalso IsVersion8 andalso IsSafari.


auth_user(#{<<"id">> := LinkedInUserId} = Profile, AccessTokenData, Context) ->
    lager:debug("[linkedin] Authenticating ~p ~p", [LinkedInUserId, Profile]),
    Location = maps:get(<<"location">>, Profile),
    Country = maps:get(<<"country">>, Location),
    PersonProps = [
            {title, maps:get(<<"formattedName">>, Profile, undefined)},
            {name_first, maps:get(<<"firstName">>, Profile, undefined)},
            {name_surname, maps:get(<<"lastName">>, Profile, undefined)},
            {summary, maps:get(<<"headline">>, Profile, undefined)},
            {body, z_html:escape_link(maps:get(<<"summary">>, Profile, undefined))},
            {website, maps:get(<<"publicProfileUrl">>, Profile, undefined)},
            {linkedin_url, maps:get(<<"publicProfileUrl">>, Profile, undefined)},
            {email, maps:get(<<"emailAddress">>, Profile, undefined)},
            {address_country, maps:get(<<"code">>, Country, undefined)},
            {address_line_1, maps:get(<<"name">>, Location, undefined)},
            {depiction_url, picture_url(maps:get(<<"pictureUrls">>, Profile, undefined))}
        ] ++ company_info(Profile),
    Args = controller_linkedin_authorize:get_args(Context),
    #auth_validated{
        service=linkedin,
        service_uid=LinkedInUserId,
        service_props=AccessTokenData,
        props=PersonProps,
        is_connect=z_convert:to_bool(proplists:get_value(<<"is_connect">>, Args))
    }.


company_info(#{<<"positions">> := #{<<"values">> := Qs}}) ->
    #{
        <<"company">> := #{
            <<"name">> := Company
        },
        <<"title">> := Title
    } = Qs,
    [
        {company_name, Company},
        {company_role, Title}
    ];
company_info(_) ->
    [].

picture_url(#{<<"values">> := [Url | _]}) ->
    Url;
picture_url(_) ->
    undefined.

% Exchange the code for an access token
fetch_access_token(Code, Context) ->
    {AppId, AppSecret, _Scope} = mod_linkedin:get_config(Context),
    PK = z_context:get_q("pk", Context, []),
    RedirectUrl = z_context:abs_url(z_dispatcher:url_for(linkedin_redirect, [{pk,PK}], Context), Context),
    LinkedInUrl = "https://www.linkedin.com/uas/oauth2/accessToken",
    Body = iolist_to_binary([
            "grant_type=authorization_code",
            "&client_id=", z_url:url_encode(AppId),
            "&redirect_uri=", z_convert:to_list(z_url:url_encode(RedirectUrl)),
            "&client_secret=", z_url:url_encode(AppSecret),
            "&code=", z_url:url_encode(Code)
        ]),
    case httpc:request(post, {LinkedInUrl, [], "application/x-www-form-urlencoded", Body}, httpc_http_options(), httpc_options()) of
        {ok, {{_, 200, _}, _Headers, Payload}} ->
            #{
                <<"access_token">> := AccessToken,
                <<"expires_in">> := ExpiresIn
            } = z_json:decode(Payload),
            {ok, AccessToken, ExpiresIn};
        Other ->
            lager:error("[linkedin] error fetching access token [code ~p] ~p", [Code, Other]),
            {error, {http_error, LinkedInUrl, Other}}
    end.

% Given the access token, fetch data about the user
fetch_user_data(AccessToken) ->
    LinkedInUrl = "https://api.linkedin.com/v1/people/\~:"
                ++fields()
                ++"?secure_urls=true&format=json&oauth2_access_token="
                ++z_convert:to_list(AccessToken),
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

% ensure_inets_profile(Profile) ->
%     case inets:start(httpc, [{profile, Profile}]) of
%         {ok, _Pid} ->
%             httpc:set_options([{keep_alive_timeout, 1}], Profile),
%             ok;
%         {error, {already_started, _Pid}} ->
%             ok
%     end.


%% Profile fields to request (see also https://developer.linkedin.com/documents/profile-fields#profile)
% id
% location:(country:(code))
% location:(name)
% summary
% picture-url
% public-profile-url
% email-address
fields() ->
    lists:flatten([
        $(,
            "id", $,,
            "first-name", $,,
            "last-name", $,,
            "formatted-name", $,,
            "headline", $,,
            "summary", $,,
            "location:(country:(code),name)", $,,
            "picture-urls::(original)", $,,
            "public-profile-url", $,,
            "positions:(title,company:(name))", $,,
            "email-address",
        $)
        ]).

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

