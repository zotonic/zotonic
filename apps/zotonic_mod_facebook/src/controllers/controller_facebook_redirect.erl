%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2017 Marc Worrell
%% @doc Handle the OAuth redirect of the Facebook logon handshake.
%% See http://developers.facebook.com/docs/authentication/

%% Copyright 2010-2014 Marc Worrell
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

-module(controller_facebook_redirect).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    process/4
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    QStateId = z_context:get_q(<<"state">>, Context),
    Code = z_context:get_q(<<"code">>, Context),
    Context1 = z_context:reset_state_cookie(Context),
    case z_context:get_state_cookie(Context) of
        {ok, {CookieStateId, Args}} ->
            html_1(Code, QStateId, CookieStateId, Args, Context1);
        {error, Reason} ->
            lager:error("Facebook redirect with illegal state cookie: ~p", [Reason]),
            html_error(cancel, Context1)
    end.

html_1(Code, State, State, Args, Context) when is_binary(Code), Code =/= <<>> ->
    access_token(fetch_access_token(Code, Context), Args, Context);
html_1(_Code, _State, _SessionState, _Args, Context) ->
    Context1 = z_render:wire({script, [{script, "window.close();"}]}, Context),
    html_error(cancel, Context1).

access_token({ok, AccessToken, Expires}, Args, Context) ->
    Data = [
        {access_token, AccessToken},
        {expires, Expires}
    ],
    user_data(fetch_user_data(AccessToken), Data, Args, Context);
access_token({error, _Reason}, _Args, Context) ->
    html_error(access_token, Context).

user_data({ok, UserProps}, AccessData, Args, Context) ->
    Auth = auth_user(UserProps, AccessData, Args),
    do_auth_user(Auth, Context);
user_data({error, _Reason}, _AccessData, _Args, Context) ->
    html_error(service_user_data, Context).

do_auth_user(Auth, Context) ->
    case z_notifier:first(Auth, Context) of
        undefined ->
            % No handler for signups, or signup not accepted
            lager:warning("[facebook] Undefined auth_user return for user with props ~p", [Auth]),
            html_error(auth_user_undefined, Context);
        {error, duplicate} ->
            lager:info("[facebook] Duplicate connection for user with props ~p", [Auth]),
            html_error(duplicate, Context);
        {error, {duplicate_email, Email}} ->
            lager:info("[facebook] User with email \"~s\" already exists", [Email]),
            html_error(duplicate_email, Email, Context);
        {error, signup_confirm} ->
            % We need a confirmation from the user before we add a new account
            html_error(signup_confirm, {auth, Auth}, Context);
        {error, _} = Err ->
            lager:warning("[facebook] Error return ~p for user with props ~p", [Err, Auth]),
            html_error(auth_user_error, Context);
        {ok, Context1} ->
            html_ok(Context1)
    end.


html_ok(Context) ->
    Html = z_template:render("logon_service_done.tpl", [{service, "Facebook"} | z_context:get_all(Context)], Context),
    z_context:output(Html, Context).

html_error(Error, Context) ->
    html_error(Error, undefined, Context).

html_error(Error, What, Context) ->
    Vars = [
        {service, "Facebook"},
        {error, Error},
        {what, What}
    ],
    Html = z_template:render("logon_service_error.tpl", Vars, Context),
    z_context:output(Html, Context).


auth_user(FBProps, AccessTokenData, Args) ->
    FacebookUserId = proplists:get_value(<<"id">>, FBProps),
    lager:debug("[facebook] Authenticating ~p ~p", [FacebookUserId, FBProps]),
    PersonProps = [
        {title, proplists:get_value(<<"name">>, FBProps)},
        {name_first, proplists:get_value(<<"first_name">>, FBProps)},
        {name_surname, proplists:get_value(<<"last_name">>, FBProps)},
        {website, proplists:get_value(<<"link">>, FBProps)},
        {email, proplists:get_value(<<"email">>, FBProps, <<>>)},
        {depiction_url, iolist_to_binary([
                <<"https://graph.facebook.com/">>,
                FacebookUserId,
                <<"/picture?type=large">>
            ])}
    ],
    #auth_validated{
        service = facebook,
        service_uid = FacebookUserId,
        service_props = AccessTokenData,
        props = PersonProps,
        is_connect = z_convert:to_bool(proplists:get_value(<<"is_connect">>, Args))
    }.


% Exchange the code for an access token
fetch_access_token(Code, Context) ->
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

% [{"id","10000123456789"},
% {"name","Jane Doe"},
% {"first_name","Jane"},
% {"last_name","Doe"},
% {"link","http://www.facebook.com/profile.php?id=10000123456789"},
% {"gender","male"},
% {"email","me@example.com"},
% {"timezone",2},
% {"locale","en_US"},
% {"username","jane.doe.123"},
% {"verified",true}
% {"updated_time","2010-05-09T11:27:09+0000"}]
