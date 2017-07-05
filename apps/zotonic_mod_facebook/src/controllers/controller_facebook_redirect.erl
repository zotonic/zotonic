%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2014 Marc Worrell
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
    html/1
    ]).

-include_lib("zotonic_core/include/controller_html_helper.hrl").

html(Context) ->
    case z_context:get_q("code", Context) of
        undefined ->
            Context1 = z_render:wire({script, [{script, "window.close();"}]}, Context),
            html_error(cancel, Context1);
        Code ->
            access_token(fetch_access_token(Code, Context), Context)
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
    case proplists:get_value(<<"email">>, UserProps) of
        undefined ->
            lager:info("[facebook] No email returned for user with props ~p", [UserProps]),
            html_error(email_required, Context);
        _Email ->
            case auth_user(UserProps, AccessData, Context) of
                undefined ->
                    % No handler for signups, or signup not accepted
                    lager:warning("[facebook] Undefined auth_user return for user with props ~p", [UserProps]),
                    html_error(auth_user_undefined, Context);
                {error, duplicate} ->
                    lager:info("[facebook] Duplicate connection for user with props ~p", [UserProps]),
                    html_error(duplicate, Context);
                {error, _} = Err ->
                    lager:warning("[facebook] Error return ~p for user with props ~p", [Err, UserProps]),
                    html_error(auth_user_error, Context);
                {ok, Context1} ->
                    html_ok(Context1)
            end
    end;
user_data({error, _Reason}, _AccessData, Context) ->
    html_error(service_user_data, Context).


html_ok(Context) ->
    Html = z_template:render("logon_service_done.tpl", [{service, "Facebook"} | z_context:get_all(Context)], Context),
    z_context:output(Html, Context).

html_error(Error, Context) ->
    Vars = [
        {service, "Facebook"},
        {error, Error},
        {auth_link, controller_facebook_authorize:redirect_location(Context)++"&auth_type=rerequest"}
    ],
    Html = z_template:render("logon_service_error.tpl", Vars, Context),
    z_context:output(Html, Context).


auth_user(FBProps, AccessTokenData, Context) ->
    FacebookUserId = proplists:get_value(<<"id">>, FBProps),
    lager:debug("[facebook] Authenticating ~p ~p", [FacebookUserId, FBProps]),
    PersonProps = [
        {title, proplists:get_value(<<"name">>, FBProps)},
        {name_first, proplists:get_value(<<"first_name">>, FBProps)},
        {name_surname, proplists:get_value(<<"last_name">>, FBProps)},
        {website, proplists:get_value(<<"link">>, FBProps)},
        {email, proplists:get_value(<<"email">>, FBProps, [])},
        {depiction_url, iolist_to_binary([
                <<"https://graph.facebook.com/">>,
                FacebookUserId,
                <<"/picture?type=large">>
            ])}
    ],
    Args = controller_facebook_authorize:get_args(Context),
    z_notifier:first(#auth_validated{
            service=facebook,
            service_uid=FacebookUserId,
            service_props=AccessTokenData,
            props=PersonProps,
            is_connect=z_convert:to_bool(proplists:get_value("is_connect", Args))
        },
        Context).


% Exchange the code for an access token
fetch_access_token(Code, Context) ->
    {AppId, AppSecret, _Scope} = mod_facebook:get_config(Context),
    RedirectUrl = z_context:abs_url(z_dispatcher:url_for(facebook_redirect, Context), Context),
    FacebookUrl = "https://graph.facebook.com/oauth/access_token?client_id="
                ++ z_url:url_encode(AppId)
                ++ "&redirect_uri=" ++ z_convert:to_list(z_url:url_encode(RedirectUrl))
                ++ "&client_secret=" ++ z_url:url_encode(AppSecret)
                ++ "&code=" ++ z_url:url_encode(Code),
    case httpc:request(FacebookUrl) of
        {ok, {{_, 200, _}, Headers, Payload}} ->
            Qs = decode_access_token(proplists:get_value("content-type", Headers), Payload),
            {ok, proplists:get_value("access_token", Qs), z_convert:to_integer(proplists:get_value("expires", Qs))};
        Other ->
            lager:error("[facebook] error fetching access token [code ~p] ~p", [Code, Other]),
            {error, {http_error, FacebookUrl, Other}}
    end.

decode_access_token("application/json"++_, Payload) ->
    {struct, Props} = mochijson:decode(Payload),
    Props;
decode_access_token(_ContentType, Payload) ->
    mochiweb_util:parse_qs(Payload).

% Given the access token, fetch data about the user
fetch_user_data(AccessToken) ->
    FacebookUrl = "https://graph.facebook.com/v2.3/me?access_token=" ++ z_url:url_encode(AccessToken),
    case httpc:request(FacebookUrl) of
        {ok, {{_, 200, _}, _Headers, Payload}} ->
            {struct, Props} = mochijson:binary_decode(Payload),
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
