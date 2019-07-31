%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015 Marc Worrell
%% @doc Handle the OAuth redirect of the Instagram logon handshake.
%% See http://instagram.com/developer/authentication/

%% Copyright 2015 Marc Worrell
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

-module(controller_instagram_redirect).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    process/4
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    case z_context:get_q(<<"error">>, Context) of
        undefined ->
            case z_context:get_q(<<"code">>, Context) of
                Code when is_binary(Code), Code =/= <<>> ->
                    check_state(Code, Context);
                _NoCode ->
                    Context1 = z_render:wire({script, [{script, "window.close();"}]}, Context),
                    html_error(cancel, Context1)
            end;
        <<"access_denied">> ->
            html_error(z_context:get_q(<<"error_reason">>, Context), Context);
        Error ->
            lager:warning("[instagram] Error redirect with error: ~p", [Error]),
            html_error(auth_user_error, Context)
    end.

check_state(Code, Context) ->
    Context1 = z_context:reset_state_cookie(Context),
    case z_context:get_state_cookie(Context) of
        {ok, {CookieStateId, Args}} ->
            case z_context:get_q(<<"state">>, Context1) of
                CookieStateId ->
                    access_token(fetch_access_token(Code, Context1), Args, Context1);
                _Other ->
                    lager:error("[instagram] oauth redirect: mismatch on state arg"),
                    html_error(state_error, Context1)
            end;
        {error, _} ->
            lager:error("[instagram] oauth redirect: no or illegal state cookie"),
            html_error(state_error, Context1)
    end.


access_token({ok, {AccessToken, UserData}}, Args, Context) ->
    user_data(fetch_user_data(AccessToken, UserData), AccessToken, Args, Context);
access_token({error, _Reason}, _Args, Context) ->
    html_error(access_token, Context).

user_data({ok, UserProps}, AccessToken, Args, Context) ->
    case auth_user(UserProps, AccessToken, Args, Context) of
        undefined ->
            % No handler for signups, or signup not accepted
            lager:warning("[instagram] Undefined auth_user return for user with props ~p", [UserProps]),
            html_error(auth_user_undefined, Context);
        {error, duplicate} ->
            lager:info("[instagram] Duplicate connection for user with props ~p", [UserProps]),
            html_error(duplicate, Context);
        {error, _} = Err ->
            lager:warning("[instagram] Error return ~p for user with props ~p", [Err, UserProps]),
            html_error(auth_user_error, Context);
        {ok, Context1} ->
            html_ok(Context1)
    end.
% user_data({error, _Reason}, _AccessData, _Args, Context) ->
%     html_error(service_user_data, Context).


html_ok(Context) ->
    Html = z_template:render("logon_service_done.tpl", [{service, "Instagram"} | z_context:get_all(Context)], Context),
    z_context:output(Html, Context).

html_error(Error, Context) when is_atom(Error) ->
    Vars = [
        {service, "Instagram"},
        {error, Error}
    ],
    Html = z_template:render("logon_service_error.tpl", Vars, Context),
    z_context:output(Html, Context);
html_error("user_denied", Context) ->
    Context1 = z_render:wire({script, [{script, "window.close();"}]}, Context),
    html_error(cancel, Context1);
html_error(ErrorReason, Context) ->
    lager:warning("[instagram] Error return with reason ~p", [ErrorReason]),
    html_error(auth_user_error, Context).


auth_user(InstProps, AccessToken, Args, Context) ->
    InstagramUserId = maps:get(<<"id">>, InstProps),
    lager:debug("[instagram] Authenticating ~p ~p", [InstagramUserId, InstProps]),
    PersonProps = [
        {title, maps:get(<<"full_name">>, InstProps, <<>>)},
        {depiction_url, maps:get(<<"profile_picture">>, InstProps, <<>>)}
    ],
    z_notifier:first(#auth_validated{
            service=instagram,
            service_uid=InstagramUserId,
            service_props=[
                {access_token, AccessToken},
                {username, maps:get(<<"username">>, InstProps)}
            ],
            props=PersonProps,
            is_connect = z_convert:to_bool(maps:get(<<"is_connect">>, Args, false))
        },
        Context).


% Exchange the code for an access token
fetch_access_token(Code, Context) ->
    {AppId, AppSecret, _Scope} = mod_instagram:get_config(Context),
    RedirectUrl = controller_instagram_authorize:redirect_uri(Context),
    InstagramUrl = "https://api.instagram.com/oauth/access_token",
    FormData = iolist_to_binary([
            "client_id=", z_url:url_encode(AppId),
            "&client_secret=", z_url:url_encode(AppSecret),
            "&redirect_uri=", z_url:url_encode(RedirectUrl),
            "&grant_type=authorization_code",
            "&code=", z_url:url_encode(Code)
        ]),
    case httpc:request(post,
                       {InstagramUrl, [], "application/x-www-form-urlencoded", FormData},
                       httpc_http_options(), httpc_options())
    of
        {ok, {{_, 200, _}, _Headers, Payload}} ->
            #{
                <<"user">> := UserData = #{
                    <<"access_token">> := AccessToken
                }
            } = z_json:decode(Payload),
            {ok, {AccessToken, UserData}};
        Other ->
            lager:error("[instagram] error fetching access token [code ~p] ~p", [Code, Other]),
            {error, {http_error, InstagramUrl, Other}}
    end.


% Given the access token, fetch data about the user
fetch_user_data(_AccessToken, UserData) ->
    {ok, UserData}.

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

