%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2020 Marc Worrell
%% @doc Handle the OAuth redirect of the OAuth logon handshake.

%% Copyright 2014-2020 Marc Worrell
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

-module(controller_oauth2_service_redirect).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    process/4
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    QState = z_context:get_q(<<"state">>, Context),
    Context1 = z_context:reset_state_cookie(Context),
    case z_context:get_state_cookie(Context) of
        {ok, {QState, ServiceMod, Args}} when is_atom(ServiceMod) ->
            case ServiceMod:oauth_version() of
                1 ->
                    access_token(
                        ServiceMod:fetch_access_token(<<>>, Args, Context1),
                        ServiceMod,
                        Args,
                        Context1);
                2 ->
                    case z_context:get_q(<<"code">>, Context) of
                        Code when is_binary(Code), Code =/= <<>> ->
                            access_token(
                                ServiceMod:fetch_access_token(Code, Args, Context1),
                                ServiceMod,
                                Args,
                                Context1);
                        undefined ->
                            Context2 = z_render:wire({script, [{script, "window.close();"}]}, Context1),
                            html_error(cancel, Context2)
                    end
            end;
        {ok, {SessionState, _, _}} ->
            lager:warning("OAuth redirect with state mismatch, expected ~p, got ~p",
                          [SessionState, QState]),
            Context2 = z_render:wire({script, [{script, "window.close();"}]}, Context1),
            html_error(wrong_secret, Context2);
        _ ->
            lager:warning("OAuth redirect with missing or illegal state cookie"),
            html_error(missing_secret, Context1)
    end.

access_token({ok, #{ <<"access_token">> := _ } = AccessData}, ServiceMod, Args, Context) ->
    user_data(
        ServiceMod:auth_validated(AccessData, Args, Context),
        Context);
access_token({error, _Reason}, _ServiceMod, _Args, Context) ->
    html_error(access_token, Context).

user_data({ok, Auth}, Context) ->
    case z_notifier:first(Auth, Context) of
        undefined ->
            % No handler for signups, or signup not accepted
            lager:warning("Undefined auth_user return for user with props ~p", [Auth]),
            html_error(auth_user_undefined, Context);
        {error, duplicate} ->
            lager:info("Duplicate connection for user with props ~p", [Auth]),
            html_error(duplicate, Context);
        {error, {duplicate_email, Email}} ->
            lager:info("User with email \"~s\" already exists", [Email]),
            html_error(duplicate_email, Email, Context);
        {error, signup_confirm} ->
            % We need a confirmation from the user before we add a new account
            html_error(signup_confirm, {auth, Auth}, Context);
        {error, _} = Err ->
            lager:warning("Error return ~p for user with props ~p", [Err, Auth]),
            html_error(auth_user_error, Context);
        {ok, Context1} ->
            html_ok(Context1)
    end;
user_data(_UserError, Context) ->
    html_error(service_user_data, Context).


html_ok(Context) ->
    Html = z_template:render("logon_service_done.tpl", z_context:get_all(Context), Context),
    z_context:output(Html, Context).

html_error(Error, Context) ->
    html_error(Error, undefined, Context).

html_error(Error, What, Context) ->
    Vars = [
        {what, What},
        {error, Error}
    ],
    Html = z_template:render("logon_service_error.tpl", Vars, Context),
    z_context:output(Html, Context).

