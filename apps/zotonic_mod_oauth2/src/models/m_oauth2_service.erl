%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020 Marc Worrell
%% @doc OAuth2 model for OAuth authentication

%% Copyright 2020 Marc Worrell
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

-module(m_oauth2_service).

-behaviour(zotonic_model).

-export([
    m_get/3,

    m_post/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(SESSION_AUTH_TTL, 3600).

m_get(_Path, _Msg, _Context) ->
    {error, notfound}.


m_post([ <<"oauth-redirect">> ], #{ payload := Payload }, Context) ->
    StateId = maps:get(<<"state_id">>, Payload),
    StateData = maps:get(<<"state_data">>, Payload),
    QArgs = maps:get(<<"qargs">>, Payload),
    Secret = z_context:state_cookie_secret(Context),
    case termit:decode_base64(StateData, Secret) of
        {ok, Expires} ->
            case termit:check_expired(Expires) of
                {ok, {StateId, ServiceMod, ServiceData, Data}} when is_atom(ServiceMod) ->
                    handle_redirect(StateId, ServiceMod, ServiceData, Data, QArgs, Context);
                {ok, _} ->
                    {error, state};
                {error, _} = Error ->
                    Error
            end;
        {error, _} ->
            {error, state_data}
    end.

handle_redirect(StateId, ServiceMod, ServiceData, Args, QArgs, Context) ->
    case ServiceMod:oauth_version() of
        1 ->
            access_token(
                ServiceMod:fetch_access_token(<<>>, ServiceData, Args, QArgs, Context),
                ServiceMod,
                Args,
                Context);
        2 ->
            case maps:get(<<"state">>, QArgs) of
                StateId ->
                    case maps:get(<<"code">>, QArgs) of
                        Code when is_binary(Code), Code =/= <<>> ->
                            access_token(
                                ServiceMod:fetch_access_token(Code, ServiceData, Args, QArgs, Context),
                                ServiceMod,
                                Args,
                                Context);
                        undefined ->
                            {error, code}
                    end;
                _ ->
                    lager:warning("OAuth redirect with missing or illegal state argument"),
                    {error, missing_secret}
            end
    end.

access_token({ok, #{ <<"access_token">> := _ } = AccessData}, ServiceMod, Args, Context) ->
    user_data(
        ServiceMod:auth_validated(AccessData, Args, Context),
        Context);
access_token({error, denied}, _ServiceMod, _Args, _Context) ->
    {error, denied};
access_token({error, _Reason}, _ServiceMod, _Args, _Context) ->
    {error, access_token}.

user_data({ok, Auth}, Context) ->
    case z_notifier:first(Auth, Context) of
        undefined ->
            % No handler for auth, signups, or signup not accepted
            lager:warning("Undefined auth_user return for user with props ~p", [Auth]),
            {error, auth_user_undefined};
        {ok, UserId} ->
            % TODO: generate one time token to login for this user
            Token = z_authentication_tokens:encode_onetime_token(UserId, Context),
            {ok, #{
                result => token,
                token => Token
            }};
        {error, signup_confirm} ->
            % We need a confirmation from the user before we add a new account
            % html_error(signup_confirm, {auth, Auth}, Context);
            Secret = z_context:state_cookie_secret(Context),
            Expires = termit:expiring(Auth, ?SESSION_AUTH_TTL),
            Encoded = termit:encode_base64(Expires, Secret),
            {ok, #{
                result => confirm,
                auth => Encoded
            }};
        {error, duplicate} ->
            {error, duplicate};
        {error, {duplicate_email, Email}} ->
            lager:info("User with email \"~s\" already exists", [Email]),
            {error, duplicate_email};
        {error, _} = Err ->
            lager:warning("Error return ~p for user with props ~p", [Err, Auth]),
            {error, auth_user_error}
    end;
user_data(_UserError, _Context) ->
    {error, service_user_data}.

