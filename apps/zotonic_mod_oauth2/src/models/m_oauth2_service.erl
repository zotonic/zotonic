%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020-2021 Marc Worrell
%% @doc OAuth2 model for authentication using remote OAuth2 services.

%% Copyright 2020-2021 Marc Worrell
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

-include_lib("kernel/include/logger.hrl").

-export([
    m_get/3,
    m_post/3,

    redirect_url/1
]).

-define(SESSION_AUTH_TTL, 3600).

m_get([ <<"redirect_url">> | Rest ], _Msg, Context) ->
    Url = redirect_url(Context),
    {ok, {Url, Rest}};
m_get(_Path, _Msg, _Context) ->
    {error, notfound}.


m_post([ <<"oauth-redirect">> ], #{ payload := Payload }, Context) ->
    StateId = maps:get(<<"state_id">>, Payload),
    StateData = maps:get(<<"state_data">>, Payload),
    QArgs = maps:get(<<"qargs">>, Payload),
    SId = maps:get(<<"cotonic_sid">>, Payload),
    Secret = z_context:state_cookie_secret(Context),
    case termit:decode_base64(StateData, Secret) of
        {ok, Expires} ->
            case termit:check_expired(Expires) of
                {ok, {StateId, ServiceMod, ServiceData, Data}} when is_atom(ServiceMod) ->
                    handle_redirect(StateId, ServiceMod, ServiceData, Data, QArgs, SId, Context);
                {ok, _} ->
                    {error, state};
                {error, _} = Error ->
                    Error
            end;
        {error, _} ->
            {error, state_data}
    end.

-spec redirect_url(z:context()) -> binary().
redirect_url(Context) ->
    Context1 = z_context:set_language('x-default', Context),
    z_context:abs_url(
        z_dispatcher:url_for(oauth2_service_redirect, Context1),
        Context1).

handle_redirect(StateId, ServiceMod, ServiceData, Args, QArgs, SId, Context) ->
    case ServiceMod:oauth_version() of
        1 ->
            access_token(
                ServiceMod:fetch_access_token(<<>>, ServiceData, Args, QArgs, Context),
                ServiceMod,
                Args,
                SId,
                Context);
        2 ->
            case maps:get(<<"error">>, QArgs, undefined) of
                undefined ->
                    case maps:get(<<"state">>, QArgs, undefined) of
                        StateId ->
                            case maps:get(<<"code">>, QArgs, undefined) of
                                Code when is_binary(Code), Code =/= <<>> ->
                                    access_token(
                                        ServiceMod:fetch_access_token(Code, ServiceData, Args, QArgs, Context),
                                        ServiceMod,
                                        Args,
                                        SId,
                                        Context);
                                _ ->
                                    ?LOG_ERROR(#{
                                        text => <<"OAuth redirect error when fetching code">>,
                                        in => zotonic_mod_oauth2,
                                        result => error,
                                        reason => code,
                                        qargs => QArgs
                                    }),
                                    {error, code}
                            end;
                        _ ->
                            ?LOG_WARNING("OAuth redirect with missing or illegal state argument"),
                            {error, missing_secret}
                    end;
                <<"access_denied">> ->
                    case maps:get(<<"error_reason">>, QArgs, undefined) of
                        <<"user_denied">> ->
                            {error, code};
                        Error ->
                            ?LOG_ERROR(#{
                                text => <<"OAuth redirect error">>,
                                in => zotonic_mod_oauth2,
                                result => error,
                                reason => Error,
                                qargs => QArgs
                            }),
                            {error, code}
                    end;
                Error ->
                    ?LOG_ERROR(#{
                        text => <<"OAuth redirect error">>,
                        in => zotonic_mod_oauth2,
                        result => error,
                        reason => Error,
                        qargs => QArgs
                    }),
                    {error, code}
            end
    end.

access_token({ok, #{ <<"access_token">> := _ } = AccessData}, ServiceMod, Args, SId, Context) ->
    user_data(
        ServiceMod:auth_validated(AccessData, Args, Context),
        SId,
        Context);
access_token({ok, #{} = AccessData}, ServiceMod, _Args, _SId, _Context) ->
    ?LOG_WARNING(#{
        text => <<"OAuth2 access token with unknown return">>,
        in => zotonic_mod_oauth2,
        result => error,
        reason => unknown_data,
        service => ServiceMod,
        access_data => AccessData
    }),
    {error, access_token};
access_token({error, denied}, _ServiceMod, _Args, _SId, _Context) ->
    {error, denied};
access_token({error, _Reason}, _ServiceMod, _Args, _SId, _Context) ->
    {error, access_token}.

% Handle the #auth_validated{} record.
user_data({ok, Auth}, SId, Context) ->
    case z_notifier:first(Auth, Context) of
        undefined ->
            % No handler for auth, signups, or signup not accepted
            ?LOG_WARNING(#{
                text => <<"Undefined auth_user return for user">>,
                in => zotonic_mod_oauth2,
                result => error,
                reason => auth_user_undefined,
                props => Auth
            }),
            {error, auth_user_undefined};
        {ok, UserId} ->
            % Generate one time token to login for this user
            case z_authentication_tokens:encode_onetime_token(UserId, SId, Context) of
                {ok, Token} ->
                    {ok, #{
                        result => token,
                        token => Token
                    }};
                {error, Reason} = Err ->
                    ?LOG_WARNING(#{
                        text => <<"Error return for user">>,
                        in => zotonic_mod_oauth2,
                        result => error,
                        reason => Reason,
                        props => Auth
                    }),
                    Err
            end;
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
            ?LOG_NOTICE(#{
                text => <<"User with email already exists">>,
                in => zotonic_mod_oauth2,
                email => Email,
                result => error,
                reason => duplicate_email
            }),
            {error, duplicate_email};
        {error, Reason} ->
            ?LOG_WARNING(#{
                text => <<"Error return for user">>,
                in => zotonic_mod_oauth2,
                auth => Auth,
                result => error,
                reason => Reason
            }),
            {error, auth_user_error}
    end;
user_data(_UserError, _SId, _Context) ->
    {error, service_user_data}.

