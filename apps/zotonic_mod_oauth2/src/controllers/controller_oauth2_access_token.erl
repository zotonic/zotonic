%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021-2025 Marc Worrell
%% @doc Exchange an authorization code for an access token.
%% @end

%% Copyright 2021-2025 Marc Worrell
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

-module(controller_oauth2_access_token).
-moduledoc("
Todo

Not yet documented.
").
-author("Marc Worrell <marc@worrell.nl>").

-export([
    service_available/1,
    allowed_methods/1,
    content_types_provided/1,
    process/4
    ]).


service_available(Context) ->
    Context1 = z_context:set_noindex_header(Context),
    Context2 = z_context:set_nocache_headers(Context1),
    {true, Context2}.

allowed_methods(Context) ->
    {[ <<"POST">>, <<"GET">> ], Context}.

content_types_provided(Context) ->
    {[ {<<"application">>, <<"json">>, []} ], Context}.

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    Data = process_1(z_context:get_q(<<"grant_type">>, Context), Context),
    {jsxrecord:encode(Data), Context}.

process_1(<<"authorization_code">>, Context) ->
    ClientId = z_context:get_q(<<"client_id">>, Context),
    ClientSecret = z_context:get_q(<<"client_secret">>, Context),
    RedirectURL = z_context:get_q(<<"redirect_uri">>, Context),
    Code = z_context:get_q(<<"code">>, Context),
    case m_oauth2:exchange_accept_code(ClientId, ClientSecret, RedirectURL, Code, Context) of
        {ok, {AccessToken, UserId}} ->
            #{
                <<"access_token">> => AccessToken,
                <<"token_type">> => <<"Bearer">>,
                <<"user_id">> => UserId,
                <<"user">> => user(UserId, Context)
            };
        {error, Reason} ->
            return_error(Reason)
    end;
process_1(<<"client_credentials">>, Context) ->
    ClientId = z_context:get_q(<<"client_id">>, Context),
    ClientSecret = z_context:get_q(<<"client_secret">>, Context),
    case m_oauth2:exchange_client_credentials(ClientId, ClientSecret, Context) of
        {ok, {AccessToken, Expires, UserId}} when Expires =:= 0; Expires =:= undefined ->
            #{
                <<"access_token">> => AccessToken,
                <<"token_type">> => <<"Bearer">>,
                <<"user_id">> => UserId,
                <<"user">> => user(UserId, Context)
            };
        {ok, {AccessToken, Expires, UserId}} ->
            #{
                <<"access_token">> => AccessToken,
                <<"token_type">> => <<"Bearer">>,
                <<"expires_in">> => Expires,
                <<"user_id">> => UserId,
                <<"user">> => user(UserId, Context)
            };
        {error, Reason} ->
            return_error(Reason)
    end;
process_1(_, _Context) ->
    #{
        <<"error">> => <<"grant_type">>,
        <<"error_reason">> => <<"wrong_grant_type">>,
        <<"error_description">> => <<"Grant type must be 'authorization_code' or 'client_credentials'">>
    }.

return_error(disabled) ->
    #{
        <<"error">> => <<"client_id">>,
        <<"error_reason">> => <<"disabled">>,
        <<"error_description">> => <<"The client_id is disabled">>
    };
return_error(secret) ->
    #{
        <<"error">> => <<"client_secret">>,
        <<"error_reason">> => <<"nonmatching_secret">>,
        <<"error_description">> => <<"The passed client_secret does not match">>
    };
return_error(code) ->
    #{
        <<"error">> => <<"code">>,
        <<"error_reason">> => <<"unknown_code">>,
        <<"error_description">> => <<"The passed code was unknown">>
    };
return_error(mismatch) ->
    #{
        <<"error">> => <<"code">>,
        <<"error_reason">> => <<"nonmatch_code">>,
        <<"error_description">> => <<"The passed code did not match the client_id or redirect_uri">>
    };
return_error(enoent) ->
    #{
        <<"error">> => <<"client_id">>,
        <<"error_reason">> => <<"unknown_client">>,
        <<"error_description">> => <<"The passed client_id was unknown">>
    };
return_error(_) ->
    #{
        <<"error">> => <<"error">>,
        <<"error_reason">> => <<"unknown">>,
        <<"error_description">> => <<"Could not exchange the code">>
    }.

user(UserId, Context) ->
    ContextUser = z_acl:logon(UserId, Context),
    #{
        <<"uri">> => m_rsc:uri(UserId, ContextUser),
        <<"is_authoritative">> => m_rsc:p(UserId, is_authoritative, ContextUser),
        <<"title">> => m_rsc:p(UserId, title, ContextUser),
        <<"summary">> => m_rsc:p(UserId, summary, ContextUser),
        <<"name_first">> => m_rsc:p(UserId, name_first, ContextUser),
        <<"name_surname_prefix">> => m_rsc:p(UserId, name_surname_prefix, ContextUser),
        <<"name_surname">> => m_rsc:p(UserId, name_surname, ContextUser),
        <<"email">> => m_rsc:p(UserId, email_raw, ContextUser),
        <<"address_country">> => m_rsc:p(UserId, address_country, ContextUser),
        <<"pref_language">> => m_rsc:p(UserId, pref_language, ContextUser),
        <<"pref_tz">> => m_rsc:p(UserId, pref_tz, ContextUser)
    }.
