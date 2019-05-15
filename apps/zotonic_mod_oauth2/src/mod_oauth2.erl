%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc OAuth2 (https://tools.ietf.org/html/draft-ietf-oauth-v2-26)

%% Copyright 2019 Marc Worrell
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

-module(mod_oauth2).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("OAuth2").
-mod_description("Provides authentication over OAuth2.").
-mod_prio(900).
-mod_schema(1).

-export([
    observe_request_context/3,
    manage_schema/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").


%% @doc Check if there is a valid Authorization header or 'access_token' argument.
-spec observe_request_context( #request_context{}, z:context(), z:context() ) -> z:context().
observe_request_context(#request_context{ phase = init }, Context, _Context) ->
    case z_context:get(anonymous, Context, false) of
        true ->
            Context;
        false ->
            case z_auth:is_auth(Context) of
                true ->
                    Context;
                false ->
                    try_auth(Context)
            end
    end;
observe_request_context(#request_context{ phase = _Phase }, Context, _Context) ->
    Context.

try_auth(Context) ->
    case cowmachine_req:get_req_header(<<"authorization">>, Context) of
        <<"Bearer ", Token/binary>> ->
            try_bearer(Token, Context);
        <<"bearer ", Token/binary>> ->
            try_bearer(Token, Context);
        _ ->
            case z_context:get_q(<<"access_token">>, Context) of
                undefined ->
                    Context;
                Token when is_binary(Token) ->
                    try_bearer(Token, Context)
            end
    end.

try_bearer(<<>>, Context) ->
    Context;
try_bearer(<<" ", Token/binary>>, Context) ->
    try_bearer(Token, Context);
try_bearer(Token, Context) ->
    case m_oauth2:decode_bearer_token(Token, Context) of
        {ok, {TokenId, TokenSecret}} ->
            try_token(TokenId, TokenSecret, Context);
        {error, unknown_token} ->
            % Somebody else's token - ignore
            Context;
        {error, Reason} ->
            % Illegal token, maybe throw a 400 here?
            lager:info("Could not decode OAuth2 token, error ~p for ~p", [ Reason, Token ]),
            Context
    end.

try_token(TokenId, TokenSecret, Context) ->
    case m_oauth2:get_token_access(TokenId, Context) of
        {ok, #{ secret := TokenSecret } = Token} ->
            #{
                user_id := UserId,
                user_groups := UserGroups,
                is_read_only := IsReadOnly,
                is_full_access := IsFullAccess
            } = Token,
            Options = case IsFullAccess of
                true ->
                    % No restriction on user groups
                    #{
                        is_read_only => IsReadOnly
                    };
                false ->
                    % Limited access, user groups will be filtered
                    #{
                        user_groups => UserGroups,
                        is_read_only => IsReadOnly
                    }
            end,
            case z_auth:is_enabled(UserId, Context) of
                true ->
                    % TODO: add a log entry for the request
                    z_acl:logon(UserId, Options, Context);
                false ->
                    % User is disabled, maybe throw a 403 here?
                    lager:info("Authenticated OAuth2 request for disabled user ~p with token ~p", [ UserId, TokenId ]),
                    Context
            end;
        {ok, _} ->
            % Mismatch on secret, maybe throw a 400 here?
            lager:info("Authenticated OAuth2 request with wrong secret ~p", [ TokenId ]),
            Context;
        {error, _} ->
            % Illegal token, maybe throw a 400 here?
            lager:info("Authenticated OAuth2 request for unknown token ~p", [ TokenId ]),
            Context
    end.


-spec manage_schema( z_module_manager:manage_schema(), z:context() ) -> ok.
manage_schema(Version, Context) ->
    m_oauth2:manage_schema(Version, Context).
