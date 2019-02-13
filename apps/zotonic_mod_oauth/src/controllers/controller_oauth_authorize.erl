%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2009-10-01
%% @doc Authorizing an OAuth request key

%% Copyright 2009 Arjan Scherpenisse
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

-module(controller_oauth_authorize).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
         is_authorized/1,
         resource_exists/1,
         allowed_methods/1,
         process/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").


allowed_methods(Context) ->
    {[<<"POST">>, <<"GET">>, <<"HEAD">>], Context}.

is_authorized(Context) ->
    {z_auth:is_auth(Context), Context}.

resource_exists(Context) ->
    Token = m_oauth_app:get_request_token(z_context:get_q(<<"oauth_token">>, Context), Context),
    case Token of
        undefined ->
            {false, Context};
        _ ->
            Context1 = z_context:set(token, Token, Context),
            {true, Context1}
    end.

process(<<"POST">>, _AcceptedCT, _ProvidedCT, Context) ->
    Token = z_context:get(token, Context),
    m_oauth_app:authorize_request_token(Token, z_acl:user(Context), Context),
    Redirect = case proplists:get_value(callback_uri, Token, <<>>) of
                   <<>> -> <<"/oauth/authorize/finished">>;
                   X when is_binary(X) -> X
               end,
    Redirect1 = iolist_to_binary([
                    Redirect,
                    <<"?oauth_token=">>,
                    z_url:url_encode(proplists:get_value(token, Token))
                ]),
    Context1 = z_context:set_resp_header(<<"location">>, Redirect1, Context),
    {{halt, 301}, Context1};
process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    Vars = [ {token, z_context:get(token, Context)} ],
    Html = z_template:render(<<"oauth_authorize.tpl">>, Vars, Context),
    z_context:output(Html, Context).

