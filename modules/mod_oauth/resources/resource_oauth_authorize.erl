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

-module(resource_oauth_authorize).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
         is_authorized/2,
         resource_exists/2,
         allowed_methods/2,
         process_post/2
]).

-include_lib("resource_html.hrl").


allowed_methods(ReqData, Context) ->
    {['POST', 'GET', 'HEAD'], ReqData, Context}.


is_authorized(ReqData, Context) ->
    z_acl:wm_is_authorized(use, mod_admin, ReqData, Context).


resource_exists(ReqData, Context) ->
    Token = m_oauth_app:get_request_token(z_context:get_q("oauth_token", Context), Context),
    case Token of
        undefined ->
            {false, ReqData, Context};
        _ ->
            Context1 = z_context:set("token", Token, Context),
            {true, ReqData, Context1}
    end.


html(Context) ->
    Vars = [ {token, z_context:get("token", Context)} ],
    Html = z_template:render("oauth_authorize.tpl", Vars, Context),
	z_context:output(Html, Context).


process_post(ReqData, Context) ->
    Token = z_context:get("token", Context),
    ?DEBUG(Token),
    m_oauth_app:authorize_request_token(Token, Context#context.user_id, Context),
    Redirect = case z_db:get(callback_uri, Token) of
                   <<>> ->
                       "/oauth/authorize/finished";
                   X when is_binary(X) ->
                       binary_to_list(X)
               end,
    Redirect1 = Redirect ++ "?oauth_token=" ++ oauth_uri:encode(binary_to_list(z_db:get(token, Token))),
    ReqData1 = wrq:set_resp_header("Location", Redirect1, ReqData),
    {{halt, 301}, ReqData1, Context}.
