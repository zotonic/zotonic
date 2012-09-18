%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2009-10-01
%% @doc Entrypoint for obtaining an oauth access token.

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

-module(controller_oauth_request_token).

-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
         init/1,
         resource_exists/2,
         allowed_methods/2,
         content_types_provided/2,
         response/2,
         process_post/2
        ]).

-include_lib("webmachine_controller.hrl").
-include_lib("zotonic.hrl").



init([]) -> 
    {ok, []}.


resource_exists(ReqData, _Context) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:ensure_qs(Context),
    {true, ReqData, Context1}.


allowed_methods(ReqData, Context) ->
    {['POST', 'GET', 'HEAD'], ReqData, Context}.


content_types_provided(ReqData, Context) ->
    {[{"text/html", response}], ReqData, Context}.


process_post(ReqData, Context) ->
    response(ReqData, Context).

response(ReqData, Context) ->
    case mod_oauth:request_is_signed(ReqData) of
        false ->
            % Request was not signed.
            mod_oauth:authenticate("Not an OAuth request.", ReqData, Context);
        true ->
            mod_oauth:serve_oauth(ReqData, Context, 
                          fun(URL, Params, Consumer, Signature) ->
                                  %Token = m_oauth_app:secrets_for_verify(none, Consumer, mod_oauth:oauth_param("oauth_token", ReqData), Context),
                                  SigMethod = mod_oauth:oauth_param("oauth_signature_method", ReqData),
                                  case oauth:verify(Signature, atom_to_list(ReqData#wm_reqdata.method), URL,
                                                    Params, mod_oauth:to_oauth_consumer(Consumer, SigMethod), "") of
                                      true ->
                                          {ok, Token} = m_oauth_app:request_token(Consumer, Context),
                                          ReqData1 = wrq:set_resp_body(oauth_uri:params_to_string(
                                                                         [{"oauth_token", binary_to_list(z_db:get(token, Token))},
                                                                          {"oauth_token_secret", binary_to_list(z_db:get(token_secret, Token))}]), ReqData),
                                          {{halt, 200}, ReqData1, Context};
                                      false ->
                                          mod_oauth:authenticate("Signature verification failed.", ReqData, Context)
                                  end
                          end
                         )
    end.

