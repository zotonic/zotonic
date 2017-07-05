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
         allowed_methods/1,
         content_types_provided/1,
         response/1,
         process_post/1
        ]).

-include_lib("zotonic_core/include/zotonic.hrl").

allowed_methods(Context) ->
    {[<<"POST">>, <<"GET">>, <<"HEAD">>], Context}.

content_types_provided(Context) ->
    {[{<<"text/html">>, response}], Context}.

process_post(Context) ->
    response(Context).

response(Context) ->
    case mod_oauth:request_is_signed(Context) of
        false ->
            % Request was not signed.
            mod_oauth:authenticate(<<"Not an OAuth request.">>, Context);
        true ->
            mod_oauth:serve_oauth(Context,
                    fun(URL, Params, Consumer, Signature) ->
                          SigMethod = mod_oauth:oauth_param(<<"oauth_signature_method">>, Context),
                          case oauth:verify(
                                    Signature,
                                    z_convert:to_list(m_req:get(method, Context)),
                                    URL,
                                    Params,
                                    mod_oauth:to_oauth_consumer(Consumer, SigMethod),
                                    "")
                          of
                              true ->
                                  {ok, Token} = m_oauth_app:request_token(Consumer, Context),
                                  Context1 = cowmachine_req:set_resp_body(oauth:uri_params_encode([
                                            {"oauth_token", binary_to_list(proplists:get_value(token, Token))},
                                            {"oauth_token_secret", binary_to_list(proplists:get_value(token_secret, Token))}
                                        ]), Context),
                                  {{halt, 200}, Context1};
                              false ->
                                  mod_oauth:authenticate(<<"Signature verification failed.">>, Context)
                          end
                    end)
    end.

