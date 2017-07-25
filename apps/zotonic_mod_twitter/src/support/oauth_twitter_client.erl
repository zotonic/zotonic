%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2011-09-22
%% @doc Redirect to the authorize uri of Twitter
%% See: http://developers.facebook.com/docs/authentication/

%% Copyright 2011 Arjan Scherpenisse
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

-module(oauth_twitter_client).

-include_lib("zotonic_core/include/zotonic.hrl").


-export([
         get_request_token/1,
         get_consumer/1,
         authorize_url/1,
         get_access_token/2,
         request/4,
         request/5
        ]).


get_request_token(Context) ->
  case oauth:get("https://api.twitter.com/oauth/request_token", [], get_consumer(Context), "", "") of
      {ok, Response = { {_, 200, _}, _, _}} ->
          P = oauth:params_decode(Response),
          {ok, {oauth:token(P), oauth:token_secret(P)}};
      Error ->
          Error
  end.


authorize_url(Token) ->
  oauth:uri("https://api.twitter.com/oauth/authorize", [{"oauth_token", Token}]).


get_access_token({RequestToken, RequestSecret}, Context) ->
    Verifier = z_context:get_q("oauth_verifier", Context),
    Params = case z_utils:is_empty(Verifier) of
                 false -> [{"oauth_verifier", Verifier}];
                 true -> []
             end,
    case oauth:get("https://api.twitter.com/oauth/access_token", Params, get_consumer(Context), RequestToken, RequestSecret) of
        {ok, Response = { {_, 200, _}, _, _}} ->
            P = oauth:params_decode(Response),
            {ok, {oauth:token(P), oauth:token_secret(P)}};
        Error ->
            Error
    end.


request(get, ApiCall, {AccessToken, AccessSecret}, Context) ->
    request(get, ApiCall, [], {AccessToken, AccessSecret}, Context).
request(get, ApiCall, Params, {AccessToken, AccessSecret}, Context) ->
    handle_result(oauth:get("https://api.twitter.com/1.1/" ++ ApiCall ++ ".json", Params, get_consumer(Context), AccessToken, AccessSecret));
request(post, ApiCall, Params, {AccessToken, AccessSecret}, Context) ->
  handle_result(oauth:post("https://api.twitter.com/1.1/" ++ ApiCall ++ ".json", Params, get_consumer(Context), AccessToken, AccessSecret)).

handle_result({ok, {{_, 200, _}, _Headers, Body}}) ->
    {ok, z_convert:convert_json(mochijson2:decode(Body))};
handle_result({ok, {{_, 401, _}, _Headers, _Body}}) ->
    {error, unauthorized};
handle_result({ok, {{_, 404, _}, _Headers, _Body}}) ->
    {error, notfound};
handle_result({ok, {{_, 420, _}, _Headers, _Body}}) ->
    {error, connection_limit};
handle_result({ok, {{_, 429, _}, _Headers, _Body}}) ->
    {error, rate_limit};
handle_result({ok, {{_, Code, _}, _Headers, _Body}}) ->
    {error, {code, Code}};
handle_result({error, _} = Error) ->
    Error.


get_consumer(Context) ->
    case {m_config:get_value(mod_twitter, consumer_key, undefined, Context),
          m_config:get_value(mod_twitter, consumer_secret, undefined, Context)}
    of
        {None, _} when None =:= undefined; None =:= <<>> ->
            undefined;
        {_, None} when None =:= undefined; None =:= <<>> ->
            undefined;
        {CKey, CSec} ->
            {z_convert:to_list(CKey), z_convert:to_list(CSec), hmac_sha1}
    end.
