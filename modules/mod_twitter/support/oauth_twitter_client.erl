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

-include_lib("zotonic.hrl").


-export([
         get_request_token/1,
         authorize_url/1,
         get_access_token/2,
         request/4,
         request/5
        ]).


get_request_token(Context) ->
  case oauth_get("http://twitter.com/oauth/request_token", [], get_consumer(Context), "", "") of
    {ok, Response} ->
      case oauth_http:response_code(Response) of
        200 ->
              P = oauth_http:response_params(Response),
              {ok, {oauth:token(P), oauth:token_secret(P)}};
        _ ->
          Response
      end;
    Error ->
      Error
  end.


authorize_url(Token) ->
  oauth:uri("http://twitter.com/oauth/authorize", [{"oauth_token", Token}]).


get_access_token({RequestToken, RequestSecret}, Context) ->
  case oauth_get("http://twitter.com/oauth/access_token", [], get_consumer(Context), RequestToken, RequestSecret) of
    {ok, Response} ->
      case oauth_http:response_code(Response) of
        200 ->
              P = oauth_http:response_params(Response),
              {ok, {oauth:token(P), oauth:token_secret(P)}};
        _ ->
          Response
      end;
    Error ->
      Error
  end.


request(get, ApiCall, {AccessToken, AccessSecret}, Context) ->
    request(get, ApiCall, [], {AccessToken, AccessSecret}, Context).
request(get, ApiCall, Params, {AccessToken, AccessSecret}, Context) ->
  case oauth_get("http://twitter.com/" ++ ApiCall ++ ".json", Params, get_consumer(Context), AccessToken, AccessSecret) of
      {ok, {{_, 200, _}, _Headers, Body}} ->
          {ok, z_convert:convert_json(mochijson2:decode(Body))};
    {ok, {{_, 401, _}, _Headers, _Body}} ->
          {error, unauthorized}
  end;
request(post, ApiCall, Params, {AccessToken, AccessSecret}, Context) ->
  case oauth_post("http://twitter.com/" ++ ApiCall ++ ".json", Params, get_consumer(Context), AccessToken, AccessSecret) of
      {ok, {{_, 200, _}, _Headers, Body}} ->
          {ok, z_convert:convert_json(mochijson2:decode(Body))};
      {ok, {{_, 401, _}, _Headers, _Body}} ->
          {error, unauthorized}
  end.


%% get_direct_messages(Client) ->
%%   URL = "http://twitter.com/direct_messages.xml",
%%   oauth_client:get(Client, URL, []).


get_consumer(Context) ->
    case {m_config:get_value(mod_twitter, consumer_key, undefined, Context), m_config:get_value(mod_twitter, consumer_secret, undefined, Context)} of
        {undefined, _} ->
            undefined;
        {CKey, CSec} ->
            {z_convert:to_list(CKey), z_convert:to_list(CSec), hmac_sha1}
    end.




oauth_get(URL, Params, Consumer, Token, TokenSecret) ->
  Signed = oauth:signed_params("GET", URL, Params, Consumer, Token, TokenSecret),
  {AuthorizationParams, QueryParams} = lists:partition(fun({K, _}) -> lists:prefix("oauth_", K) end, Signed),
  Request = {oauth:uri(URL, QueryParams), [oauth:header(AuthorizationParams)]},
  http:request(get, Request, [{autoredirect, false}], []).

oauth_post(URL, Params, Consumer, Token, TokenSecret) ->
    Signed = oauth:signed_params("POST", URL, Params, Consumer, Token, TokenSecret),
    Body = oauth_uri:params_to_string(Signed),
    Request = {URL, [], "application/x-www-form-urlencoded", Body},
    http:request(post, Request, [{autoredirect, false}], []).
