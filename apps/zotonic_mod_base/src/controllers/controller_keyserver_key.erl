%% @author Maas-Maarten Zeeman <maas@channel.me>
%% @copyright 2018 Maas-Maarten Zeeman <maas@channel.me>
%% @doc Retrieve the public key of the keyserver.

%% Copyright 2018 Maas-Maarten Zeeman
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

-module(controller_keyserver_key).
-author("Maas-Maarten Zeeman <maas@channel.me>").

-export([
    content_types_provided/1,
    process/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

content_types_provided(Context) ->
    {[ {<<"application">>, <<"javascript">>, []} ], Context}.

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    Context1 = z_context:set_noindex_header(Context),
    Context2 = set_cache_header(Context1),
    
    KeyServerName = z_utils:name_for_site(keyserver, z_context:site(Context)), 
    {ok, [PublicExponent, Modulus]} = keyserver:public_enc_key(KeyServerName),
    
    E = base64url:encode(PublicExponent),
    N = base64url:encode(Modulus),

    z_context:output(["keyserver_public_encrypt_key = {\"kty\": \"RSA\", \"e\":\"", E, "\", \"n\": \"", N, "\"}"], Context2).

set_cache_header(Context) ->
    z_context:set_resp_header(
      <<"cache-control">>,
      <<"public, max-age=600">>,
      Context).
