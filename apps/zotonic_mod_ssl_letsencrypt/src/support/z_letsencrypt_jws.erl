%% Copyright 2015-2020 Guillaume Bour
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%% http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(z_letsencrypt_jws).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([init/1, encode/3, keyauth/2]).


% init(Key)
%
% Initialize a RSA JWS with given private key.
%
% returns:
%   JWS
-spec init(z_letsencrypt:ssl_privatekey()) -> z_letsencrypt:jws().
init(#{b64 := {N,E}}) ->
    #{
        alg => 'RS256',
        jwk =>  #{
            kty     => 'RSA',
            <<"n">> => N,
            <<"e">> => E
        },
        nonce => undefined
    }.

% encode(Key, Jws, Payload)
%
% Build Jws body.
% ref: https://www.rfc-editor.org/rfc/rfc8555.html#section-6.2
%
% returns:
%   JwsBody
%
-spec encode(z_letsencrypt:ssl_privatekey(), z_letsencrypt:jws(), map()|empty) -> binary().
encode(#{raw := RSAKey}, Jws, Content) ->
    Protected = z_letsencrypt_utils:b64encode(jsx:encode(Jws)),
    Payload = case Content of
        % for POST-as-GET queries, payload is just an empty string
        empty -> <<>>;
        _     -> z_letsencrypt_utils:b64encode(jsx:encode(Content))
    end,

    Sign  = crypto:sign(rsa, sha256, <<Protected/binary, $., Payload/binary>>, RSAKey),
    Sign2 = z_letsencrypt_utils:b64encode(Sign),

    jsx:encode(#{
        %{header, {[]}},
        <<"protected">> => Protected,
        <<"payload">> => Payload,
        <<"signature">> => Sign2
    }).

% keyauth(Key, Token)
%
% Build acme key authorization.
% ref: https://www.rfc-editor.org/rfc/rfc8555.html#section-8.1
%
% returns:
%   KeyAuthorization
%
keyauth(#{<<"e">> := E, <<"n">> := N, <<"kty">> := Kty}, Token) ->
    Thumbprint = jsx:encode(#{
        <<"e">> => E,
        <<"kty">> => Kty,
        <<"n">> => N
    }),
    <<Token/binary, $., (z_letsencrypt_utils:b64encode(crypto:hash(sha256, Thumbprint)))/binary>>.
