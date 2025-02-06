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

-module(z_letsencrypt_utils).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([b64encode/1, hexdigest/1, hashdigest/2, bin/1, str/1]).

-type character() :: integer().

-spec b64encode(string()|binary()) -> binary().
b64encode(X) ->
    Base64 = base64:encode(X),
    << <<(encode_byte(B)):8>> || <<B:8>> <= Base64, B =/= $= >>.


-spec encode_byte(character()) -> character().
encode_byte($+) -> $-;
encode_byte($/) -> $_;
encode_byte(B) -> B.


-spec hexdigest(string()|binary()) -> binary().
hexdigest(X) ->
    << <<(hex(H)),(hex(L))>> || <<H:4,L:4>> <= X >>.

hex(C) when C < 10 -> $0 + C;
hex(C)             -> $a + C - 10.

% returns hexadecimal digest of SHA256 hashed content
-spec hashdigest(sha256, binary()) -> binary().
hashdigest(sha256, Content) ->
    hexdigest(crypto:hash(sha256, Content)).


-spec bin( binary() | string() | atom() ) -> binary().
bin(X) when is_binary(X) ->
    X;
bin(X) when is_list(X) ->
    case unicode:characters_to_binary(X) of
        B when is_binary(B) -> B;
        _ -> throw(invalid)
    end;
bin(X) ->
    z_convert:to_binary(X).

-spec str( binary() | string() | integer() ) -> string().
str(X) when is_binary(X); is_list(X) ->
    case unicode:characters_to_list(X) of
        L when is_list(L) -> L;
        _ -> throw(invalid)
    end;
str(X) when is_integer(X) ->
    z_convert:to_list(X).

