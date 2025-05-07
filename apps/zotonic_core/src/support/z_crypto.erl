%% @author Marc Worrell
%% @copyright 2023-2025 Marc Worrell
%% @doc Crypto related functions for checksums and signatures.
%% @end

%% Copyright 2023-2025 Marc Worrell
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

-module(z_crypto).
-include("zotonic.hrl").

-export([
    auth_hash/3,

    encode_value/2,
    encode_value_expire/3,

    decode_value/2,
    decode_value_expire/2,

    checksum/2,
    checksum_assert/3,

    hex_sha/1,
    hex_sha2/1,
    hex_sha2_file/1,

    pickle/2,
    depickle/2
    ]).

% The hash algorithms used for the various encodings.

-define(AUTH_HASH_ALGORITHM, sha256).
-define(CHECKSUM_HASH_ALGORITHM, sha256).

-define(PICKLE_HASH_ALGORITHM, sha256).
-define(PICKLE_HASH_BYTES, 32).

-define(ENCODE_HASH_ALGORITHM, sha).
-define(ENCODE_HASH_BYTES, 20).


%% @doc Hash a value for authentication. This is used for signing payload data in
%% user authentication flows. The data is signed using the concatenation of the
%% Site secret and the user secret. Changing any of those will invalidate all
%% signed data (cookies, autologon secrets etc.) for the user.
-spec auth_hash(SiteSecret, UserSecret, Data) -> Hash when
    SiteSecret :: binary(),
    UserSecret :: binary(),
    Data :: iodata(),
    Hash :: binary().
auth_hash(SiteSecret, UserSecret, Data) ->
    crypto:mac(hmac, ?AUTH_HASH_ALGORITHM, <<SiteSecret/binary, UserSecret/binary>>, Data).


%% @doc Encode value to a binary with a checksum, for use in cookies.
encode_value(Value, #context{} = Context) ->
    encode_value(Value, z_ids:sign_key(Context));
encode_value(Value, Secret) when is_list(Secret); is_binary(Secret) ->
    Salt = z_ids:rand_bytes(4),
    BinVal = erlang:term_to_binary(Value),
    Hash = crypto:mac(hmac, ?ENCODE_HASH_ALGORITHM, Secret, [ BinVal, Salt ]),
    base64:encode(iolist_to_binary([ 1, Salt, Hash, BinVal ])).

%% @doc Decode a value. Crash if the checksum is invalid.
decode_value(Data, #context{} = Context) ->
    decode_value(Data, z_ids:sign_key(Context));
decode_value(Data, Secret) when is_list(Secret); is_binary(Secret) ->
    <<1, Salt:4/binary, Hash:?ENCODE_HASH_BYTES/binary, BinVal/binary>> = base64:decode(Data),
    Hash = crypto:mac(hmac, ?ENCODE_HASH_ALGORITHM, Secret, [ BinVal, Salt ]),
    erlang:binary_to_term(BinVal).

%% @doc Encode a value using a checksum, add a date to check for expiration.
-spec encode_value_expire(Value, Date, Context) -> Encoded when
    Value :: term(),
    Date :: calendar:datetime(),
    Context :: z:context(),
    Encoded :: binary().
encode_value_expire(Value, Date, Context) ->
    encode_value({Value, Date}, Context).

%% @doc Decode a value using a checksum, check date to check for expiration. Crashes
%% if the checksum is invalid.
-spec decode_value_expire(Encoded, Context) -> {ok, Value} | {error, expired} when
    Encoded :: binary(),
    Value :: term(),
    Context :: z:context().
decode_value_expire(Data, Context) ->
    {Value, Expire} = decode_value(Data, Context),
    case Expire >= calendar:universal_time() of
        false -> {error, expired};
        true -> {ok, Value}
    end.


%%% CHECKSUM %%%

%% @doc Calculate a checksum for the given data using the sign_key_simple of the site.
-spec checksum(Data, Context) -> Checksum when
    Data :: iodata(),
    Context :: z:context(),
    Checksum :: binary().
checksum(Data, Context) ->
    Key = z_ids:sign_key_simple(Context),
    Checksum = crypto:mac(hmac, ?CHECKSUM_HASH_ALGORITHM, Key, Data),
    base64url:encode(Checksum).

%% @doc Assert that the checksum is correct. Throws an exception of class error with
%% reason checksum_invalid if the checksum is not valid. The sign_key_simple if used
%% for the checksum calculation.
-spec checksum_assert(Data, Checksum, Context) -> ok | no_return() when
    Data :: iodata(),
    Checksum :: binary() | string(),
    Context :: z:context().
checksum_assert(Data, Checksum, Context) when is_binary(Checksum )->
    try
        Key = z_ids:sign_key_simple(Context),
        Decoded = base64url:decode(Checksum),
        Decoded = crypto:mac(hmac, ?CHECKSUM_HASH_ALGORITHM, Key, Data),
        ok
    catch
        error:_ ->
            erlang:error(checksum_invalid)
    end;
checksum_assert(Data, Checksum, Context) ->
    checksum_assert(Data, z_convert:to_binary(Checksum), Context).

%% @doc Hash data and encode into a hex string safe for filenames and texts.
-spec hex_sha(Value) -> Hash when
    Value :: iodata(),
    Hash :: binary().
hex_sha(Value) ->
    z_url:hex_encode_lc(crypto:hash(sha, Value)).

%% @doc Hash256 data and encode into a hex string safe for filenames and texts.
-spec hex_sha2(Value) -> Hash when
    Value :: iodata(),
    Hash :: binary().
hex_sha2(Value) ->
    z_url:hex_encode_lc(crypto:hash(sha256, Value)).

%% @doc Calculate the hash of a file by incrementally reading it.
-spec hex_sha2_file(File) -> {ok, Hash} | {error, Reason} when
    File :: file:filename_all(),
    Hash :: binary(),
    Reason :: file:posix() | term().
hex_sha2_file(File) ->
    Hash = crypto:hash_init(sha256),
    case file:open(File, [read, binary]) of
        {ok, Fh} ->
            try
                case hash_file(Fh, Hash) of
                    {ok, Hash1} ->
                        Digest = crypto:hash_final(Hash1),
                        {ok, z_url:hex_encode_lc(Digest)};
                    {error, _} = Error ->
                        Error
                end
            after
                file:close(Fh)
            end;
        {error, _} = Error ->
            Error
    end.

hash_file(Fh, Hash) ->
    case file:read(Fh, 4096) of
        {ok, Bin} ->
            Hash1 = crypto:hash_update(Hash, Bin),
            hash_file(Fh, Hash1);
        eof ->
            {ok, Hash};
        {error, _} = Error ->
            Error
    end.

%%% PICKLE / UNPICKLE %%%

%% @doc Encode an arbitrary to a binary. A checksum is added to prevent
%% decoding erlang terms not originating from this server. An Nonce is
%% added so that identical terms vary in their checksum. The encoded value
%% is safe to use in URLs (base64url). The site's sign_key is used as the
%% secret.
-spec pickle(Term, Context) -> Data when
    Term :: term(),
    Context :: z:context(),
    Data :: binary().
pickle(Data, Context) ->
    TermData = erlang:term_to_binary(Data),
    Nonce = z_ids:rand_bytes(4),
    Key  = z_ids:sign_key(Context),
    SignedData = <<Nonce:4/binary, TermData/binary>>,
    Hash = crypto:mac(hmac, ?PICKLE_HASH_ALGORITHM, Key, SignedData),
    base64url:encode(<<Hash:?PICKLE_HASH_BYTES/binary, SignedData/binary>>).

%% @doc Decode pickled base64url data. If the data checksum is invalid then an exception
%% of class error with reason checksum_invalid is thrown. The site's sign_key is
%% used as the secret.
-spec depickle(Data, Context) -> Term | no_return() when
    Data :: binary(),
    Context :: z:context(),
    Term :: term().
depickle(Data, Context) ->
    try
        <<Hash:?PICKLE_HASH_BYTES/binary, SignedData/binary>> = base64url:decode(Data),
        Key = z_ids:sign_key(Context),
        DataHash = crypto:mac(hmac, ?PICKLE_HASH_ALGORITHM, Key, SignedData),
        if
            Hash =:= DataHash ->
                <<_Nonce:4/binary, TermData/binary>> = SignedData,
                erlang:binary_to_term(TermData);
            true ->
                erlang:error(checksum_invalid)
        end
    catch
        E:R:S ->
            ?LOG_ERROR(#{
                in => zotonic_core,
                text => <<"Postback data invalid, could not depickle">>,
                result => E,
                reason => R,
                stack => S
            }),
            erlang:error(checksum_invalid)
    end.
