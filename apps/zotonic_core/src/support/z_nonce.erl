%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc Nonce support. Create nonces, and track nonce usage. Used nonce
%% values are tracked for 15 minutes. Nonce values themselves are also
%% typically valid for 15 minutes, unless another period is requested.
%% @end

%% Copyright 2025 Marc Worrell
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

-module(z_nonce).
-author("Marc Worrell <marc@worrell.nl>").

-include("zotonic.hrl").

-export([
    init_nonce_tables/0,
    nonce_secure/0,
    nonce/0,
    nonce/1,
    register/1,
    register_any/1,
    unregister/1,
    unregister_any/1,
    is_registered/1,
    is_registered_any/1,

    nonce_cleanup/0,
    nonce_secret/0,
    nonce_generation/1,
    nonce_oldest_generation/0
]).


% Nonce tables, cycling through these, ensuring that after NONCE_TIMEOUT
% they are all emptied. Generation garbage collection by periodically
% emptying the oldest table.
-define(NONCE_TABLES, [
        z_nonce_0, z_nonce_1, z_nonce_2, z_nonce_3, z_nonce_4,
        z_nonce_5, z_nonce_6, z_nonce_7, z_nonce_8, z_nonce_9
    ]).

% Number of seconds an nonce is valid. This can vary 10% due to the
% periodic cleanup of the nonce tables. The 1000 sec, with a variety
% of 10% gives 15 minutes reuse checks.
-define(NONCE_TIMEOUT, 1000).

% Default validity of a generated nonce value, must be used within this
% period otherwise the key will be expired. This period is shorter than
% the max period we keep registered (used) keys in the generational ets
% tables so that we can guarantee the unique use of these keys.
-define(NONCE_VALIDITY, 900).

% Maximum number of nonce tokens allowed in a single generational nonce
% table. The routines will give an overload error if passed this.
-define(NONCE_OVERLOAD, 10_000_000).

% Maximum size for random nonce values from other services.
-define(NONCE_ANY_MAX_SIZE, 50).


%%%--------------------------------------------------------------------------
%%% API
%%%--------------------------------------------------------------------------

-spec init_nonce_tables() -> ok.
%% @doc Initialize the nonce tables. Calles by zotonic_core_sup, which process
%% will be the owner of these tables. Force a reset of the secure nonce secret
%% as the nonce tables are re-initialized.
init_nonce_tables() ->
    % Force reinit of secure secret, as the ets tables are emptied.
    application:set_env(zotonic_core, nonce_secure_secret, <<>>),
    % Start generational cleanup function.
    SecsPerTable = ?NONCE_TIMEOUT div length(?NONCE_TABLES),
    timer:apply_interval((SecsPerTable div 2) * 1000, ?MODULE, nonce_cleanup, []),
    lists:foreach(
        fun(N) ->
            ets:new(N, [ named_table, public ])
        end,
        ?NONCE_TABLES).

-spec nonce_secure() -> Nonce when
    Nonce :: binary().
%% @doc Return a new nonce value, strictly valid for the how long we keep the
%% used nonce values. The keys are not valid between restarts, this gives maximum
%% protection against replay attacks.
nonce_secure() ->
    Number = nonce_generation(?NONCE_VALIDITY),
    Random = z_ids:id(10),
    Key = <<"s", (integer_to_binary(Number))/binary, "-", Random/binary>>,
    Hash = crypto:mac(hmac, sha256, Key, nonce_secure_secret()),
    HashHex = base64url:encode(Hash),
    <<Key/binary, "-", HashHex/binary>>.


-spec nonce() -> Nonce when
    Nonce :: binary().
%% @doc Return a new nonce value, strictly valid for the how long we keep the
%% used nonce values. The key is valid across restarts, registered keys could
%% be re-used after a restart.
nonce() ->
    nonce(?NONCE_VALIDITY).

-spec nonce(Timeout) -> Nonce when
    Timeout :: integer(),
    Nonce :: binary().
%% @doc Return a new nonce value, valid for about the given number of seconds. The key
%% is valid across restarts, registered keys could be re-used after a restart.
nonce(Timeout) ->
    Number = nonce_generation(Timeout),
    Random = z_ids:id(10),
    Key = <<(integer_to_binary(Number))/binary, "-", Random/binary>>,
    Hash = crypto:mac(hmac, sha256, Key, nonce_secret()),
    HashHex = base64url:encode(Hash),
    <<Key/binary, "-", HashHex/binary>>.

-spec register(Nonce) -> ok | {error, Reason} when
    Nonce :: binary(),
    Reason :: duplicate | overload | key | expired.
%% @doc Register a nonce for use, will be remembered for the next ?NONCE_TIMEOUT
%% seconds or until unregistered. The nonce must be generated with nonce/1 and
%% will be checked if it was expired.
register(<<>>) ->
    {error, key};
register(Nonce) ->
    Oldest = nonce_oldest_generation(),
    case split_nonce(Nonce) of
        {Prefix, Number, Random, Hash, Secret} when Number >= Oldest ->
            CheckKey = <<Prefix/binary, (integer_to_binary(Number))/binary, "-", Random/binary>>,
            CheckHash = crypto:mac(hmac, sha256, CheckKey, Secret),
            if
                CheckHash =:= Hash ->
                    register_1(CheckKey);
                true ->
                    {error, key}
            end;
        {_Prefix, _Number, _Random, _Key, _Secret} ->
            {error, expired};
        error ->
            {error, key}
    end.

-spec register_any(Nonce) -> ok | {error, Reason} when
    Nonce :: binary(),
    Reason :: duplicate | overload | key | expired.
%% @doc Register a nonce for use, will be remembered for the next ?NONCE_TIMEOUT
%% seconds or until unregistered. The nonce can be any binary smaller than 50
%% bytes.
register_any(Nonce) when size(Nonce) =< ?NONCE_ANY_MAX_SIZE ->
    register_1(Nonce);
register_any(_Nonce) ->
    {error, key}.


register_1(Key) ->
    case is_registered_any(Key) of
        false ->
            Table = nonce_table(),
            Size = ets:info(Table, size),
            if
                Size < ?NONCE_OVERLOAD ->
                    ets:insert(Table, {Key, secs()}),
                    ok;
                true ->
                    ?LOG_ERROR(#{
                        in => zotonic_core,
                        text => <<"Nonce table overload">>,
                        result => error,
                        reason => overload,
                        table => Table,
                        size => Size
                    }),
                    {error, overload}
            end;
        true ->
            {error, duplicate}
    end.

split_nonce(Nonce) ->
    case binary:split(Nonce, <<"-">>) of
        [ Number, KeyHash ] ->
            case binary:split(KeyHash, <<"-">>) of
                [Key, Hash] ->
                    try
                        Hash1 = base64url:decode(Hash),
                        case Number of
                            <<"s", Number1/binary>> ->
                                {<<"s">>, binary_to_integer(Number1), Key, Hash1, nonce_secure_secret()};
                            _ ->
                                {<<>>, binary_to_integer(Number), Key, Hash1, nonce_secret()}
                        end
                    catch _:_ ->
                        error
                    end;
                _ ->
                    error
            end;
        _ ->
            error
    end.


-spec unregister(Nonce) -> ok | {error, key} when
    Nonce :: binary().
%% @doc Forget about a nonce. Removes it from the registered nonce values. The
%% nonce must have been created by nonce/1
unregister(Nonce) ->
    case split_nonce(Nonce) of
        {Prefix, Number, Random, _Hash, _Secret} ->
            CheckKey = <<Prefix/binary, (integer_to_binary(Number))/binary, "-", Random/binary>>,
            unregister_any(CheckKey);
        _ ->
            {error, key}
    end.

-spec unregister_any(Nonce) -> ok when
    Nonce :: binary().
%% @doc Forget about a nonce. Removes it from the registered nonce values. The nonce
%% can be any key.
unregister_any(Nonce) ->
    lists:foreach(fun(T) -> ets:delete(T, Nonce) end, ?NONCE_TABLES).

-spec is_registered(Nonce) -> boolean() when
    Nonce :: binary().
%% @doc Check if the nonce has been registered in the last ?NONCE_TIMEOUT seconds
%% The nonce must have been created by nonce/1.
is_registered(Nonce) ->
    case split_nonce(Nonce) of
        {Prefix, Number, Random, _Hash, _Key} ->
            CheckKey = <<Prefix/binary, (integer_to_binary(Number))/binary, "-", Random/binary>>,
            is_registered_any(CheckKey);
        _ ->
            false
    end.

-spec is_registered_any(Nonce) -> boolean() when
    Nonce :: binary().
%% @doc Check if the nonce has been registered in the last ?NONCE_TIMEOUT seconds. The nonce
%% can be any key.
is_registered_any(Nonce) ->
    case lists:foldl(
        fun
            (T, []) -> ets:lookup(T, Nonce);
            (_, Acc) -> Acc
        end,
        [],
        ?NONCE_TABLES)
    of
        [] -> false;
        _ -> true
    end.

-spec nonce_cleanup() -> ok.
%% @doc Remove all keys from the "previous" nonce registration table,
%% schedule a timer for the next removal.
nonce_cleanup() ->
    ets:delete_all_objects(nonce_oldest_table()),
    ok.

nonce_table() ->
    N = nonce_generation(0),
    N1 = N rem length(?NONCE_TABLES),
    lists:nth(N1+1, ?NONCE_TABLES).

nonce_oldest_table() ->
    NTab = length(?NONCE_TABLES),
    N = nonce_generation(0) - NTab + 1,
    N1 = N rem NTab,
    lists:nth(N1+1, ?NONCE_TABLES).

% Return the current "generation" of the nonce values.
nonce_generation(DeltaSecs) ->
    SecsPerTable = ?NONCE_TIMEOUT div length(?NONCE_TABLES),
    (secs() + DeltaSecs) div SecsPerTable.

nonce_oldest_generation() ->
    nonce_generation(0) - length(?NONCE_TABLES) + 1.

secs() ->
    {MSecs, Secs, _} = os:timestamp(),
    MSecs * 1_000_000 + Secs.

-spec nonce_secret() -> binary().
%% @doc Return the secret used for signing the nonce values returned by nonce/0 and nonce/1
nonce_secret() ->
    case application:get_env(zotonic_core, nonce_secret) of
        {ok, Key} when size(Key) >= 50 ->
            Key;
        _ ->
            jobs:run(zotonic_singular_job, fun generate_nonce_secret/0),
            nonce_secret()
    end.

generate_nonce_secret() ->
    case application:get_env(zotonic_core, nonce_secret) of
        {ok, Key} when size(Key) >= 50 ->
            ok;
        _ ->
            SecFile = filename:join([ z_config:get(security_dir), "nonce-secret.bin" ]),
            case file:read_file(SecFile) of
                {ok, Key} when size(Key) >= 50 ->
                    application:set_env(zotonic_core, nonce_secret, Key),
                    Key;
                _ ->
                    Key = z_ids:rand_bytes(50),
                    file:write_file(SecFile, Key),
                    application:set_env(zotonic_core, nonce_secret, Key),
                    Key
            end
    end.

-spec nonce_secure_secret() -> binary().
%% @doc Return the secret used for signing the nonce values returned by nonce_secure/1
%% This secret is regenerated on every restart of the server, as the server looses the
%% nonce ets tables after restart.
nonce_secure_secret() ->
    case application:get_env(zotonic_core, nonce_secure_secret) of
        {ok, Key} when size(Key) >= 50 ->
            Key;
        _ ->
            jobs:run(zotonic_singular_job, fun generate_nonce_secure_secret/0),
            nonce_secure_secret()
    end.

generate_nonce_secure_secret() ->
    case application:get_env(zotonic_core, nonce_secure_secret) of
        {ok, Key} when size(Key) >= 50 ->
            ok;
        _ ->
            application:set_env(zotonic_core, nonce_secure_secret, z_ids:rand_bytes(50))
    end.
