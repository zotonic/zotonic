%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc Nonce support. Create nonces, and track nonce usage.
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
% periodic cleanup of the nonce tables.
-define(NONCE_TIMEOUT, 600).

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
%% will be the owner of these tables.
init_nonce_tables() ->
    nonce_next_cleanup(),
    lists:foreach(
        fun(N) ->
            ets:new(N, [ named_table, public ])
        end,
        ?NONCE_TABLES).

-spec nonce() -> Nonce when
    Nonce :: binary().
%% @doc Return a new nonce value, strictly valid for the how long we keep the
%% used nonce values. This gives maximum protection against replay attacks.
nonce() ->
    nonce(?NONCE_TIMEOUT).

-spec nonce(Timeout) -> Nonce when
    Timeout :: integer(),
    Nonce :: binary().
%% @doc Return a new nonce value, valid for about the given number of seconds.
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
        {Number, Random, Hash} when Number >= Oldest ->
            CheckKey = <<(integer_to_binary(Number))/binary, "-", Random/binary>>,
            CheckHash = crypto:mac(hmac, sha256, CheckKey, nonce_secret()),
            if
                CheckHash =:= Hash ->
                    register_1(CheckKey);
                true ->
                    {error, key}
            end;
        {_Number, _Random, _Key} ->
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
    case is_registered(Key) of
        false ->
            Table = nonce_table(),
            Size = ets:info(Table, size),
            if
                Size < ?NONCE_OVERLOAD ->
                    ets:insert(nonce_table(), {Key, secs()}),
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
                        {binary_to_integer(Number), Key, Hash1}
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
        {Number, Random, _Hash} ->
            CheckKey = <<(integer_to_binary(Number))/binary, "-", Random/binary>>,
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
        {Number, Random, _Hash} ->
            CheckKey = <<(integer_to_binary(Number))/binary, "-", Random/binary>>,
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
    nonce_next_cleanup(),
    ets:delete_all_objects(nonce_prev_table()),
    ok.

nonce_next_cleanup() ->
    SecsPerTable = ?NONCE_TIMEOUT div length(?NONCE_TABLES),
    timer:apply_after((SecsPerTable div 2) * 1000, ?MODULE, nonce_cleanup, []).

nonce_table() ->
    N = nonce_generation(0),
    N1 = N rem length(?NONCE_TABLES),
    lists:nth(N1+1, ?NONCE_TABLES).

nonce_prev_table() ->
    N = nonce_generation(0),
    NTab = length(?NONCE_TABLES),
    N1 = (N + NTab - 1) rem NTab,
    lists:nth(N1+1, ?NONCE_TABLES).

nonce_generation(DeltaSecs) ->
    SecsPerTable = ?NONCE_TIMEOUT div length(?NONCE_TABLES),
    (secs() + DeltaSecs) div SecsPerTable.

nonce_oldest_generation() ->
    nonce_generation(0) - length(?NONCE_TABLES) + 1.

secs() ->
    {MSecs, Secs, _} = os:timestamp(),
    MSecs * 1_000_000 + Secs.

-spec nonce_secret() -> binary().
%% @doc Return the secret used for signing the nonce values returned by nonce/1
nonce_secret() ->
    case application:get_env(zotonic_core, nonce_secret) of
        {ok, Key} when size(Key) >= 50 ->
            Key;
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

