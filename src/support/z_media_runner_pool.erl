%% @copyright 2026 Marc Worrell
%% @doc Media runner client configuration. Invalid configuration never selects local execution.

%% Copyright 2026 Marc Worrell
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

-module(z_media_runner_pool).

-export([configured/0, runners/0, identity/1, rank/4]).

%% @doc An explicit pool overrides the single runner, including an empty pool.
%% This is a configuration-presence check, not a health or validity check.
-spec configured() -> boolean().
configured() ->
    case z_config:get(media_runners) of
        undefined -> not lists:member(z_config:get(media_runner_hostname), [undefined, <<>>, ""]);
        [] -> false;
        _ -> true
    end.

%% @doc Only {ok, []} permits local execution. A non-empty pool is remote-only;
%% configuration errors and subsequent remote failures must be returned to the caller.
-spec runners() -> {ok, [map()]} | {error, media_runner_configuration}.
runners() ->
    Entries = case z_config:get(media_runners) of
        undefined ->
            case configured() of
                false -> [];
                true -> [#{
                    hostname => z_config:get(media_runner_hostname),
                    protocol => z_config:get(media_runner_protocol, <<"https">>),
                    oauth2_key => z_config:get(media_runner_oauth2_key, <<>>)
                }]
            end;
        Pool -> Pool
    end,
    try
        true = is_list(Entries) andalso length(Entries) =< 32,
        Runners = [runner(E) || E <- Entries],
        Ids = [identity(R) || R <- Runners],
        true = length(Ids) =:= length(lists:usort(Ids)),
        {ok, Runners}
    catch
        _:_ -> {error, media_runner_configuration}
    end.

runner(Entry) ->
    Host = maps:get(hostname, Entry, maps:get(<<"hostname">>, Entry, undefined)),
    Protocol = maps:get(protocol, Entry, maps:get(<<"protocol">>, Entry, <<"https">>)),
    Token = token(maps:get(oauth2_key, Entry, maps:get(<<"oauth2_key">>, Entry, <<>>))),
    {ok, Url} = z_media_runner_protocol:endpoint(Host, Protocol),
    #{url => Url, token => Token}.

%% Bearer tokens are non-empty header values, never arbitrary Erlang terms.
token(Token) when is_list(Token) ->
    token(unicode:characters_to_binary(Token));
token(Token) when is_binary(Token), byte_size(Token) > 0 ->
    match = re:run(Token, <<"^[\\x21-\\x7e]+\\z">>, [{capture, none}]),
    Token.

%% @doc Scope future cache hints by endpoint and credential without exposing tokens in keys.
-spec identity(Runner) -> binary() when Runner :: map().
identity(#{url := Url, token := Token}) ->
    crypto:hash(sha256, term_to_binary({Url, Token})).

%% @doc Prefer expected cached bytes, then fewer active jobs in this work class.
%% Rendezvous hashing spreads cold files while keeping repeated inputs together.
-spec rank([map()], map(), map(), map()) -> [map()].
rank(Runners, Job, Hints, Active) ->
    Files = maps:get(<<"files">>, Job),
    Hashes = lists:usort([H || #{<<"sha256">> := H} <- Files]),
    Seed = case Hashes of [] -> make_ref(); [Hash | _] -> Hash end,
    Now = erlang:monotonic_time(second),
    Scored = [begin
        Id = identity(R),
        Bytes = lists:sum([max(1, Size) || #{<<"sha256">> := H, <<"size">> := Size} <- Files,
            maps:get({Id, H}, Hints, Now) > Now]),
        Score = {Bytes, -maps:get(Id, Active, 0), crypto:hash(sha256, term_to_binary({Seed, Id}))},
        {Score, R}
    end || R <- Runners],
    [R || {_, R} <- lists:reverse(lists:sort(Scored))].

