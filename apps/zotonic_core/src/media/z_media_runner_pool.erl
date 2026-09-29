%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Configure and rank independent media runners using bounded file-location hints.
%% @end

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

-export([configured/0, runners/0, identity/1, rank/4, remember/3, forget/3]).

%% @doc An explicit pool takes precedence over the legacy single-runner setting.
-spec configured() -> boolean().
configured() ->
    case z_config:get(media_runners) of
        undefined -> not lists:member(z_config:get(media_runner_hostname), [undefined, <<>>, ""]);
        [] -> false;
        _ -> true
    end.

%% @doc Normalize trusted system configuration; reject an invalid pool as a whole.
-spec runners() -> {ok, [map()]} | {error, media_runner_configuration}.
runners() ->
    Entries = case z_config:get(media_runners) of
        undefined ->
            case configured() of
                false ->
                    [];
                true ->
                    [
                        #{
                            hostname => z_config:get(media_runner_hostname),
                            protocol => z_config:get(media_runner_protocol, <<"https">>),
                            oauth2_key => z_config:get(media_runner_oauth2_key, <<>>)
                        }
                    ]
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
    Token = z_convert:to_binary(maps:get(oauth2_key, Entry, maps:get(<<"oauth2_key">>, Entry, <<>>))),
    true = is_binary(Token) andalso Token =/= <<>>,
    {ok, Url} = z_media_runner_protocol:endpoint(Host, Protocol),
    #{
        url => Url,
        token => Token
    }.

%% @doc Scope hints by endpoint and credential, without storing credentials in keys.
-spec identity(map()) -> binary().
identity(#{ url := Url, token := Token }) ->
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

%% @doc Keep at most 10,000 file-location hints for one hour. Eviction remains safe:
%% hash-only admission always checks the runner's actual cache before uploading.
-spec remember(binary(), [map()], map()) -> map().
remember(Id, Files, Hints) ->
    Now = erlang:monotonic_time(second),
    Fresh = maps:filter(fun(_, Until) -> Until > Now end, Hints),
    Added = lists:foldl(
        fun
            (#{<<"sha256">> := H}, Acc) -> Acc#{{Id, H} => Now + 3600};
            (_, Acc) -> Acc
        end, Fresh, Files),
    case map_size(Added) =< 10000 of
        true -> Added;
        false ->
            Sorted = lists:reverse(lists:sort([{Until, Key} || {Key, Until} <- maps:to_list(Added)])),
            maps:from_list([{Key, Until} || {Until, Key} <- lists:sublist(Sorted, 10000)])
    end.

%% @doc Remove hints when a runner explicitly reports missing inputs.
-spec forget(binary(), [binary()], map()) -> map().
forget(Id, Hashes, Hints) ->
    maps:without([{Id, H} || H <- Hashes], Hints).
