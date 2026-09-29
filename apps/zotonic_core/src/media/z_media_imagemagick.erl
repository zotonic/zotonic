%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Detect local and remote ImageMagick versions with bounded, configuration-aware caching.
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

-module(z_media_imagemagick).

-moduledoc "Preview and identification share the same ImageMagick probe. A configured
runner reports its actual local installation via authenticated HTTPS. Separate local
and remote caches expire after 60 seconds; failures retry after 5 seconds. Configuration
and executable changes invalidate the appropriate cache immediately.".

-export([selected/0, local/0, clear_cache/0, installed/1, installations/0, probe_remote/2]).

-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/include/file.hrl").

%% @doc Select the most common available ImageMagick major version in the pool.
%% Ties follow configuration order; unavailable installations do not vote.
%% Cache successful lookups for 60 seconds and failures for 5 seconds. Local tools
%% are only probed without a runner or when local fallback is enabled.
-spec selected() -> map().
selected() ->
    case z_media_runner:enabled() of
        false -> local();
        true ->
            Config = {z_media_runner_pool:runners(), z_config:get(environment),
                z_config:get(media_runner_local_fallback, false)},
            Key = crypto:hash(sha256, term_to_binary(Config)),
            case remote() of
                {ok, Info} ->
                    warn_mismatch(Key, Info),
                    Info;
                {error, unavailable} ->
                    case z_config:get(media_runner_local_fallback, false) of
                        true -> local();
                        _ -> missing()
                    end;
                {error, _} -> missing()
            end
    end.

%% @doc Probe the actual local binary, even on a runner configured for remote jobs.
-spec local() -> map().
local() ->
    {Tool, Path} = case os:find_executable("magick") of
        false -> {<<"convert">>, os:find_executable("convert")};
        P -> {<<"magick">>, P}
    end,
    IdentifyPath = os:find_executable("identify"),
    Key = {Path, executable_info(Path), IdentifyPath, executable_info(IdentifyPath), os:getenv("PATH")},
    case cached(local, Key, fun() -> probe(Tool, Path) end) of
        {ok, Info} -> Info;
        {error, _} -> missing()
    end.

executable_info(false) -> undefined;
executable_info(Path) ->
    case file:read_file_info(Path, [{time, posix}]) of
        {ok, #file_info{mtime = MTime, ctime = CTime, size = Size, inode = Inode}} ->
            {MTime, CTime, Size, Inode};
        {error, Reason} -> {error, Reason}
    end.

missing() -> #{available => false, cmd => false, identify => false, legacy => false}.

probe(_, false) -> {error, not_installed};
probe(Tool, Path) ->
    %% Only a trusted installed executable and a fixed argument; no uploaded media.
    Command = unicode:characters_to_binary([z_filelib:os_filename(Path), " -version"]),
    case z_exec:run(Command, #{timeout => 5000, max_size => 65536}) of
        {ok, Output} ->
            case re:run(Output, <<"ImageMagick ([0-9]+)\\.([0-9]+\\.[0-9]+(?:-[0-9]+)?)">>,
                    [{capture, [1, 2], binary}]) of
                {match, [Major, Rest]} ->
                    Info = info(Tool, <<Major/binary, ".", Rest/binary>>, binary_to_integer(Major),
                        z_filelib:os_filename(Path)),
                    Identify = case {Tool, os:find_executable("identify")} of
                        {<<"convert">>, false} -> false;
                        {<<"convert">>, IdentifyPath} -> z_filelib:os_filename(IdentifyPath);
                        _ -> maps:get(identify, Info)
                    end,
                    {ok, Info#{identify => Identify}};
                nomatch -> {error, unknown_version}
            end;
        {error, _} = Error -> Error
    end.

info(Tool, Version, Major, Command) ->
    Identify = case Tool of
        <<"magick">> -> Command ++ " identify";
        <<"convert">> -> "identify"
    end,
    #{available => true, tool => Tool, version => Version, major => Major,
        legacy => Major < 7, cmd => Command, identify => Identify}.

remote() ->
    case installations() of
        {ok, Installed} -> majority([Reply || {_, Reply} <- Installed]);
        {error, _} -> {error, configuration}
    end.

%% Select a representative from the largest major-version group. Keep the first
%% configured representative on a tie, so command generation remains stable.
majority(Results) ->
    Available = [Info || {ok, #{available := true} = Info} <- Results],
    case Available of
        [] ->
            case Results of
                [] -> {error, unavailable};
                _ -> lists:last(Results)
            end;
        _ ->
            Counts = lists:foldl(fun(#{major := Major}, Acc) ->
                Acc#{Major => maps:get(Major, Acc, 0) + 1}
            end, #{}, Available),
            Largest = lists:max(maps:values(Counts)),
            [Selected | _] = [Info || #{major := Major} = Info <- Available,
                maps:get(Major, Counts) =:= Largest],
            {ok, Selected}
    end.

%% @doc Look up one runner in the shared, configuration-aware capability snapshot.
-spec installed(map()) -> {ok, map()} | {error, term()}.
installed(Runner) ->
    case installations() of
        {ok, Installed} ->
            case lists:keyfind(Runner, 1, Installed) of
                {Runner, Reply} -> Reply;
                false -> {error, unavailable}
            end;
        Error -> Error
    end.

%% @doc Probe at most 32 configured runners concurrently within a 5.5-second
%% deadline. Cache the entire snapshot, so one slow host cannot repeatedly expire
%% earlier failure entries while later probes are still running.
-spec installations() -> {ok, [{map(), {ok, map()} | {error, term()}}]} | {error, term()}.
installations() ->
    case z_media_runner_pool:runners() of
        {ok, Runners} ->
            Key = crypto:hash(sha256, term_to_binary({Runners, z_config:get(environment),
                z_config:get(media_runner_local_fallback, false)})),
            {pool, Installed} = cached(remote, Key, fun() -> {pool, probe_pool(Runners)} end),
            {ok, Installed};
        Error -> Error
    end.

probe_pool(Runners) ->
    Alias = alias(),
    Deadline = erlang:monotonic_time(millisecond) + 5500,
    Workers = lists:foldl(fun(Runner, Acc) ->
        case z_sidejob:start(?MODULE, probe_remote, [Alias, Runner]) of
            {ok, Pid} ->
                Monitor = monitor(process, Pid),
                Pid ! {Alias, probe},
                Acc#{Pid => {Runner, Monitor}};
            {error, overload} -> Acc
        end
    end, #{}, Runners),
    try
        Replies = collect(Workers, Alias, Deadline, #{}),
        [{R, maps:get(R, Replies, {error, unavailable})} || R <- Runners]
    after
        unalias(Alias),
        maps:foreach(fun(Pid, {_, Monitor}) ->
            exit(Pid, kill),
            demonitor(Monitor, [flush])
        end, Workers),
        flush_replies(Alias)
    end.

%% @doc Run a supervised capability probe after the caller installs its monitor.
%% The handshake keeps a fast reply ordered before DOWN; sidejob admission can
%% otherwise return after the worker has already exited. Unexpected crashes are
%% reported by the sidejob's proc_lib process instead of becoming silent failures.
-spec probe_remote(reference(), map()) -> ok.
probe_remote(Alias, Runner) ->
    receive
        {Alias, probe} ->
            Alias ! {Alias, self(), fetch_remote(Runner)},
            ok
    after 5500 ->
        ok
    end.

collect(Workers, _Alias, _Deadline, Replies) when map_size(Workers) =:= 0 -> Replies;
collect(Workers, Alias, Deadline, Replies) ->
    receive
        {Alias, Pid, Reply} ->
            {{Runner, Monitor}, Rest} = maps:take(Pid, Workers),
            demonitor(Monitor, [flush]),
            collect(Rest, Alias, Deadline, Replies#{Runner => Reply});
        {'DOWN', Monitor, process, Pid, _} when is_map_key(Pid, Workers) ->
            {{Runner, Monitor}, Rest} = maps:take(Pid, Workers),
            collect(Rest, Alias, Deadline, Replies#{Runner => {error, unavailable}})
    after max(0, Deadline - erlang:monotonic_time(millisecond)) ->
        Replies
    end.

flush_replies(Alias) ->
    receive {Alias, _, _} -> flush_replies(Alias) after 0 -> ok end.


fetch_remote(#{url := Base, token := Token}) ->
    case z_media_runner_protocol:request(z_media_runner_protocol:control_url(Base, <<"capabilities">>), Token, #{}, 5000) of
        {ok, Info} -> decode(Info);
        {error, {http_status, Code}} when Code =:= 429; Code =:= 502; Code =:= 503; Code =:= 504 ->
            {error, unavailable};
        {error, {http_status, _}} -> {error, invalid_capabilities};
        {error, _} -> {error, unavailable}
    end.

decode(Body) ->
    try Body of
        #{<<"imagemagick">> := #{<<"available">> := false}} -> {ok, missing()};
        #{<<"imagemagick">> := #{<<"tool">> := Tool, <<"version">> := Version, <<"major">> := Major}}
                when (Tool =:= <<"magick">> orelse Tool =:= <<"convert">>),
                     is_binary(Version), byte_size(Version) < 96,
                     is_integer(Major), Major >= 6, Major =< 99 ->
            case re:run(Version, <<"^[0-9]+\\.[0-9]+\\.[0-9]+(?:-[0-9]+)?$">>, [{capture, none}]) of
                match ->
                    [MajorBin | _] = binary:split(Version, <<".">>, [global]),
                    Major = binary_to_integer(MajorBin),
                    {ok, info(Tool, Version, Major, binary_to_list(Tool))};
                nomatch -> {error, invalid_version}
            end;
        _ -> {error, invalid_capabilities}
    catch
        _:_ -> {error, invalid_capabilities}
    end.

%% Serialize refreshes so a page of thumbnails makes one probe, not one per image.
%% Keep a single entry per scope: changing host or credentials cannot grow the cache.
cached(Scope, Key, Fetch) ->
    case cached_value(Scope, Key) of
        {hit, Value} -> Value;
        miss ->
            global:trans({{?MODULE, Scope}, self()}, fun() ->
                case cached_value(Scope, Key) of
                    {hit, Value} -> Value;
                    miss ->
                        Value = Fetch(),
                        TTL = cache_ttl(Value),
                        persistent_term:put({?MODULE, Scope},
                            {Key, erlang:monotonic_time(second) + TTL, Value}),
                        Value
                end
            end, [node()])
    end.

cache_ttl({ok, _}) -> 60;
cache_ttl({pool, Installed}) ->
    case lists:all(fun({_, Reply}) -> element(1, Reply) =:= ok end, Installed) of
        true -> 60;
        false -> 5
    end;
cache_ttl(_) -> 5.

cached_value(Scope, Key) ->
    Now = erlang:monotonic_time(second),
    case persistent_term:get({?MODULE, Scope}, undefined) of
        {Key, Until, Value} when Until > Now -> {hit, Value};
        _ -> miss
    end.

warn_mismatch(Key, Remote) ->
    case z_config:get(media_runner_local_fallback, false) of
        true ->
            Local = local(),
            Versions = {maps:get(major, Remote, missing), maps:get(major, Local, missing)},
            case Versions of
                {Same, Same} -> persistent_term:erase({?MODULE, warning});
                {_, _} ->
                    Warning = {Key, Versions},
                    case persistent_term:get({?MODULE, warning}, undefined) of
                        Warning -> ok;
                        _ ->
                            global:trans({{?MODULE, warning}, self()}, fun() ->
                                case persistent_term:get({?MODULE, warning}, undefined) of
                                    Warning -> ok;
                                    _ ->
                                        persistent_term:put({?MODULE, warning}, Warning),
                                        ?LOG_WARNING(#{
                                            in => zotonic_core,
                                            text => <<"Media runner and local ImageMagick major versions differ; local fallback may fail or produce different previews">>,
                                            remote_version => maps:get(version, Remote, missing),
                                            local_version => maps:get(version, Local, missing)
                                        })
                                end
                            end, [node()])
                    end
            end;
        _ -> ok
    end.

%% @doc Clear discovery after an administrator changes the installed tools.
-spec clear_cache() -> ok.
clear_cache() ->
    lists:foreach(fun
        ({{?MODULE, _} = Key, _}) -> persistent_term:erase(Key);
        (_) -> ok
    end, persistent_term:get()).
