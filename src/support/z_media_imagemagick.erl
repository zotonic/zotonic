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

-export([selected/0, local/0, clear_cache/0, installed/1, installations/0]).

-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/include/file.hrl").

%% @doc Select the most common available ImageMagick major version in the pool.
%% Ties follow configuration order; unavailable installations do not vote.
%% Cache successful lookups for 60 seconds and failures for 5 seconds. Local tools
%% are only probed when no runner is configured.
-spec selected() -> map().
selected() ->
    case z_media_runner:enabled() of
        false -> local();
        true ->
            case remote() of
                {ok, Info} -> Info;
                _ -> missing()
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
    Command = unicode:characters_to_binary([z_utils:os_filename(Path), " -version"]),
    case {ok, unicode:characters_to_binary(z_exec:run(binary_to_list(Command), #{timeout => 5000, max_size => 65536}))} of
        {ok, Output} ->
            case re:run(Output, <<"ImageMagick ([0-9]+)\\.([0-9]+\\.[0-9]+(?:-[0-9]+)?)">>,
                    [{capture, [1, 2], binary}]) of
                {match, [Major, Rest]} ->
                    Info = info(Tool, <<Major/binary, ".", Rest/binary>>, binary_to_integer(Major),
                        z_utils:os_filename(Path)),
                    Identify = case {Tool, os:find_executable("identify")} of
                        {<<"convert">>, false} -> false;
                        {<<"convert">>, IdentifyPath} -> z_utils:os_filename(IdentifyPath);
                        _ -> maps:get(identify, Info)
                    end,
                    {ok, Info#{identify => Identify}};
                nomatch -> {error, unknown_version}
            end
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
                remote_only})),
            {pool, Installed} = cached(remote, Key, fun() -> {pool, probe_pool(Runners)} end),
            {ok, Installed};
        Error -> Error
    end.

%% Use a disposable collector in place of process aliases (OTP 24+).
probe_pool(Runners) ->
    Parent = self(),
    Tag = make_ref(),
    {Collector, Ref} = spawn_monitor(fun() ->
        ParentRef = monitor(process, Parent),
        Workers = [begin
            Me = self(),
            {Pid, Mon} = spawn_monitor(fun() -> Me ! {probe, self(), fetch_remote(R)} end),
            {Pid, Mon, R}
        end || R <- Runners],
        try
            Replies = collect(Workers, erlang:monotonic_time(millisecond) + 5500, ParentRef, #{}),
            Parent ! {Tag, [{R, maps:get(R, Replies, {error, unavailable})} || R <- Runners]}
        after
            lists:foreach(fun({Pid, Mon, _}) -> exit(Pid, kill), demonitor(Mon, [flush]) end, Workers)
        end
    end),
    receive
        {Tag, Results} -> demonitor(Ref, [flush]), Results;
        {'DOWN', Ref, process, Collector, _} -> [{R, {error, unavailable}} || R <- Runners]
    end.

collect([], _, _, Replies) -> Replies;
collect(Workers, Deadline, ParentRef, Replies) ->
    receive
        {probe, Pid, Reply} ->
            {Pid, Mon, R} = lists:keyfind(Pid, 1, Workers),
            demonitor(Mon, [flush]),
            collect(lists:keydelete(Pid, 1, Workers), Deadline, ParentRef, Replies#{R => Reply});
        {'DOWN', ParentRef, process, _, _} -> exit(normal);
        {'DOWN', _Mon, process, Pid, _} ->
            collect(lists:keydelete(Pid, 1, Workers), Deadline, ParentRef, Replies)
    after max(0, Deadline - erlang:monotonic_time(millisecond)) -> Replies
    end.

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

%% @doc Clear discovery after an administrator changes the installed tools.
-spec clear_cache() -> ok.
clear_cache() ->
    lists:foreach(fun
        ({{?MODULE, _} = Key, _}) -> persistent_term:erase(Key);
        (_) -> ok
    end, persistent_term:get()).
