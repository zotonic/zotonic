%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Test ImageMagick discovery, cache isolation, refreshes and fallback warnings.
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

-module(z_media_imagemagick_tests).

-include_lib("eunit/include/eunit.hrl").
-export([log/2]).

log(#{level := warning, msg := {report, #{remote_version := Remote, local_version := Local}}},
        #{config := #{pid := Pid}}) ->
    Pid ! {version_warning, Remote, Local}, ok;
log(_, _) -> ok.

version_probe_test_() -> {timeout, 30, fun version_probe/0}.

version_probe() ->
    ensure_sidejobs(),
    Keys = [media_runner_hostname, media_runner_oauth2_key, media_runner_local_fallback,
        environment],
    Old = [{K, application:get_env(zotonic, K)} || K <- Keys],
    Counter = ets:new(version_probe, [public, set]),
    ets:insert(Counter, [{calls, 0}, {reply, version(6)}]),
    meck:new(z_media_runner_protocol, [passthrough, no_link]),
    meck:new(z_exec, [passthrough, no_link]),
    logger:add_handler(zmr_version_test, ?MODULE, #{level => warning, config => #{pid => self()}}),
    try
        application:set_env(zotonic, media_runner_hostname, <<"runner.example">>),
        application:set_env(zotonic, media_runner_oauth2_key, <<"token-a">>),
        application:set_env(zotonic, media_runner_local_fallback, false),
        application:set_env(zotonic, environment, production),
        meck:expect(z_exec, run, fun(Command, #{timeout := 5000, max_size := 65536}) ->
            ?assert(is_binary(Command)),
            {ok, <<"Version: ImageMagick 7.1.2-3 Q16 HDRI">>}
        end),
        meck:expect(z_media_runner_protocol, request, fun(_, _, _, 5000) ->
            ets:update_counter(Counter, calls, 1),
            [{reply, Reply}] = ets:lookup(Counter, reply), Reply
        end),
        z_media_imagemagick:clear_cache(),
        %% Remote-only discovery must work before any local tool is probed.
        ?assertMatch(#{major := 6, legacy := true}, z_media_imagemagick:selected()),
        ?assertEqual(0, meck:num_calls(z_exec, run, '_')),
        ?assertMatch(#{version := <<"7.1.2-3">>}, z_media_imagemagick:local()),
        z_media_imagemagick:local(),
        ?assertEqual(1, meck:num_calls(z_exec, run, '_')),
        ?assertMatch(#{legacy := true, cmd := "convert"}, z_media_imagemagick:selected()),
        ?assertEqual(true, z_media_preview:is_legacy_imagemagick()),
        ?assertEqual(1, calls(Counter)),
        %% A cold-cache burst shares one refresh.
        z_media_imagemagick:clear_cache(),
        Parent = self(),
        [spawn(fun() -> Parent ! {selected, z_media_imagemagick:selected()} end) || _ <- lists:seq(1, 20)],
        [receive {selected, #{major := 6}} -> ok after 10000 -> error(probe_timeout) end || _ <- lists:seq(1, 20)],
        ?assertEqual(2, calls(Counter)),
        lists:foreach(fun({K, V}) ->
            Before = calls(Counter),
            application:set_env(zotonic, K, V),
            z_media_imagemagick:selected(),
            ?assertEqual(Before + 1, calls(Counter))
        end, [{media_runner_hostname, <<"other.example">>},
            {media_runner_oauth2_key, <<"token-b">>}, {environment, development}]),
        ets:insert(Counter, {reply, version(7)}),
        expire(remote),
        ?assertMatch(#{major := 7, legacy := false}, z_media_imagemagick:selected()),
        ets:insert(Counter, {reply, {error, timeout}}),
        expire(remote),
        ?assertMatch(#{available := false}, z_media_imagemagick:selected()),
        BeforeRetry = calls(Counter),
        ets:insert(Counter, {reply, version(6)}),
        ?assertMatch(#{available := false}, z_media_imagemagick:selected()),
        ?assertEqual(BeforeRetry, calls(Counter)),
        expire(remote),
        ?assertMatch(#{major := 6}, z_media_imagemagick:selected()),
        %% Successful refreshes and remote failures must not trigger local probes.
        ?assertEqual(1, meck:num_calls(z_exec, run, '_')),
        application:set_env(zotonic, media_runner_local_fallback, true),
        [spawn(fun() -> Parent ! {warned, z_media_imagemagick:selected()} end) || _ <- lists:seq(1, 20)],
        [receive {warned, #{major := 6}} -> ok after 10000 -> error(probe_timeout) end || _ <- lists:seq(1, 20)],
        receive {version_warning, <<"6.9.12-1">>, <<"7.1.2-3">>} -> ok
            after 1000 -> error(missing_version_warning) end,
        z_media_imagemagick:selected(),
        receive {version_warning, _, _} -> error(repeated_warning) after 50 -> ok end,
        %% Minor and patch differences within the same major must not warn.
        ets:insert(Counter, {reply, {ok, json(#{imagemagick => #{
            available => true, tool => <<"magick">>, major => 7, version => <<"7.0.8-1">>}})}}),
        expire(remote),
        ?assertMatch(#{major := 7}, z_media_imagemagick:selected()),
        receive {version_warning, _, _} -> error(same_major_warning) after 50 -> ok end,
        ets:insert(Counter, {reply, {error, timeout}}),
        expire(remote),
        ?assertMatch(#{major := 7}, z_media_imagemagick:selected()),
        ets:insert(Counter, {reply, {error, {http_status, 403}}}),
        expire(remote),
        ?assertMatch(#{available := false}, z_media_imagemagick:selected()),
        application:unset_env(zotonic, media_runner_hostname),
        ?assertEqual(false, z_media_preview:is_legacy_imagemagick())
    after
        logger:remove_handler(zmr_version_test),
        meck:unload(z_media_runner_protocol), meck:unload(z_exec),
        ets:delete(Counter), z_media_imagemagick:clear_cache(),
        lists:foreach(fun
            ({K, undefined}) -> application:unset_env(zotonic, K);
            ({K, {ok, V}}) -> application:set_env(zotonic, K, V)
        end, Old)
    end.

version(Major) ->
    {Tool, Version} = case Major of
        6 -> {<<"convert">>, <<"6.9.12-1">>};
        7 -> {<<"magick">>, <<"7.1.2-3">>}
    end,
    {ok, json(#{imagemagick => #{available => true,
        tool => Tool, major => Major, version => Version}})}.

calls(Counter) -> ets:lookup_element(Counter, calls, 2).

expire(Scope) ->
    {Key, _, Value} = persistent_term:get({z_media_imagemagick, Scope}),
    persistent_term:put({z_media_imagemagick, Scope}, {Key, erlang:monotonic_time(second) - 1, Value}).

json(Value) -> z_json:decode(z_json:encode(Value)).

pool_versions_test_() -> {timeout, 10, fun pool_versions/0}.

pool_versions() ->
    ensure_sidejobs(),
    Keys = [media_runners, media_runner_local_fallback],
    Old = [{K, application:get_env(zotonic, K)} || K <- Keys],
    meck:new(z_media_runner_protocol, [passthrough, no_link]),
    meck:new(z_exec, [passthrough, no_link]),
    try
        application:set_env(zotonic, media_runners, [
            #{hostname => <<"versions-a.example">>, oauth2_key => <<"a">>},
            #{hostname => <<"versions-b.example">>, oauth2_key => <<"b">>}
        ]),
        application:set_env(zotonic, media_runner_local_fallback, false),
        meck:expect(z_media_runner_protocol, request, fun(_, Token, #{}, 5000) ->
            case Token of <<"a">> -> version(6); <<"b">> -> version(7) end
        end),
        z_media_imagemagick:clear_cache(),
        {ok, [A, B]} = z_media_runner_pool:runners(),
        ?assertMatch(#{major := 6}, z_media_imagemagick:selected()),
        ?assertMatch({ok, #{major := 7}}, z_media_imagemagick:installed(B)),
        ?assertMatch({ok, #{major := 6}}, z_media_imagemagick:installed(A)),
        ?assertMatch({ok, #{major := 7}}, z_media_imagemagick:installed(B)),
        ?assertEqual(2, meck:num_calls(z_media_runner_protocol, request, '_')),
        meck:expect(z_media_runner_protocol, request, fun(_, Token, #{}, 5000) ->
            case Token of <<"a">> -> {error, timeout}; <<"b">> -> version(7) end
        end),
        expire(remote),
        ?assertMatch(#{major := 7}, z_media_imagemagick:selected()),
        ?assertEqual(4, meck:num_calls(z_media_runner_protocol, request, '_')),
        %% The first configured runner must not outweigh two newer runners.
        {ok, Pool} = application:get_env(zotonic, media_runners),
        application:set_env(zotonic, media_runners, Pool ++ [
            #{hostname => <<"versions-c.example">>, oauth2_key => <<"c">>}
        ]),
        meck:expect(z_media_runner_protocol, request, fun(_, Token, #{}, 5000) ->
            case Token of
                <<"a">> -> version(6);
                <<"b">> -> version(7);
                <<"c">> -> {ok, json(#{imagemagick => #{available => true,
                    tool => <<"magick">>, major => 7, version => <<"7.0.8-1">>}})}
            end
        end),
        z_media_imagemagick:clear_cache(),
        ?assertMatch(#{major := 7, legacy := false}, z_media_imagemagick:selected()),
        ?assertNot(z_media_preview:is_legacy_imagemagick()),
        Before = meck:num_calls(z_media_runner_protocol, request, '_'),
        ?assertMatch(#{major := 7}, z_media_imagemagick:selected()),
        ?assertEqual(Before, meck:num_calls(z_media_runner_protocol, request, '_')),
        %% Version refreshes can change the majority, including back to IM 6.
        meck:expect(z_media_runner_protocol, request, fun(_, _, #{}, 5000) -> version(6) end),
        expire(remote),
        ?assertMatch(#{major := 6, legacy := true}, z_media_imagemagick:selected()),
        %% Missing and unreachable installations do not form a majority.
        meck:expect(z_media_runner_protocol, request, fun(_, Token, #{}, 5000) ->
            case Token of
                <<"a">> -> {error, timeout};
                <<"b">> -> version(7);
                <<"c">> -> {ok, json(#{imagemagick => #{available => false}})}
            end
        end),
        z_media_imagemagick:clear_cache(),
        ?assertMatch(#{major := 7}, z_media_imagemagick:selected()),
        ?assertEqual(0, meck:num_calls(z_exec, run, '_'))
    after
        meck:unload(z_media_runner_protocol),
        meck:unload(z_exec),
        z_media_imagemagick:clear_cache(),
        lists:foreach(fun
            ({K, undefined}) -> application:unset_env(zotonic, K);
            ({K, {ok, V}}) -> application:set_env(zotonic, K, V)
        end, Old)
    end.

parallel_discovery_test_() -> {timeout, 15, fun parallel_discovery/0}.

parallel_discovery() ->
    ensure_sidejobs(),
    Old = application:get_env(zotonic, media_runners),
    Parent = self(),
    meck:new(z_media_runner_protocol, [passthrough, no_link]),
    try
        application:set_env(zotonic, media_runners, [
            #{hostname => <<Name/binary, ".example">>, oauth2_key => Name}
            || Name <- [<<"slow-a">>, <<"slow-b">>, <<"slow-c">>, <<"healthy">>]]),
        meck:expect(z_media_runner_protocol, request, fun(_, Token, _, 5000) ->
            Parent ! {probe_started, self(), Token},
            case Token of
                <<"healthy">> -> version(7);
                _ -> receive never -> {error, timeout} end
            end
        end),
        z_media_imagemagick:clear_cache(),
        {Micros, #{major := 7}} = timer:tc(fun z_media_imagemagick:selected/0),
        ?assert(Micros < 8000000),
        Workers = [receive {probe_started, Pid, _} -> Pid after 100 -> error(missing_probe) end
            || _ <- lists:seq(1, 4)],
        lists:foreach(fun(Pid) ->
            Ref = monitor(process, Pid),
            receive {'DOWN', Ref, process, Pid, _} -> ok after 1000 -> error(leaked_probe) end
        end, Workers),
        %% The partial snapshot's retry period starts after collection finishes.
        {CachedMicros, #{major := 7}} = timer:tc(fun z_media_imagemagick:selected/0),
        ?assert(CachedMicros < 1000000),
        receive {probe_started, _, _} -> error(repeated_probe) after 0 -> ok end
    after
        meck:unload(z_media_runner_protocol),
        z_media_imagemagick:clear_cache(),
        case Old of
            undefined -> application:unset_env(zotonic, media_runners);
            {ok, V} -> application:set_env(zotonic, media_runners, V)
        end
    end.

arguments_snapshot_test() ->
    Counter = ets:new(arguments_snapshot, [public]),
    ets:insert(Counter, {calls, 0}),
    meck:new(z_media_imagemagick, [passthrough, no_link]),
    try
        meck:expect(z_media_imagemagick, selected, fun() ->
            N = ets:update_counter(Counter, calls, 1),
            #{legacy => N =/= 1}
        end),
        Props = #{<<"width">> => 16, <<"height">> => 16, <<"mime">> => <<"image/gif">>},
        {ok, {_, _, Args}} = z_media_preview:cmd_args(Props, [lossless, {removebg, <<"10">>}], <<"image/gif">>),
        Command = iolist_to_binary(lists:join(" ", Args)),
        ?assertNotEqual(nomatch, binary:match(Command, <<"CompareAny">>)),
        ?assertNotEqual(nomatch, binary:match(Command, <<"alpha 0,0 floodfill">>)),
        ?assertEqual(1, ets:lookup_element(Counter, calls, 2))
    after
        meck:unload(z_media_imagemagick),
        ets:delete(Counter)
    end.

%% Standalone EUnit does not start zotonic_core's sidejob resource.
ensure_sidejobs() ->
    {ok, _} = application:ensure_all_started(sidejob),
    case whereis(zotonic_sidejobs) of
        undefined -> {ok, _} = z_sidejob:init(), ok;
        _ -> ok
    end.

probe_overload_test() ->
    Old = application:get_env(zotonic, media_runners),
    meck:new(z_sidejob, [passthrough, no_link]),
    meck:new(z_media_runner_protocol, [passthrough, no_link]),
    try
        application:set_env(zotonic, media_runners, [
            #{hostname => <<"overloaded.example">>, oauth2_key => <<"test">>}]),
        meck:expect(z_sidejob, start, fun(z_media_imagemagick, probe_remote, _) ->
            {error, overload}
        end),
        z_media_imagemagick:clear_cache(),
        ?assertMatch({ok, [{_, {error, unavailable}}]}, z_media_imagemagick:installations()),
        ?assertEqual(0, meck:num_calls(z_media_runner_protocol, request, '_'))
    after
        meck:unload(z_media_runner_protocol),
        meck:unload(z_sidejob),
        z_media_imagemagick:clear_cache(),
        case Old of
            undefined -> application:unset_env(zotonic, media_runners);
            {ok, V} -> application:set_env(zotonic, media_runners, V)
        end
    end.
