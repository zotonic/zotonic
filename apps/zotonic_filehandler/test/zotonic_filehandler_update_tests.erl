%% @doc Run in an isolated VM: these tests own the node-wide update coordinator.
-module(zotonic_filehandler_update_tests).
-include_lib("eunit/include/eunit.hrl").

update_test_() ->
    case whereis(zotonic_filehandler_update) of
        undefined -> isolated_tests();
        _ -> {timeout, 60, fun isolated_vm/0}
    end.

isolated_vm() ->
    Args = ["-noshell", "-pa"] ++ code:get_path() ++ ["-eval",
        "case eunit:test(zotonic_filehandler_update_tests, [verbose]) of ok -> halt(0); _ -> halt(1) end."],
    Port = open_port({spawn_executable, os:find_executable("erl")},
        [binary, exit_status, stderr_to_stdout, {args, Args}]),
    isolated_result(Port, []).

isolated_result(Port, Output) ->
    receive
        {Port, {data, Data}} -> isolated_result(Port, [Data | Output]);
        {Port, {exit_status, Status}} ->
            io:put_chars(lists:reverse(Output)),
            ?assertEqual(0, Status)
    end.

isolated_tests() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun delayed_helper/0,
        fun public_status/0,
        fun failed_build/0,
        fun automatic_failures/0,
        fun missing_compile_options/0,
        fun interrupted_build/0,
        fun interrupted_automatic_update/0,
        fun active_migrations/0,
        fun atomic_failure/0,
        fun old_code_in_use/0,
        fun watcher_replay/0,
        fun periodic_mtime_flush/0,
        fun scanner_mtime_flush/0
    ]}.

setup() ->
    persistent_term:erase(zotonic_filehandler_update),
    {ok, Pid} = zotonic_filehandler_update:start_link(),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid),
    persistent_term:erase(zotonic_filehandler_update),
    lists:foreach(fun(M) -> code:purge(M), code:delete(M), code:purge(M) end,
        [update_test_owner, update_test_helper, update_test_loop]).

%% Reproduce #4573: the new owner is ready before the helper. Both simulated
%% sites must see the new helper before they can record the target version.
delayed_helper() ->
    Owner1 = beam(update_test_owner, "-export([version/0, migrate/0]). version() -> 1. migrate() -> update_test_helper:version()."),
    Owner2 = beam(update_test_owner, "-export([version/0, migrate/0]). version() -> 2. migrate() -> update_test_helper:version()."),
    Helper1 = beam(update_test_helper, "-export([version/0]). version() -> 1."),
    Helper2 = beam(update_test_helper, "-export([version/0]). version() -> 2."),
    {ok, _} = zotonic_filehandler_reload:load([Owner1, Helper1]),
    Parent = self(),
    Updater = spawn_link(fun() ->
        Result = zotonic_filehandler_update:deploy(fun() ->
            Parent ! owner_compiled,
            receive helper_compiled -> ok end,
            zotonic_filehandler_reload:load([Owner2, Helper2])
        end),
        Parent ! {updated, Result}
    end),
    receive owner_compiled -> ok after 1000 -> error(timeout) end,
    [spawn_link(fun() ->
        Result = zotonic_filehandler_update:schema(fun() ->
            {fixture_call(update_test_owner, version, []), fixture_call(update_test_owner, migrate, [])}
        end),
        Parent ! {migrated, Result}
    end) || _ <- [site_a, site_b]],
    receive {migrated, _} -> error(early_migration) after 30 -> ok end,
    ?assertEqual(1, fixture_call(update_test_owner, version, [])),
    Updater ! helper_compiled,
    receive {updated, {ok, _}} -> ok after 1000 -> error(timeout) end,
    [receive {migrated, Result} -> ?assertEqual({2, 2}, Result)
     after 1000 -> error(timeout) end || _ <- [site_a, site_b]].

public_status() ->
    ?assertMatch(#{active := false, blocked := false, deployment := false},
        zotonic_filehandler_update:status()),
    ?assertEqual({error, private_reason}, zotonic_filehandler_update:deploy(fun() ->
        ?assertMatch(#{active := true, deployment := true}, zotonic_filehandler_update:status()),
        {error, private_reason}
    end)),
    ?assertEqual(#{active => false, blocked => true, deployment => true, restart_required => false},
        zotonic_filehandler_update:status()),
    ok = zotonic_filehandler_update:build(fun() -> ok end),
    ?assertMatch(#{blocked := false, deployment := false}, zotonic_filehandler_update:status()).

failed_build() ->
    ?assertEqual({error, compile_failed},
        zotonic_filehandler_update:deploy(fun() -> {error, compile_failed} end)),
    ?assert(zotonic_filehandler_update:is_pending()),
    ?assertMatch({error, {code_update_failed, compile_failed}},
        zotonic_filehandler_update:schema(fun() -> error(must_not_run) end)),
    ?assertMatch({error, _}, zotonic_filehandler_update:run(fun() -> error(must_not_run) end)),
    ?assertEqual({error, retry_failed},
        zotonic_filehandler_update:build(fun() -> {error, retry_failed} end)),
    ?assert(zotonic_filehandler_update:is_pending()),
    ?assertEqual(ok, zotonic_filehandler_update:build(fun() -> ok end)),
    ?assertEqual(migrated, zotonic_filehandler_update:schema(fun() -> migrated end)).

%% Neither single-file updates nor ordinary full builds latch a deployment failure.
automatic_failures() ->
    lists:foreach(fun(Update) ->
        ?assertEqual({error, compile_failed}, Update(fun() -> {error, compile_failed} end)),
        ?assertNot(zotonic_filehandler_update:is_pending()),
        ?assertEqual(ok, zotonic_filehandler_update:run(fun() -> ok end)),
        ?assertEqual(ok, zotonic_filehandler_update:schema(fun() -> ok end)),
        ?assertEqual({ok, ok}, zotonic_filehandler_update:try_idle(fun() -> ok end)),
        ?assertError(compile_crashed, Update(fun() -> error(compile_crashed) end)),
        ?assertNot(zotonic_filehandler_update:is_pending())
    end, [fun zotonic_filehandler_update:run/1, fun zotonic_filehandler_update:build/1]).

interrupted_build() ->
    Parent = self(),
    {Updater, Ref} = spawn_monitor(fun() ->
        zotonic_filehandler_update:deploy(fun() ->
            Parent ! started,
            receive stop -> ok end
        end)
    end),
    receive started -> ok after 1000 -> error(timeout) end,
    exit(Updater, kill),
    receive {'DOWN', Ref, _, _, _} -> ok after 1000 -> error(timeout) end,
    ?assertMatch({error, _}, zotonic_filehandler_update:schema(fun() -> error(must_not_run) end)),
    ?assertEqual(ok, zotonic_filehandler_update:deploy(fun() -> ok end)).

interrupted_automatic_update() ->
    Parent = self(),
    {Updater, Ref} = spawn_monitor(fun() ->
        zotonic_filehandler_update:run(fun() ->
            Parent ! started,
            receive stop -> ok end
        end)
    end),
    receive started -> ok after 1000 -> error(timeout) end,
    exit(Updater, kill),
    receive {'DOWN', Ref, _, _, _} -> ok after 1000 -> error(timeout) end,
    %% The schema lease waits for the coordinator to process its DOWN message.
    ?assertEqual(ok, zotonic_filehandler_update:schema(fun() -> ok end)),
    ?assertNot(zotonic_filehandler_update:is_pending()).

active_migrations() ->
    Parent = self(),
    Reader = spawn_link(fun() ->
        zotonic_filehandler_update:schema(fun() ->
            Parent ! migrating,
            receive finish_migration -> ok end
        end)
    end),
    receive migrating -> ok after 1000 -> error(timeout) end,
    spawn_link(fun() ->
        zotonic_filehandler_update:deploy(fun() -> Parent ! building, ok end)
    end),
    receive building -> error(build_during_migration) after 30 -> ok end,
    Reader ! finish_migration,
    receive building -> ok after 1000 -> error(timeout) end.

atomic_failure() ->
    V1 = beam(update_test_owner, "-export([version/0]). version() -> 1."),
    V2 = beam(update_test_owner, "-export([version/0]). version() -> 2."),
    {ok, _} = zotonic_filehandler_reload:load([V1]),
    ?assertMatch({error, _}, zotonic_filehandler_reload:load(
        [V2, {update_test_helper, "bad.beam", <<"not a beam">>}])),
    ?assertEqual(1, fixture_call(update_test_owner, version, [])),
    OnLoad = beam(update_test_helper,
        "-on_load(init/0). -export([version/0]). init() -> ok. version() -> 2."),
    ?assertMatch({error, _}, zotonic_filehandler_reload:load([V2, OnLoad])),
    ?assertEqual(1, fixture_call(update_test_owner, version, [])).

old_code_in_use() ->
    V1 = beam(update_test_loop, "-export([loop/1, version/0]). version() -> 1. loop(P) -> P ! running, receive stop -> ok end."),
    V2 = beam(update_test_loop, "-export([version/0]). version() -> 2."),
    V3 = beam(update_test_loop, "-export([version/0]). version() -> 3."),
    {ok, _} = zotonic_filehandler_reload:load([V1]),
    Parent = self(),
    {Worker, Ref} = spawn_monitor(fun() -> fixture_call(update_test_loop, loop, [Parent]) end),
    receive running -> ok after 1000 -> error(timeout) end,
    {ok, _} = zotonic_filehandler_reload:load([V2]),
    ?assertEqual({error, {old_code_in_use, [update_test_loop]}},
        zotonic_filehandler_reload:load([V3])),
    ?assert(is_process_alive(Worker)),
    ?assertEqual(2, fixture_call(update_test_loop, version, [])),
    Worker ! stop,
    receive {'DOWN', Ref, _, _, normal} -> ok after 1000 -> error(timeout) end,
    ?assertMatch({ok, _}, zotonic_filehandler_reload:load([V3])).

%% Rebar can touch uncompiled test sources after a successful deployment.
%% Skipping them must neither poison the barrier nor clear a real build failure.
missing_compile_options() ->
    meck:new(jobs, [no_link]),
    meck:expect(jobs, run, fun(zotonic_filehandler_single_job, Fun) -> Fun() end),
    File = "/nonexistent/zotonic_filehandler/test/uncompiled_deploy_fixture.erl",
    try
        ?assertEqual(false, zotonic_filehandler_compile:compile_options(File)),
        ?assertEqual(ok, zotonic_filehandler_compile:recompile_task(File)),
        ?assertEqual(false, zotonic_filehandler_update:is_pending()),
        ?assertEqual(ok, zotonic_filehandler_update:schema(fun() -> ok end)),
        ?assertEqual({error, compile_failed},
            zotonic_filehandler_update:deploy(fun() -> {error, compile_failed} end)),
        ?assertEqual({error, {code_update_failed, compile_failed}},
            zotonic_filehandler_compile:recompile_task(File)),
        ?assertEqual(true, zotonic_filehandler_update:is_pending()),
        ?assertEqual({error, {code_update_failed, compile_failed}},
            zotonic_filehandler_update:schema(fun() -> error(unexpected_schema) end))
    after
        meck:unload(jobs)
    end.

watcher_replay() ->
    meck:new(zotonic_notifier, [no_link]),
    meck:new(zotonic_filehandler_mapper, [no_link]),
    meck:expect(zotonic_notifier, observe, fun(_, _, _, _, _) -> ok end),
    meck:expect(zotonic_notifier, map, fun(_, _, _, _) -> [] end),
    Parent = self(),
    meck:expect(zotonic_filehandler_mapper, map_change, fun(Verb, File) ->
        Parent ! {mapped, Verb, File}, false
    end),
    {ok, Handler} = zotonic_filehandler_handler:start_link(),
    try
        ?assertEqual({error, failed}, zotonic_filehandler_update:deploy(fun() ->
            ok = zotonic_filehandler_handler:handle_changes(#{<<"page.tpl">> => modify}),
            {error, failed}
        end)),
        receive {mapped, _, _} -> error(early_file_action) after 30 -> ok end,
        ok = zotonic_filehandler_update:deploy(fun() -> ok end),
        receive {mapped, modify, <<"page.tpl">>} -> ok after 1000 -> error(timeout) end,
        ?assertEqual({error, compile_failed}, zotonic_filehandler_update:run(fun() ->
            ok = zotonic_filehandler_handler:handle_changes(#{<<"fixed.erl">> => modify}),
            {error, compile_failed}
        end)),
        receive {mapped, modify, <<"fixed.erl">>} -> ok after 1000 -> error(timeout) end
    after
        gen_server:stop(Handler),
        meck:unload(zotonic_filehandler_mapper),
        meck:unload(zotonic_notifier)
    end.

periodic_mtime_flush() ->
    {ok, MTime} = z_file_mtime:start_link(false),
    File = <<"/nonexistent/zotonic-mtime-barrier-test">>,
    try
        ?assertEqual({error, notfound}, z_file_mtime:mtime(File)),
        ?assert(ets:member(z_filewatcher_mtime_tab, File)),
        ?assertEqual({error, failed}, zotonic_filehandler_update:deploy(fun() ->
            flush_mtimes(MTime),
            ?assert(ets:member(z_filewatcher_mtime_tab, File)),
            {error, failed}
        end)),
        %% Periodic ticks during a failed deployment must retain the cache too.
        flush_mtimes(MTime),
        ?assert(ets:member(z_filewatcher_mtime_tab, File)),
        ok = zotonic_filehandler_update:deploy(fun() -> ok end),
        _ = sys:get_state(MTime),
        ?assertNot(ets:member(z_filewatcher_mtime_tab, File)),
        %% Normal periodic clearing resumes once the barrier is open.
        ?assertEqual({error, notfound}, z_file_mtime:mtime(File)),
        flush_mtimes(MTime),
        ?assertNot(ets:member(z_filewatcher_mtime_tab, File))
    after
        gen_server:stop(MTime)
    end.

scanner_mtime_flush() ->
    {ok, MTime} = z_file_mtime:start_link(true),
    File = <<"/nonexistent/zotonic-mtime-scanner-test">>,
    try
        ?assertEqual({error, notfound}, z_file_mtime:mtime(File)),
        ok = zotonic_filehandler_update:deploy(fun() -> flush_mtimes(MTime) end),
        _ = sys:get_state(MTime),
        ?assert(ets:member(z_filewatcher_mtime_tab, File))
    after
        gen_server:stop(MTime)
    end.

flush_mtimes(Pid) ->
    Pid ! flush,
    _ = sys:get_state(Pid),
    ok.

beam(Module, Body) ->
    Source = "-module(" ++ atom_to_list(Module) ++ "). " ++ Body,
    {ok, Tokens, _} = erl_scan:string(Source),
    Forms = forms(Tokens, [], []),
    {ok, Module, Binary} = compile:forms(Forms, [binary]),
    {Module, atom_to_list(Module) ++ ".beam", Binary}.

forms([], [], Acc) -> lists:reverse(Acc);
forms([{dot, _} = Dot | Rest], Current, Acc) ->
    {ok, Form} = erl_parse:parse_form(lists:reverse([Dot | Current])),
    forms(Rest, [], [Form | Acc]);
forms([Token | Rest], Current, Acc) -> forms(Rest, [Token | Current], Acc).

%% These modules are compiled and loaded by the test, not present in the PLT.
fixture_call(Module, Function, Args) ->
    apply(Module, Function, Args).
