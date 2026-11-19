%% @doc Integration test: local git pull, real umbrella compile and activation.
-module(zotonic_filehandler_deploy_tests).
-include_lib("eunit/include/eunit.hrl").

deploy_test_() ->
    case whereis(z_sites_manager) of
        undefined -> [];
        _ -> {timeout, 180, fun deploy/0}
    end.

deploy() ->
    Root = filename:join(os:getenv("TMPDIR", "/tmp"),
        "zotonic-deploy-test-" ++ integer_to_list(erlang:unique_integer([positive]))),
    Remote = filename:join(Root, "remote"),
    Checkout = filename:join(Root, "zotonic_update_fixture"),
    Ebin = filename:join(Checkout, "ebin"),
    ok = filelib:ensure_dir(filename:join(Remote, "ebin/placeholder")),
    %% Rebar touches test sources throughout the host umbrella. Do not schedule
    %% unrelated development recompiles while testing this fixture's deployments.
    %% Watcher buffering/replay is exercised separately by the isolated tests.
    HostRoot = z_path:get_path(),
    zotonic_filehandler_handler:ignore_dir(HostRoot, true),
    ok = zotonic_filehandler_handler:update_barrier(),
    try
        git("init", Remote),
        write_version(Remote, 1),
        ok = file:write_file(filename:join(Remote, "rebar.lock"), <<"upstream lock 1">>),
        ok = file:write_file(filename:join(Remote, "local.txt"), <<"original">>),
        git("add .", Remote),
        commit(Remote),
        git("clone " ++ z_filelib:os_filename(Remote) ++ " " ++
            z_filelib:os_filename(Checkout), Root),
        %% Keep this fixture independent of the developer's pull.rebase setting,
        %% which can reject unrelated unstaged edits even on a fast-forward pull.
        git("config pull.rebase false", Checkout),
        true = code:add_patha(Ebin),
        {module, deploy_test_owner} = code:load_file(deploy_test_owner),
        {module, deploy_test_helper} = code:load_file(deploy_test_helper),
        ?assertEqual(1, fixture_call(deploy_test_owner, migrate, [])),
        %% Simulate generated apps_user changes, including a staged lock file.
        ok = file:write_file(filename:join(Checkout, "rebar.lock"), <<"generated lock">>),
        git("add rebar.lock", Checkout),
        ok = file:write_file(filename:join(Checkout, "local.txt"), <<"keep local edit">>),
        write_version(Remote, 2),
        ok = file:write_file(filename:join(Remote, "rebar.lock"), <<"upstream lock 2">>),
        git("add .", Remote),
        commit(Remote),
        ?assertMatch({ok, _}, checked_deploy(Checkout)),
        ?assertNot(zotonic_filehandler_update:is_pending()),
        ?assertEqual({ok, <<"upstream lock 2">>},
            file:read_file(filename:join(Checkout, "rebar.lock"))),
        ?assertEqual({ok, <<"keep local edit">>},
            file:read_file(filename:join(Checkout, "local.txt"))),
        %% Repositories without a tracked lock file must still deploy normally.
        git("rm rebar.lock", Remote),
        commit(Remote),
        ?assertMatch({ok, _}, checked_deploy(Checkout)),
        ok = file:write_file(filename:join(Checkout, "rebar.lock"), <<"untracked lock">>),
        ?assertMatch({ok, _}, checked_deploy(Checkout)),
        ?assertEqual({ok, <<"untracked lock">>},
            file:read_file(filename:join(Checkout, "rebar.lock"))),
        ?assertEqual({2, 2}, zotonic_filehandler_update:schema(fun() ->
            {fixture_call(deploy_test_owner, version, []), fixture_call(deploy_test_owner, migrate, [])}
        end)),
        ?assertEqual({ok, <<"2">>}, file:read_file(filename:join(Checkout, "page.tpl"))),
        %% A failing pull closes the barrier, and a full rebuild can recover it.
        git("remote set-url origin /nonexistent/zotonic-deployment-test", Checkout),
        ?assertMatch({error, _}, checked_deploy(Checkout)),
        ?assert(zotonic_filehandler_update:is_pending()),
        ?assertMatch({error, _}, zotonic_filehandler_update:schema(fun() -> must_not_run end)),
        ?assertEqual(ok, zotonic_filehandler:compile_all_sync()),
        ?assertNot(zotonic_filehandler_update:is_pending())
    after
        zotonic_filehandler_handler:ignore_dir(HostRoot, false),
        code:del_path(Ebin),
        [begin code:purge(M), code:delete(M), code:purge(M) end ||
            M <- [deploy_test_owner, deploy_test_helper]],
        file:del_dir_r(Root)
    end.

write_version(Dir, Version) ->
    V = integer_to_list(Version),
    write_beam(Dir, deploy_test_owner,
        "-export([version/0, migrate/0]). version() -> " ++ V ++
        ". migrate() -> deploy_test_helper:version()."),
    write_beam(Dir, deploy_test_helper,
        "-export([version/0]). version() -> " ++ V ++ "."),
    ok = file:write_file(filename:join(Dir, "page.tpl"), V).

write_beam(Dir, Module, Body) ->
    File = filename:join(Dir, atom_to_list(Module) ++ ".erl"),
    ok = file:write_file(File, "-module(" ++ atom_to_list(Module) ++ "). " ++ Body),
    {ok, Module} = compile:file(File, [{outdir, filename:join(Dir, "ebin")}]).

commit(Dir) ->
    git("-c user.name=ZotonicTest -c user.email=test@localhost commit -m fixture", Dir).

git(Args, Dir) ->
    {ok, _} = exec:run("git " ++ Args, [sync, stdout, stderr, {cd, Dir}]),
    ok.

%% Trace the calling process to verify the deployment lease is released before
%% either refresh starts. The tracer acknowledges delivery before assertions.
checked_deploy(Checkout) ->
    Tracer = spawn_link(fun() -> collect_refresh_trace([]) end),
    MFAs = [{zotonic_filehandler_update, deploy, 1}, {z, flush, 0}, {z, reindex, 0}],
    lists:foreach(fun(MFA) -> erlang:trace_pattern(MFA, [{'_', [], [{return_trace}]}], [local]) end, MFAs),
    erlang:trace(self(), true, [call, {tracer, Tracer}]),
    try
        Result = zotonic_filehandler_compile:deploy(git, Checkout),
        erlang:trace(self(), false, [call]),
        Ref = erlang:trace_delivered(self()),
        receive {trace_delivered, _, Ref} -> ok after 1000 -> error(trace_timeout) end,
        Tracer ! {get, self()},
        Events = receive {events, Es} -> Es after 1000 -> error(trace_timeout) end,
        case Result of
            {ok, _} -> ?assertEqual([released, flush, reindex], Events);
            {error, _} -> ?assertEqual([released], Events)
        end,
        Result
    after
        erlang:trace(self(), false, [call]),
        lists:foreach(fun(MFA) -> erlang:trace_pattern(MFA, false, [local]) end, MFAs),
        Tracer ! stop
    end.

collect_refresh_trace(Events) ->
    receive
        {trace, _, return_from, {zotonic_filehandler_update, deploy, 1}, _} ->
            collect_refresh_trace([released | Events]);
        {trace, _, call, {z, Function, []}} when Function =:= flush; Function =:= reindex ->
            collect_refresh_trace([Function | Events]);
        {get, Pid} ->
            Pid ! {events, lists:reverse(Events)},
            collect_refresh_trace(Events);
        stop -> ok;
        _ -> collect_refresh_trace(Events)
    end.

%% These modules are compiled and loaded by the test, not present in the PLT.
fixture_call(Module, Function, Args) ->
    apply(Module, Function, Args).
