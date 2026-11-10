-module(zotonic_filewatcher_sup_tests).
-include_lib("eunit/include/eunit.hrl").

inotify_watch_files_test() ->
    Root = filename:join(os:getenv("TMPDIR", "/tmp"),
        "zotonic-watch-test-" ++ integer_to_list(erlang:unique_integer([positive]))),
    Lib = filename:join(Root, "lib with spaces"),
    Source = filename:join(Root, "source"),
    Dirs = ["app/ebin", "app/node_modules/deep", "app/.git/objects"],
    SourceDirs = ["templates/nested", "files/archive/deep", "data/cache", "lib-src/node_modules/deep"],
    try
        [ok = filelib:ensure_dir(filename:join([Lib, D, "placeholder"])) || D <- Dirs],
        [ok = filelib:ensure_dir(filename:join([Source, D, "placeholder"])) || D <- SourceDirs],
        Priv = filename:join([Lib, "app", "priv"]),
        ok = file:make_symlink(Source, Priv),
        %% The recursive watcher skips symlinks; the explicit root must remain.
        Input = zotonic_filewatcher_sup:inotify_watch_files([Lib, Priv, Lib]),
        Lines = binary:split(Input, <<"\n">>, [global]),
        ?assertEqual(1, length([L || L <- Lines, L =:= unicode:characters_to_binary(Lib)])),
        ?assert(lists:member(unicode:characters_to_binary(Priv), Lines)),
        Expected = [filename:join(Lib, "app/node_modules"), filename:join(Lib, "app/.git"),
            filename:join(Priv, "files"), filename:join(Priv, "data"),
            filename:join(Priv, "lib-src/node_modules")],
        Excluded = [binary_to_list(P) || <<"@", P/binary>> <- Lines],
        ?assertEqual(lists:sort(Expected), lists:sort(Excluded)),
        ?assertNot(lists:any(fun(P) -> lists:suffix("deep", P) end, Excluded))
    after
        file:del_dir_r(Root)
    end.

watcher_arguments_test() ->
    Modules = [exec, zotonic_filewatcher_sup],
    lists:foreach(fun(M) -> meck:new(M, [no_link]) end, Modules),
    Roots = ["/site " ++ integer_to_list(N) || N <- lists:seq(1, 5000)],
    Input = unicode:characters_to_binary([[D, "\n"] || D <- Roots]),
    Pid = self(),
    meck:expect(zotonic_filewatcher_sup, watch_dirs_expanded, fun() -> Roots end),
    meck:expect(zotonic_filewatcher_sup, inotify_watch_files, fun(RootsArg) ->
        ?assertEqual(Roots, RootsArg), Input
    end),
    meck:expect(exec, run_link, fun(_, _) -> {ok, Pid, 123} end),
    meck:expect(exec, send, fun(_, _) -> ok end),
    try
        {ok, State} = zotonic_filewatcher_inotify:init(["inotifywait"]),
        {noreply, _} = zotonic_filewatcher_inotify:handle_info(start, State),
        [{_, {exec, run_link, [Args, Options]}, _}] = [H || H = {_, {exec, run_link, _}, _} <- meck:history(exec)],
        ?assertEqual(10, length(Args)),
        ?assertEqual(["--fromfile", "-"], lists:nthtail(8, Args)),
        ?assert(lists:member(stdin, Options)),
        ?assert(meck:called(exec, send, [Pid, Input])),
        ?assert(meck:called(exec, send, [Pid, eof])),
        meck:reset(exec),
        {ok, FsState} = zotonic_filewatcher_fswatch:init(["fswatch"]),
        {noreply, _} = zotonic_filewatcher_fswatch:handle_info(start, FsState),
        [{_, {exec, run_link, [FsArgs, _]}, _}] = [H || H = {_, {exec, run_link, _}, _} <- meck:history(exec)],
        ?assertEqual(["fswatch", "-0", "-x", "-Lr", "-E", "-e",
            zotonic_filewatcher_handler:re_exclude()] ++ Roots, FsArgs)
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.
