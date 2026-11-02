%% @doc Preview real Git histories without modifying the deployed checkout.
-module(z_site_update_preview_tests).
-include_lib("eunit/include/eunit.hrl").

preview_test_() -> {timeout, 30, fun git_preview/0}.

git_preview() ->
    {ok, _} = application:ensure_all_started(erlexec),
    Root = filename:join(os:getenv("TMPDIR", "/tmp"),
        "zotonic-preview-test-" ++ integer_to_list(erlang:unique_integer([positive]))),
    Remote = filename:join(Root, "remote"),
    Checkout = filename:join(Root, "checkout"),
    ok = filelib:ensure_dir(filename:join(Remote, "placeholder")),
    try
        git("init", Remote),
        ok = file:write_file(filename:join(Remote, "page.tpl"), <<"original">>),
        ok = file:write_file(filename:join(Remote, "rebar.lock"), <<"original lock">>),
        git("add .", Remote),
        commit("Original", Remote),
        git("clone " ++ z_filelib:os_filename(Remote) ++ " " ++ z_filelib:os_filename(Checkout), Root),
        Head = git("rev-parse HEAD", Checkout),
        {ok, Same} = z_site_update_preview:preview({git, Checkout}),
        ?assertEqual(0, maps:get(count, Same)),
        ?assertNot(maps:get(can_deploy, Same)),
        ok = file:write_file(filename:join(Checkout, "rebar.lock"), <<"local generated lock">>),
        git("add rebar.lock", Checkout),
        ok = file:write_file(filename:join(Remote, "page.tpl"), <<"updated">>),
        git("add .", Remote),
        commit("Remote <script>alert(1)</script>\n\nBody with | separators\nand another line.", Remote),
        ok = file:write_file(filename:join(Remote, "page.tpl"), <<"latest">>),
        git("add .", Remote),
        commit("Newest", Remote),
        {ok, Incoming} = z_site_update_preview:preview({git, Checkout}),
        ?assertEqual(2, maps:get(count, Incoming)),
        ?assert(maps:get(can_deploy, Incoming)),
        ?assertMatch(#{subject := <<"Original">>, body := <<>>}, maps:get(current, Incoming)),
        [Newest, Commit] = maps:get(log, Incoming),
        ?assertMatch(#{subject := <<"Newest">>, body := <<>>}, Newest),
        ?assertEqual(maps:get(hash, Newest), z_string:trim(git("rev-parse --short HEAD", Remote))),
        ?assertMatch(#{subject := <<"Remote <script>alert(1)</script>">>,
            author := <<"ZotonicTest">>, body := <<"Body with | separators\nand another line.">>}, Commit),
        ?assertNotEqual(<<>>, maps:get(hash, Commit)),
        ?assertEqual(10, byte_size(maps:get(date, Commit))),
        ?assertEqual(Head, git("rev-parse HEAD", Checkout)),
        ?assertEqual({ok, <<"original">>}, file:read_file(filename:join(Checkout, "page.tpl"))),
        ?assertEqual({ok, <<"local generated lock">>}, file:read_file(filename:join(Checkout, "rebar.lock"))),
        ?assertNotEqual(<<>>, git("diff --cached -- rebar.lock", Checkout)),
        commit("Local commit", Checkout),
        {ok, Diverged} = z_site_update_preview:preview({git, Checkout}),
        ?assert(maps:get(is_diverged, Diverged)),
        ?assertNot(maps:get(can_deploy, Diverged)),
        git("remote set-url origin /nonexistent/zotonic-preview-test", Checkout),
        ?assertMatch({ok, #{current := _, error := fetch_failed}},
            z_site_update_preview:preview({git, Checkout})),
        git("branch --unset-upstream", Checkout),
        ?assertMatch({ok, #{current := _, error := no_upstream}},
            z_site_update_preview:preview({git, Checkout}))
    after
        file:del_dir_r(Root)
    end.

commit(Message, Dir) ->
    git("-c user.name=ZotonicTest -c user.email=test@localhost commit -m " ++
        z_filelib:os_filename(Message), Dir).

git(Args, Dir) ->
    {ok, Output} = exec:run("git " ++ Args, [sync, stdout, stderr, {cd, Dir}]),
    z_string:trim(iolist_to_binary(proplists:get_value(stdout, Output, []))).
