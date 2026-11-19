%% @doc Exercise resets in a disposable site repository, including staged and untracked files.
-module(z_site_update_reset_tests).
-include_lib("eunit/include/eunit.hrl").

reset_test_() -> {timeout, 30, fun git_reset/0}.

git_reset() ->
    {ok, _} = application:ensure_all_started(erlexec),
    Root = filename:join(os:getenv("TMPDIR", "/tmp"),
        "zotonic-reset-test-" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = filelib:ensure_dir(filename:join(Root, "site/placeholder")),
    try
        git("init", Root),
        write(Root, "site/page.tpl", <<"original">>),
        write(Root, "deleted.txt", <<"restore me">>),
        write(Root, ".gitignore", <<"ignored.txt\n">>),
        git("add .", Root),
        git("-c user.name=Test -c user.email=test@localhost -c commit.gpgsign=false commit -m Original", Root),
        Head = git("rev-parse HEAD", Root),
        write(Root, "site/page.tpl", <<"staged">>),
        git("add .", Root),
        write(Root, "site/page.tpl", <<"unstaged">>),
        ok = file:delete(filename:join(Root, "deleted.txt")),
        write(Root, "new <file>.txt", <<"untracked">>),
        write(Root, "ignored.txt", <<"keep">>),
        VCS = {git, filename:join(Root, "site")},
        {ok, Changes} = z_site_update_reset:changes(VCS),
        ?assertNotEqual(nomatch, binary:match(Changes, <<"MM site/page.tpl">>)),
        ?assertNotEqual(nomatch, binary:match(Changes, <<"deleted.txt">>)),
        ?assertNotEqual(nomatch, binary:match(Changes, <<"new <file>.txt">>)),
        ?assertEqual(nomatch, binary:match(Changes, <<"ignored.txt">>)),
        ?assertEqual(ok, z_site_update_reset:reset(VCS)),
        ?assertEqual({ok, <<>>}, z_site_update_reset:changes(VCS)),
        ?assertEqual({ok, <<"original">>}, file:read_file(filename:join(Root, "site/page.tpl"))),
        ?assertEqual({ok, <<"restore me">>}, file:read_file(filename:join(Root, "deleted.txt"))),
        ?assertEqual({ok, <<"keep">>}, file:read_file(filename:join(Root, "ignored.txt"))),
        ?assertNot(filelib:is_file(filename:join(Root, "new <file>.txt"))),
        ?assertEqual(Head, git("rev-parse HEAD", Root))
    after
        file:del_dir_r(Root)
    end.

write(Root, Name, Content) -> ok = file:write_file(filename:join(Root, Name), Content).
git(Args, Dir) ->
    {ok, Output} = exec:run("git " ++ Args, [sync, stdout, stderr, {cd, Dir}]),
    iolist_to_binary(proplists:get_value(stdout, Output, [])).
