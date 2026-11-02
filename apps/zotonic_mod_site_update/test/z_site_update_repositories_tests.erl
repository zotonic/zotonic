%% @doc Identify independent module checkouts without trusting request paths.
-module(z_site_update_repositories_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

repositories_test_() -> {timeout, 30, fun repositories/0}.

repositories() ->
    {ok, _} = application:ensure_all_started(erlexec),
    Root = filename:join(os:getenv("TMPDIR", "/tmp"),
        "zotonic-module-repos-" ++ integer_to_list(erlang:unique_integer([positive]))),
    Core = filename:join(Root, "zotonic"),
    User = filename:join(Root, "user"),
    Linked = filename:join(Root, "linked"),
    Nested = filename:join(User, "apps/zotonic_mod_nested"),
    Worktree = filename:join(Root, "worktree"),
    Plain = filename:join(Root, "plain"),
    lists:foreach(fun(Dir) -> ok = filelib:ensure_dir(filename:join(Dir, "placeholder")) end,
        [Core, Nested, Plain]),
    try
        git("init", Core),
        git("init", User),
        git("-c user.name=ZotonicTest -c user.email=test@localhost commit --allow-empty -m fixture", User),
        git("worktree add -b deployment-test " ++ z_filelib:os_filename(Worktree), User),
        ok = file:make_symlink(User, Linked),
        meck:new(z_module_manager, [passthrough, no_link]),
        meck:new(z_path, [passthrough, no_link]),
        meck:new(z_acl, [passthrough, no_link]),
        meck:expect(z_path, get_path, fun() -> Core end),
        meck:expect(z_path, site_source_dir, fun
            (zotonic_mod_core_test) -> Core;
            (zotonic_mod_user_test) -> User;
            (zotonic_mod_nested_test) -> Nested;
            (zotonic_mod_linked_test) -> Linked;
            (zotonic_mod_worktree_test) -> Worktree;
            (zotonic_mod_plain_test) -> Plain
        end),
        meck:expect(z_module_manager, scan, fun() -> [
            {mod_core_test, zotonic_mod_core_test, Core},
            {mod_user_test, zotonic_mod_user_test, User},
            {mod_nested_test, zotonic_mod_nested_test, Nested},
            {mod_linked_test, zotonic_mod_linked_test, Linked},
            {mod_worktree_test, zotonic_mod_worktree_test, Worktree},
            {mod_plain_test, zotonic_mod_plain_test, Plain}
        ] end),
        Modules = z_site_update_repositories:user_modules(),
        ?assertEqual([mod_linked_test, mod_nested_test, mod_user_test, mod_worktree_test],
            lists:sort(maps:keys(Modules))),
        UserRoot = git("rev-parse --show-toplevel", User),
        WorktreeRoot = git("rev-parse --show-toplevel", Worktree),
        ?assertEqual({git, UserRoot}, z_site_update_repositories:module(mod_nested_test)),
        ?assertEqual({git, UserRoot}, z_site_update_repositories:module(<<"mod_linked_test">>)),
        ?assertEqual({git, WorktreeRoot}, z_site_update_repositories:module(mod_worktree_test)),
        ?assertEqual(false, z_site_update_repositories:module(mod_core_test)),
        ?assertEqual(false, z_site_update_repositories:module(mod_plain_test)),
        ?assertEqual(false, z_site_update_repositories:module(<<"/tmp/arbitrary-repository">>)),
        %% A displayed button must not authorize a repository removed afterwards.
        ok = file:del_dir_r(filename:join(User, ".git")),
        ?assertEqual(false, z_site_update_repositories:module(mod_user_test)),
        meck:expect(z_acl, is_admin, fun(_) -> false end),
        ?assertEqual({error, eacces}, m_site_update:m_get([<<"user_modules">>], undefined, #context{}))
    after
        [meck:unload(M) || M <- [z_module_manager, z_path, z_acl]],
        file:del_dir_r(Root)
    end.

git(Args, Dir) ->
    {ok, Output} = exec:run("git " ++ Args, [sync, stdout, stderr, {cd, Dir}]),
    unicode:characters_to_list(z_string:trim(iolist_to_binary(proplists:get_value(stdout, Output, [])))).
