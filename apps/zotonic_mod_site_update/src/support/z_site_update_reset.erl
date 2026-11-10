%% @copyright 2026 Marc Worrell
%% @doc Inspect and discard uncommitted repository changes. Call under the deployment barrier.
-module(z_site_update_reset).
-moduledoc("Reset tracked files to the current revision and remove non-ignored untracked files.
Local commits, ignored files and nested repositories are preserved. Repository paths must
be resolved by the caller, after checking deployment permissions.").
-export([changes/1, reset/1]).

-spec changes(VCS) -> {ok, binary()} | {error, term()} when VCS :: m_site_update:vcs().
changes(VCS) ->
    case repository_root(VCS) of
        {ok, VCSRoot} -> changes_root(VCSRoot);
        {error, _} = Error -> Error
    end.

-spec reset(VCS) -> ok | {error, term()} when VCS :: m_site_update:vcs().
reset(VCS) ->
    case repository_root(VCS) of
        {ok, VCSRoot} -> reset_checkout(VCSRoot);
        {error, _} = Error -> Error
    end.

reset_checkout(VCSRoot) ->
    case reset_root(VCSRoot) of
        {ok, _} ->
            case changes_root(VCSRoot) of
                {ok, <<>>} -> ok;
                {ok, _} -> {error, local_changes_remaining};
                {error, _} = Error -> Error
            end;
        {error, _} = Error -> Error
    end.

%% Resolve the root for symlinked sites and sites nested in a shared checkout.
-spec repository_root(VCS) -> {ok, m_site_update:vcs()} | {error, term()}
    when VCS :: m_site_update:vcs().
repository_root({Vcs, Path}) ->
    Command = case Vcs of
        git -> "git rev-parse --show-toplevel";
        hg -> "hg root"
    end,
    case run(Command, Path) of
        {ok, Root} -> {ok, {Vcs, unicode:characters_to_list(z_string:trim(Root))}};
        {error, _} = Error -> Error
    end.

changes_root({git, Path}) ->
    run("git -c core.quotePath=true status --porcelain=v1 --untracked-files=all --ignore-submodules=none", Path);
changes_root({hg, Path}) ->
    run("hg status", Path).

reset_root({git, Path}) ->
    %% Restore rather than reset --hard: do not delete ignored obstructions.
    case run("git restore --source=HEAD --staged --worktree -- :/", Path) of
        {ok, _} -> run("git clean -fd", Path);
        {error, _} = Error -> Error
    end;
reset_root({hg, Path}) ->
    case run("hg revert --all --no-backup", Path) of
        {ok, _} -> run("hg --config extensions.purge= purge", Path);
        {error, _} = Error -> Error
    end.

run(Command, Path) ->
    case exec:run(Command, [sync, stdout, stderr, {cd, Path},
            {env, [{"GIT_TERMINAL_PROMPT", "0"}, {"HGPLAIN", "1"}]}]) of
        {ok, Output} -> {ok, iolist_to_binary(proplists:get_value(stdout, Output, []))};
        {error, _} = Error -> Error
    end.
