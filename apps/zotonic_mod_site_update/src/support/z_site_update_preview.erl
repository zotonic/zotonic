%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Read the current revision and remote changes without changing the checkout.
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

-module(z_site_update_preview).
-export([preview/1]).

-define(LOG_LIMIT, 100).
%% NUL-delimited fields keep commit messages separate from their metadata.
-define(GIT_FORMAT, "--format='%h%x00%ad%x00%an%x00%B%x00' --date=short").
-define(HG_FORMAT, "--template '{node|short}\\0{date|shortdate}\\0{author|person}\\0{desc}\\0'").

-spec preview(VCS) -> {ok, map()} | {error, term()} when VCS :: m_site_update:vcs().
preview({git, Path}) ->
    preview_current("git log -1 " ?GIT_FORMAT, Path,
        fun(Current) -> git_preview(Current, Path) end);
preview({hg, Path}) ->
    preview_current("hg log -r . " ?HG_FORMAT, Path,
        fun(Current) -> hg_preview(Current, Path) end).

preview_current(Command, Path, Fun) ->
    case run(Command, Path) of
        {ok, Output} ->
            case commits(Output) of
                [Current] ->
                    try Fun(Current)
                    catch throw:{preview_error, Reason} -> {ok, #{current => Current, error => Reason}}
                    end;
                _ -> {error, repository_error}
            end;
        {error, _} -> {error, repository_error}
    end.

git_preview(Current, Path) ->
    %% Verify tracking first, then fetch without resetting the lock file, merging,
    %% checking out files or running compilation. All command arguments are fixed.
    Upstream = read("git rev-parse --abbrev-ref --symbolic-full-name '@{upstream}'", Path, no_upstream),
    _ = read("git fetch --no-tags --no-recurse-submodules", Path, fetch_failed),
    Count = binary_to_integer(read("git rev-list --count 'HEAD..@{upstream}'", Path, repository_error)),
    Log = read_commits("git log -100 " ?GIT_FORMAT " 'HEAD..@{upstream}'", Path),
    FastForward = case run("git merge-base --is-ancestor HEAD '@{upstream}'", Path) of
        {ok, _} -> true;
        {error, Props} ->
            case proplists:get_value(exit_status, Props) of
                256 -> false;
                _ -> throw({preview_error, repository_error})
            end
    end,
    {ok, #{current => Current, upstream => Upstream, log => Log, count => Count,
        is_truncated => Count > ?LOG_LIMIT, can_deploy => Count > 0 andalso FastForward,
        is_diverged => not FastForward}}.

hg_preview(Current, Path) ->
    case run("hg incoming --quiet --limit 100 " ?HG_FORMAT, Path) of
        {ok, Log} -> {ok, #{current => Current, log => commits(Log), can_deploy => true}};
        {error, Props} ->
            case proplists:get_value(exit_status, Props) of
                256 -> {ok, #{current => Current, log => [], can_deploy => false}};
                _ -> throw({preview_error, fetch_failed})
            end
    end.

%% Git adds a newline between records; Mercurial emits the template verbatim.
commits(Output) ->
    commit_fields(binary:split(Output, <<0>>, [global])).

commit_fields([Hash, Date, Author, Message | Rest]) ->
    {Subject, Body} = case binary:split(z_string:trim(Message), <<"\n">>) of
        [Title, Details] -> {Title, z_string:trim(Details)};
        [Title] -> {Title, <<>>}
    end,
    [#{hash => z_string:trim(Hash), date => Date, author => Author,
       subject => Subject, body => Body} | commit_fields(Rest)];
commit_fields([_TrailingNewline]) ->
    [].

read_commits(Command, Path) ->
    case run(Command, Path) of
        {ok, Output} -> commits(Output);
        {error, _} -> throw({preview_error, repository_error})
    end.

read(Command, Path, Reason) ->
    case run(Command, Path) of
        {ok, Output} -> z_string:trim(Output);
        {error, _} -> throw({preview_error, Reason})
    end.

run(Command, Path) ->
    case exec:run(Command, [sync, stdout, stderr, {cd, Path},
            {env, [{"GIT_TERMINAL_PROMPT", "0"}, {"HGPLAIN", "1"}]}]) of
        {ok, Output} ->
            {ok, iolist_to_binary(proplists:get_value(stdout, Output, []))};
        {error, _} = Error -> Error
    end.
