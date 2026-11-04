%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Remove private media artifacts even when their owning worker is killed.
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

-module(z_media_temp).
-export([guard/2, release/1, sweep/2]).
-include_lib("kernel/include/file.hrl").

-spec guard(file:filename_all(), file | directory) -> pid().
guard(Path, Kind) ->
    Parent = self(),
    Tag = make_ref(),
    {Pid, Monitor} = spawn_monitor(fun() ->
        Ref = monitor(process, Parent),
        gproc:ensure_reg({p, l, {?MODULE, filename:absname(z_convert:to_binary(Path))}}),
        Parent ! {Tag, ready},
        receive
            {release, From, ReleaseTag} -> cleanup(Path, Kind), From ! {ReleaseTag, released};
            {'DOWN', Ref, process, Parent, _} -> cleanup(Path, Kind)
        end
    end),
    receive
        {Tag, ready} -> demonitor(Monitor, [flush]), Pid;
        {'DOWN', Monitor, process, Pid, Reason} -> error({media_temp_guard, Reason})
    end.

-spec release(pid()) -> ok.
release(Pid) ->
    Ref = monitor(process, Pid),
    Pid ! {release, self(), Ref},
    receive
        {Ref, released} -> demonitor(Ref, [flush]), ok;
        {'DOWN', Ref, process, Pid, _} -> ok
    end.

cleanup(Path, file) -> file:delete(Path);
cleanup(Path, directory) -> file:del_dir_r(Path).

%% @doc Resume a bounded traversal of preview storage after VM restarts. Only
%% private managed names older than one day are removed; symlinks are not followed.
%% Ordinary previews (including no-database library previews) are never collected.
-spec sweep(undefined | [file:filename_all()], z:context()) -> [file:filename_all()].
sweep(undefined, Context) -> sweep([z_path:media_preview(Context)], Context);
sweep(Paths, _Context) ->
    Cutoff = erlang:system_time(second) - 86400,
    sweep_paths(Paths, Cutoff, 500).

sweep_paths(Paths, _, 0) -> Paths;
sweep_paths([], _, _) -> [];
sweep_paths([Path | Rest], Cutoff, Left) ->
    case file:read_link_info(Path, [{time, posix}]) of
        {ok, #file_info{type=Type, mtime=Modified}} when Type =:= regular; Type =:= directory ->
            case managed(filename:basename(z_convert:to_binary(Path)), Type) of
                true ->
                    case Modified < Cutoff andalso
                            gproc:lookup_pids({p, l, {?MODULE, filename:absname(z_convert:to_binary(Path))}}) =:= [] of
                        true -> cleanup(Path, case Type of directory -> directory; _ -> file end);
                        false -> ok
                    end,
                    sweep_paths(Rest, Cutoff, Left-1);
                false when Type =:= directory ->
                    case file:list_dir(Path) of
                        {ok, Names} -> sweep_paths(Rest ++ [filename:join(Path, N) || N <- Names], Cutoff, Left-1);
                        _ -> sweep_paths(Rest, Cutoff, Left-1)
                    end;
                false -> sweep_paths(Rest, Cutoff, Left-1)
            end;
        _ -> sweep_paths(Rest, Cutoff, Left-1)
    end.

managed(<<".preview-", _/binary>>, directory) -> true;
managed(<<".download-", _/binary>>, regular) -> true;
managed(_, _) -> false.
