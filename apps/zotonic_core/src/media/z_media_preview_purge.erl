%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Delete generated previews in bounded, restartable maintenance batches.
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

-module(z_media_preview_purge).
-export([run/4]).

-include_lib("zotonic.hrl").
-include_lib("kernel/include/file.hrl").

%% The task's fixed cutoff excludes derivatives created after it was queued.
%% Active generation claims are allowed to finish before their rows are removed.
%% medium.filename and medium.preview_filename mappings are never removed.
-spec run(Before, LocalBefore, Phase, Context) -> ok | {delay, non_neg_integer(), list()} when
    Before :: binary(), LocalBefore :: integer(),
    Phase :: cafs | {local, [file:filename_all()]} | remote,
    Context :: z:context().
run(Before, LocalBefore, cafs, Context) ->
    case z_cafs_preview:purge_batch(Before, Context) of
        more -> {delay, 1, [Before, LocalBefore, cafs]};
        busy -> {delay, 30, [Before, LocalBefore, cafs]};
        done -> {delay, 1, [Before, LocalBefore, {local, [z_path:media_preview(Context)]}]}
    end;
run(Before, LocalBefore, {local, Paths}, Context) ->
    case purge_local(Paths, LocalBefore, 100, Context) of
        [] -> {delay, 1, [Before, LocalBefore, remote]};
        Rest -> {delay, 1, [Before, LocalBefore, {local, Rest}]}
    end;
run(Before, LocalBefore, remote, Context) ->
    case z_file_request:purge_previews(Before, Context) of
        ok ->
            ?LOG_NOTICE(#{text => <<"Generated preview cleanup completed; remote deletion is queued">>,
                in => zotonic_core}),
            ok;
        pending -> {delay, 1, [Before, LocalBefore, remote]};
        {error, Reason} ->
            ?LOG_WARNING(#{text => <<"Generated preview cleanup waiting for filestore">>,
                in => zotonic_core, result => error, reason => Reason}),
            {delay, 600, [Before, LocalBefore, remote]}
    end.

purge_local([], _Before, _Left, _Context) -> [];
purge_local(Paths, _Before, 0, _Context) -> Paths;
purge_local([Path | Rest], Before, Left, Context) ->
    %% Never follow symlinks or traverse private in-progress conversion directories.
    case filename:basename(z_convert:to_binary(Path)) of
        <<".", _/binary>> -> purge_local(Rest, Before, Left-1, Context);
        _ ->
            case file:read_link_info(Path, [{time, posix}]) of
                {ok, #file_info{type=directory}} ->
                    case file:list_dir(Path) of
                        {ok, Names} ->
                            Children = [filename:join(Path, N) || N <- Names],
                            purge_local(Children ++ Rest, Before, Left-1, Context);
                        {error, enoent} -> purge_local(Rest, Before, Left-1, Context);
                        {error, Reason} -> error({preview_directory, Path, Reason})
                    end;
                {ok, #file_info{type=regular, mtime=Modified}} when Modified =< Before ->
                    case file:delete(Path) of
                        ok -> stop_readers(Path, Context);
                        {error, enoent} -> ok;
                        {error, Reason} -> error({preview_delete, Path, Reason})
                    end,
                    purge_local(Rest, Before, Left-1, Context);
                {error, enoent} -> purge_local(Rest, Before, Left-1, Context);
                {ok, _} -> purge_local(Rest, Before, Left-1, Context);
                {error, Reason} -> error({preview_stat, Path, Reason})
            end
    end.

stop_readers(Path, Context) ->
    Root = z_convert:to_binary(z_path:media_preview(Context)),
    Size = byte_size(Root),
    <<Root:Size/binary, "/", Relative/binary>> = z_convert:to_binary(Path),
    lists:foreach(fun(Pid) -> catch z_file_entry:stop(Pid) end,
        m_cafs:readers(<<"preview/", Relative/binary>>, Context)).
