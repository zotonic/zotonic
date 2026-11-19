%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Migrate owned medium files into CAFS without bulk remote downloads.
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

-module(z_media_cafs_migrate).
-export([run/3, migrate/2, cleanup/3, queue_cleanup/3, cleanup_preview/4, queue_preview_cleanup/4,
    requested/4, promote_requested/4, is_owned/2, promote_file/4]).

-include_lib("zotonic.hrl").
-include_lib("zotonic_file.hrl").
-include_lib("kernel/include/file.hrl").

%% @doc Opportunistically promote an owned original already requested by a client.
%% Never start a download here, and never delay the response for hashing. Overload
%% or a failed download leaves the legacy path intact for a later request.
-spec requested(Filename, Source, Medium, Context) -> ok when
    Filename :: binary(), Source :: {file, file:filename_all()} | {cache, term()} | {data, binary()},
    Medium :: map(), Context :: z:context().
requested(Filename, Source, Medium, Context) ->
    case m_cafs:is_enabled(Context) andalso owned_key(Filename, Medium) =/= undefined of
        true ->
            case m_cafs:lookup(Filename, Context) of
                {error, enoent} ->
                    case z_sidejob:start(?MODULE, promote_requested, [Filename, Source, Medium], Context) of
                        {ok, _} -> ok;
                        {error, overload} -> ok
                    end;
                {ok, _} -> ok
            end;
        false -> ok
    end.

%% @doc Sidejob entry point. Serialize requests for the same logical file and
%% recheck the medium under its row lock before attaching the staged bytes.
-spec promote_requested(Filename, Source, Medium, Context) -> ok when
    Filename :: binary(), Source :: {file, file:filename_all()} | {cache, term()} | {data, binary()},
    Medium :: map(), Context :: z:context().
promote_requested(Filename, Source, Medium, Context) ->
    Name = {n, l, {?MODULE, requested, z_context:site(Context), Filename}},
    Registered = try gproc:reg(Name) catch error:badarg -> false end,
    case Registered of
        true ->
            try
                case {owned_key(Filename, Medium), m_cafs:lookup(Filename, Context)} of
                    {{Key, Owned}, {error, enoent}} ->
                        promote_requested_source(Filename, Source, Key, Owned, Medium, Context);
                    _ -> ok
                end
            after gproc:unreg(Name) end;
        false -> ok
    end.

-spec is_owned(Filename, Medium) -> boolean() when Filename :: binary(), Medium :: map().
is_owned(Filename, Medium) -> owned_key(Filename, Medium) =/= undefined.

%% @doc Promote available bytes only if the medium still owns the same source.
-spec promote_file(Filename, File, Medium, Context) -> {ok, binary()} | {error, term()} when
    Filename :: binary(), File :: file:filename_all(), Medium :: map(), Context :: z:context().
promote_file(Filename, File, #{<<"id">> := Id} = Medium, Context) ->
    case owned_key(Filename, Medium) of
        {Key, Owned} ->
            case attach_stage(m_cafs:stage_original(File, Context), Id, Key, Owned, Filename, Medium, Context) of
                {attached, Hash} ->
                    z_depcache:flush({medium, Id}, Context),
                    z_depcache:flush({medium, Filename}, Context),
                    {ok, Hash};
                ok -> {error, stale};
                {error, _} = Error -> Error
            end;
        undefined -> {error, unowned}
    end.

owned_key(Filename, #{<<"filename">> := Filename, <<"is_deletable_file">> := true}) ->
    {<<"filename">>, <<"is_deletable_file">>};
owned_key(Filename, #{<<"preview_filename">> := Filename, <<"is_deletable_preview">> := true}) ->
    {<<"preview_filename">>, <<"is_deletable_preview">>};
owned_key(_, _) -> undefined.

promote_requested_source(Filename, {cache, CacheKey}, Key, Owned, Medium, Context) ->
    %% Waiting for a stalled download must not consume a sidejob indefinitely.
    %% The cache lock is process-owned and is released even on timeout or death.
    {ok, Timer} = timer:exit_after(timer:minutes(5), shutdown),
    Result = try z_file_request:cache_file(CacheKey)
        catch exit:_ -> {error, unavailable}
        after timer:cancel(Timer) end,
    case Result of
        {ok, {file, _, File}} ->
            promote_requested_source(Filename, {file, File}, Key, Owned, Medium, Context);
        {error, _} -> ok
    end;
promote_requested_source(Filename, {data, Data}, Key, Owned, Medium, Context) ->
    File = z_tempfile:new(),
    try
        ok = file:write_file(File, Data),
        promote_requested_source(Filename, {file, File}, Key, Owned, Medium, Context)
    after file:delete(File) end;
promote_requested_source(Filename, {file, File}, Key, Owned, #{<<"id">> := Id} = Medium, Context) ->
    Result = attach_stage(m_cafs:stage_original(File, Context), Id, Key, Owned, Filename, Medium, Context),
    case Result of
        {attached, Hash} ->
            z_depcache:flush({medium, Id}, Context),
            z_depcache:flush({medium, Filename}, Context),
            queue_cleanup(Filename, Hash, Context),
            ok;
        ok -> ok;
        {error, Reason} ->
            ?LOG_WARNING(#{text => <<"Requested media CAFS promotion deferred">>,
                in => zotonic_core, filename => Filename, result => error, reason => Reason}),
            ok
    end.

%% Fixed upper ID bounds the scan. Each record has its own durable retry task,
%% so one unavailable backend/file cannot prevent progress on other records.
-spec run(After, Last, Context) -> ok | {delay, pos_integer(), list()} when
    After :: non_neg_integer(), Last :: non_neg_integer(), Context :: z:context().
run(After, Last, Context) ->
    Rows = z_db:q("select id from medium where id > $1 and id <= $2 order by id limit 100",
        [After, Last], Context),
    lists:foreach(fun({Id}) ->
        {ok, _} = z_pivot_rsc:insert_task_after(0, ?MODULE, migrate,
            integer_to_binary(Id), [Id], Context)
    end, Rows),
    case Rows of
        [] -> ok;
        _ -> {Id} = lists:last(Rows), {delay, 1, [Id, Last]}
    end.

-spec migrate(Id, Context) -> ok | {delay, pos_integer(), list()} when
    Id :: integer(), Context :: z:context().
migrate(Id, Context) ->
    case z_db:select(medium, Id, Context) of
        {error, enoent} -> ok;
        {ok, Medium} ->
            Results = [migrate_file(Id, Key, Owned, Medium, Context) || {Key, Owned} <- [
                {<<"filename">>, <<"is_deletable_file">>},
                {<<"preview_filename">>, <<"is_deletable_preview">>}]],
            case [Reason || {error, Reason} <- Results] of
                [] -> ok;
                Errors ->
                    ?LOG_WARNING(#{text => <<"CAFS medium migration will retry">>,
                        in => zotonic_core, id => Id, result => error, reason => Errors}),
                    {delay, 600, [Id]}
            end
    end.

migrate_file(Id, Key, Owned, Medium, Context) ->
    case {maps:get(Key, Medium, undefined), maps:get(Owned, Medium, false)} of
        {Filename, true} when is_binary(Filename), Filename =/= <<>> ->
            case m_cafs:lookup(Filename, Context) of
                {ok, Hash} -> queue_cleanup(Filename, Hash, Context);
                {error, enoent} -> stage(Id, Key, Owned, Filename, Medium, Context)
            end;
        _ -> ok
    end.

stage(Id, Key, Owned, Filename, Medium, Context) ->
    File = z_media_archive:abspath(Filename, Context),
    Result = case file:read_link_info(File) of
        {ok, #file_info{type=regular}} ->
            attach_stage(m_cafs:stage_original(File, Context), Id, Key, Owned, Filename, Medium, Context);
        {error, enoent} ->
            %% Retargeting a remote registry entry and attaching its logical
            %% filename must commit together: the backend has unique locations.
            z_db:transaction(fun(Ctx) ->
                case z_db:qmap_row("select * from medium where id=$1 for update", [Id], Ctx) of
                    {ok, Current} ->
                        case unchanged(Key, Owned, Filename, Medium, Current) of
                            true -> attach_stage(stage_remote(Key, Filename, Medium, Ctx),
                                Id, Key, Owned, Filename, Medium, Ctx);
                            false -> ok
                        end;
                    {error, enoent} -> ok
                end
            end, Context);
        {ok, _} -> {error, not_regular};
        {error, _} = Error -> Error
    end,
    case Result of
        {attached, Hash} ->
            z_depcache:flush({medium, Id}, Context),
            z_depcache:flush({medium, Filename}, Context),
            queue_cleanup(Filename, Hash, Context);
        {rollback, Reason} -> {error, Reason};
        Other -> Other
    end.

unchanged(Key, Owned, Filename, Medium, Current) ->
    maps:get(Key, Current, undefined) =:= Filename
        andalso maps:get(Owned, Current, false)
        andalso (Key =/= <<"filename">> orelse maps:get(<<"digest">>, Current, undefined)
            =:= maps:get(<<"digest">>, Medium, undefined)).

attach_stage(Result, Id, Key, Owned, Filename, Medium, Context) ->
    case Result of
        {ok, #{hash := Hash} = Staged} ->
            Result1 = try
                z_db:transaction(fun(Ctx) ->
                    %% Hashing happens outside the lock. Recheck the complete
                    %% snapshot before attaching to a possibly replaced medium.
                    case z_db:qmap_row("select * from medium where id=$1 for update",
                            [Id], Ctx) of
                        {ok, Current} ->
                            case unchanged(Key, Owned, Filename, Medium, Current)
                            of
                                true ->
                                    ok = m_cafs:attach_original(Filename, Staged, Ctx),
                                    case Key of
                                        <<"filename">> ->
                                            {ok, _} = z_db:update(medium, Id, #{<<"digest">> => Hash}, Ctx);
                                        _ -> ok
                                    end,
                                    {attached, Hash};
                                false -> ok
                            end;
                        {error, enoent} -> ok
                    end
                end, Context)
            after
                m_cafs:release([{Filename, Staged}], Context)
            end,
            case Result1 of
                {rollback, Reason} -> {error, Reason};
                Other -> Other
            end;
        skipped ->
            %% Legacy thumbnail requests can retry this until the original is
            %% downloaded. This expected deferral must not fill normal logs.
            ?LOG_DEBUG(#{text => <<"CAFS migration deferred until file is downloaded; no local bytes or usable checksum available">>,
                in => zotonic_core, id => Id, filename => Filename}),
            ok;
        {error, _} = StageError -> StageError
    end.

stage_remote(<<"filename">>, Filename, Medium, Context) ->
    Hash = maps:get(<<"digest">>, Medium, undefined),
    Size = maps:get(<<"size">>, Medium, undefined),
    case is_binary(Hash) andalso byte_size(Hash) =:= 64
        andalso is_integer(Size) andalso Size >= 0 of
        true ->
            case re:run(Hash, <<"^[0-9a-f]{64}$">>, [{capture, none}]) of
                match -> m_cafs:stage_remote(Filename, Hash, Size, Context);
                nomatch -> skipped
            end;
        false -> skipped
    end;
stage_remote(_, _, _, _) -> skipped.

%% @doc Queue idempotent legacy cleanup after publishing an original mapping.
-spec queue_cleanup(Filename, Hash, Context) -> ok | {error, term()} when
    Filename :: binary(), Hash :: binary(), Context :: z:context().
queue_cleanup(Filename, Hash, Context) ->
    case z_pivot_rsc:insert_task_after_transaction(0, ?MODULE, cleanup, z_crypto:hex_sha2(Filename),
            [Filename, Hash], Context) of
        {ok, _} -> ok;
        {error, _} = Error -> Error
    end.

%% Keep the mapping and original preview images; only retire redundant paths.
%% The CAFS lock prevents collection while checking and removing legacy copies.
-spec cleanup(Filename, Hash, Context) -> ok | {delay, pos_integer(), list()} when
    Filename :: binary(), Hash :: binary(), Context :: z:context().
cleanup(Filename, Hash, Context) ->
    Result = z_db:transaction(fun(Ctx) ->
        State = z_db:q1("select state from cafs where hash=$1 for update", [Hash], Ctx),
        case {State, m_cafs:lookup(Filename, Ctx)} of
            {<<"ready">>, {ok, Hash}} ->
                %% A shared logical path may also be referenced as an unowned
                %% file. Such references must keep their original storage.
                case z_db:q1("select exists(select 1 from medium where
                        (filename=$1 and not is_deletable_file) or
                        (preview_filename=$1 and not is_deletable_preview))", [Filename], Ctx) of
                    true -> ok;
                    false -> cleanup_file(Filename, Hash, Ctx)
                end;
            _ -> ok
        end
    end, Context),
    case Result of
        ok -> ok;
        _ -> {delay, 600, [Filename, Hash]}
    end.

%% @doc Retire a promoted generated preview after its derivative is published.
-spec queue_preview_cleanup(Path, Hash, Adopt, Context) -> ok when
    Path :: binary(), Hash :: binary(), Adopt :: boolean(), Context :: z:context().
queue_preview_cleanup(Path, Hash, Adopt, Context) ->
    {ok, _} = z_pivot_rsc:insert_task_after_transaction(0, ?MODULE, cleanup_preview,
        z_crypto:hex_sha2(Path), [Path, Hash, Adopt], Context),
    ok.

-spec cleanup_preview(Path, Hash, Adopt, Context) -> ok | {delay, pos_integer(), list()} when
    Path :: binary(), Hash :: binary(), Adopt :: boolean(), Context :: z:context().
cleanup_preview(Path, Hash, Adopt, Context) ->
    Result = z_db:transaction(fun(Ctx) ->
        case z_db:q1("select state from cafs where hash=$1 for update", [Hash], Ctx) of
            <<"ready">> ->
                case z_db:q1("select 1 from cafs_derivative where output_hash=$1 limit 1", [Hash], Ctx) of
                    1 -> cleanup_path(<<"preview/", Path/binary>>, Hash, Adopt, Ctx);
                    undefined -> ok
                end;
            _ -> ok
        end
    end, Context),
    case Result of
        ok -> ok;
        _ -> {delay, 600, [Path, Hash, Adopt]}
    end.

cleanup_file(Filename, Hash, Context) ->
    case <<"archive/", Filename/binary>> =:= m_cafs:path(Hash) of
        true -> ok;
        false -> cleanup_legacy(Filename, Hash, Context)
    end.

cleanup_legacy(Filename, Hash, Context) ->
    cleanup_path(<<"archive/", Filename/binary>>, Hash, Context).

cleanup_path(Source, Hash, Context) -> cleanup_path(Source, Hash, true, Context).

cleanup_path(Source, Hash, Adopt, Context) ->
    Target = m_cafs:path(Hash),
    Local = filelib:is_regular(z_path:files_subdir(Target, Context)),
    %% With different legacy bytes, only verify the canonical remote object;
    %% never retarget the legacy object under another content hash.
    AdoptSource = case Adopt of true -> Source; false -> Target end,
    Adoption = remote(cafs_adopt, AdoptSource, Target, Context),
    Available = case Adoption of
        ok ->
            z_db:q("update cafs set upload_pending=false where hash=$1", [Hash], Context),
            true;
        {error, enoent} when Local ->
            z_db:q("update cafs set upload_pending=true where hash=$1", [Hash], Context),
            true;
        _ -> false
    end,
    case Available andalso remote(cafs_retire, Source, Target, Context) =:= ok of
        true ->
            %% Do not stop a reader or unlink its file while its response may
            %% still be opening it. Retry after legacy entries have gone idle.
            case [Pid || Pid <- m_cafs:readers(Source, Context),
                    Pid =/= self(), is_process_alive(Pid)] of
                [] ->
                    case file:delete(z_path:files_subdir(Source, Context)) of
                        ok -> ok;
                        {error, enoent} -> ok;
                        {error, _} = Error -> Error
                    end;
                _ -> {error, readers}
            end;
        false -> {error, pending}
    end.

remote(Action, Source, Target, Context) ->
    case z_notifier:first(#filestore{action=Action, path=Source, target_path=Target}, Context) of
        undefined ->
            case m_config:get_boolean(site, filestore_used, Context) of
                false when Action =:= cafs_retire -> ok;
                false when Action =:= cafs_adopt -> {error, enoent};
                _ -> {error, unavailable}
            end;
        Result -> Result
    end.
