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
-export([run/3, migrate/2, cleanup/3]).

-include_lib("zotonic.hrl").
-include_lib("zotonic_file.hrl").
-include_lib("kernel/include/file.hrl").

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
            attach_stage(m_cafs:stage_file(File, Context), Id, Key, Owned, Filename, Medium, Context);
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
                                    ok = m_cafs:attach(Filename, Staged, Ctx),
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
            ?LOG_NOTICE(#{text => <<"CAFS migration skipped file without local bytes or checksum">>,
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

queue_cleanup(Filename, Hash, Context) ->
    case z_pivot_rsc:insert_task_after(0, ?MODULE, cleanup, z_crypto:hex_sha2(Filename),
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
            {<<"ready">>, {ok, Hash}} -> cleanup_file(Filename, Hash, Ctx);
            _ -> ok
        end
    end, Context),
    case Result of
        ok -> ok;
        _ -> {delay, 600, [Filename, Hash]}
    end.

cleanup_file(Filename, Hash, Context) ->
    case <<"archive/", Filename/binary>> =:= m_cafs:path(Hash) of
        true -> ok;
        false -> cleanup_legacy(Filename, Hash, Context)
    end.

cleanup_legacy(Filename, Hash, Context) ->
    Source = <<"archive/", Filename/binary>>,
    Target = m_cafs:path(Hash),
    Local = filelib:is_regular(z_path:files_subdir(Target, Context)),
    Available = Local orelse remote(cafs_adopt, Source, Target, Context) =:= ok,
    case Available andalso remote(cafs_retire, Source, Target, Context) =:= ok of
        true ->
            %% Stop cached legacy readers after publishing the canonical mapping.
            lists:foreach(fun(Pid) -> catch z_file_entry:stop(Pid) end,
                m_cafs:readers(Source, Context)),
            case file:delete(z_media_archive:abspath(Filename, Context)) of
                ok -> ok;
                {error, enoent} -> ok;
                {error, _} = Error -> Error
            end;
        false -> {error, pending}
    end.

remote(Action, Source, Target, Context) ->
    case z_notifier:first(#filestore{action=Action, path=Source, target_path=Target}, Context) of
        undefined ->
            case m_config:get_boolean(site, filestore_used, Context) of
                false when Action =:= cafs_retire -> ok;
                _ -> {error, unavailable}
            end;
        Result -> Result
    end.
