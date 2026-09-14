%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012-2026 Marc Worrell
%% @doc Manage a resource's revisions, manages a table of edge changes
%% @end
% and medium records.

%% Copyright 2012-2026 Marc Worrell
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

-module(m_backup_revision).
-moduledoc(#{
    zotonic_keywords => ["reference", "operator", "model", "backup_and_restore", "resource", "metadata", "query"]
}).
-moduledoc("
Model for resource revision backup metadata, including revision list/title retrieval and revision retention settings.

Revision reads require admin access and edit permission on the live resource or its deleted ACL snapshot.
Restoration additionally checks insert permission in the selected category and content group. Missing
category/content-group references require explicit choices; local followups are suggestions only.
Missing creator/modifier references are followed to a live resource or cleared.

Revisions store URI aliases as `backup_uri_aliases` metadata, separate from resource properties.
Deletion revisions are saved before aliases are cascade-deleted. Recovery and rollback add the archived
aliases without removing current aliases or taking aliases assigned to another resource. Older revisions
without alias metadata leave the current aliases unchanged.

The deleted overview queries only `rsc_gone`, never serialized revision bodies. Its ACL migration runs
in resumable keyset batches; unmigrated entries remain hidden until their metadata is available.

Available Model API Paths
-------------------------

| Method | Path pattern | Description |
| --- | --- | --- |
| `get` | `/can_view/+id/...` | Whether the caller may read this resource’s revisions. |
| `get` | `/restore_options/+revision_id/...` | Authorized historical reference labels and followup suggestions. |
| `get` | `/list/+id/...` | Return revision history entries for resource `+id` as associative rows, newest-first where applicable. |
| `get` | `/title/+id/...` | Return title for `+id`: current resource title when it exists, otherwise archived revision title (saved resource update permission). |
| `get` | `/retention_months/...` | Return `mod_backup.revision_retention_months` (months to keep revision backups). |
| `get` | `/user_retention_days/...` | Return retention period in days for backups tied to active user resources (`backup_config:user_retention_days/1`). |
| `get` | `/deleted_user_retention_days/...` | Return retention period in days for backups tied to deleted user resources (`backup_config:deleted_user_retention_days/1`). |

`/+name` marks a variable path segment. A trailing `/...` means extra path segments are accepted for further lookups.
").

-behaviour(zotonic_model).

-export([
    m_get/3,

    revert_resource/4,
    can_view/2,
    restore_options/2,

    list_deleted/2,

    revision_title/2,
    historical_label/2,
    latest_props/2,
    save_deleted/3,
    save_revision/3,
    get_revision/2,
    list_revisions/2,
    list_revisions_assoc/2,

    edge_insert/4,
    edge_delete/4,

    medium_insert/3,
    medium_update/3,
    medium_delete/3,
    medium_delete_check/2,

    periodic_cleanup/1,

    install/1,

    insert_deleted_revisions/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

% The revision log contains entries for updates (P) and the props
% saved when deleting a resource (D).
-define(BACKUP_TYPE_PROPS, $P).
-define(BACKUP_TYPE_PROPS_DELETED, $D).

% Number of seconds a medium-log entry can be older than a revision
% and still be recovered as part of that revision. The min is always
% accepted, the max is used if no more recent medium-log item is found.
% We keep a small margin as the properties log entry can be saved after
% the medium entry is marked as deleted.
-define(DELTA_MEDIUM_REVERT_MIN, 2).
-define(DELTA_MEDIUM_REVERT_MAX, 15).

% Number of seconds an edge-log entry can be older than a revision
% and still be recovered as part of that revision.
% We keep a small margin as the properties log entry can be saved after
% the edge entries are made.
-define(DELTA_LOG_REVERT, 2).

-type revert_option() :: incoming_edges | outgoing_edges | dependent
    | {category_id, integer()} | {content_group_id, integer()}.
-export_type([ revert_option/0 ]).

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"can_view">>, Id | Rest ], _Msg, Context) ->
    {ok, {can_view(Id, Context), Rest}};
m_get([ <<"restore_options">>, RevId | Rest ], _Msg, Context) ->
    {ok, {restore_options(RevId, Context), Rest}};
m_get([ <<"list">>, Id | Rest ], _Msg, Context) ->
    Id1 = m_rsc:rid(Id, Context),
    Revs = case can_view(Id1, Context) of
        true -> list_revisions_assoc(Id1, Context);
        false -> []
    end,
    {ok, {Revs, Rest}};
m_get([ <<"title">>, Id | Rest ], _Msg, Context) ->
    Id1 = m_rsc:rid(Id, Context),
    Title = case m_rsc:exists(Id, Context) of
        true ->
            m_rsc:p(Id, <<"title">>, Context);
        false ->
            case can_view(Id1, Context) of
                true -> revision_title(Id1, Context);
                false -> undefined
            end
    end,
    {ok, {Title, Rest}};
m_get([ <<"retention_months">> | Rest ], _Msg, Context) ->
    {ok, {backup_config:retention_months(Context), Rest}};
m_get([ <<"user_retention_days">> | Rest ], _Msg, Context) ->
    {ok, {backup_config:user_retention_days(Context), Rest}};
m_get([ <<"deleted_user_retention_days">> | Rest ], _Msg, Context) ->
    {ok, {backup_config:deleted_user_retention_days(Context), Rest}};
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.


%% @doc Revert a resource to the given revision. Also restores edges and the medium record that
%% matches to that revision and date.
-spec revert_resource(RscId, RevisionId, Options, Context) -> ok | {error, Reason} when
    RscId :: m_rsc:resource_id(),
    RevisionId :: integer(),
    Options :: [ revert_option() ],
    Context :: z:context(),
    Reason :: invalid | enoent | term().
revert_resource(RscId, RevId, Options, Context) ->
    case get_revision(RevId, Context) of
        {ok, #{
            <<"rsc_id">> := RevRscId,
            <<"data">> := Props,
            <<"created">> := Created
        }} when RevRscId =:= RscId ->
            case restore_props(RscId, Props, Options, Context) of
                {ok, _} ->
                    revert_edges(RscId, Options, Created, Context),
                    revert_medium(RscId, Created, Context),
                    z_depcache:flush(RscId, Context),
                    ok;
                {error, _} = Error ->
                    Error
            end;
        {ok, _} ->
            {error, invalid};
        {error, _} = Error ->
            Error
    end.

%% @doc Revert a resource if and only if the resource was a dependent resource.
%% Revert to the newest version in the revision table.
revert_if_dependent(RscId, Options, Context) ->
    case z_db:qmap_row("
        select *
        from backup_revision
        where rsc_id = $1
        order by created desc, id desc
        limit 1",
        [RscId],
        Context)
    of
        {ok, #{
            <<"id">> := RevId,
            <<"data">> := Data
        }} ->
            IsRevert = case erlang:binary_to_term(Data) of
                #{ <<"is_dependent">> := IsDependent } ->
                    z_convert:to_bool(IsDependent);
                Props when is_list(Props) ->
                    z_convert:to_bool(proplists:get_value(is_dependent, Props, false));
                _ ->
                    false
            end,
            if
                IsRevert ->
                    % Reference choices belong to the selected page, not its dependents.
                    EdgeOptions = [Opt || Opt <- Options, is_atom(Opt)],
                    revert_resource(RscId, RevId, EdgeOptions, Context);
                true -> ok
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Replay all edges in reverse. This creates a view of all edges at a certain
%% moment in time. If the resource was deleted, then we also recover all referring edges.
revert_edges(RscId, Options, Created, Context) ->
    CreatedEdge = z_datetime:prev_second(Created, ?DELTA_LOG_REVERT),
    Objects = z_db:q("
        select subject_id, predicate, object_id, is_insert
        from backup_edge_log
        where subject_id = $1
          and timestamp >= $2
        order by id desc",
        [RscId, CreatedEdge],
        Context),
    RevertIn = lists:member(incoming_edges, Options),
    RevertOut = lists:member(outgoing_edges, Options),
    Subjects = if
        RevertIn ->
            z_db:q("
                select subject_id, predicate, object_id, is_insert
                from backup_edge_log
                where object_id = $1
                  and timestamp >= $2
                order by id desc",
                [RscId, CreatedEdge],
                Context);
        true ->
            []
    end,
    Edges = Objects ++ Subjects,
    % Incoming edges are only recovered if the resource currently exists.
    Recover = lists:foldl(
        fun
            ({SubjectId, Predicate, ObjectId, false}, Acc) when RevertIn, RscId =:= SubjectId ->
                Acc#{ {SubjectId, Predicate, ObjectId} => insert };
            ({SubjectId, Predicate, ObjectId, true}, Acc) when RevertIn, RscId =:= SubjectId ->
                Acc#{ {SubjectId, Predicate, ObjectId} => delete };
            ({SubjectId, Predicate, ObjectId, false}, Acc) when RevertOut, RscId =:= ObjectId ->
                Acc#{ {SubjectId, Predicate, ObjectId} => insert };
            ({SubjectId, Predicate, ObjectId, true}, Acc) when RevertOut, RscId =:= ObjectId ->
                Acc#{ {SubjectId, Predicate, ObjectId} => delete };
            (_E, Acc) ->
                Acc
        end,
        #{},
        Edges),
    maps:foreach(
        fun
            ({SubjectId, Predicate, ObjectId}, insert) ->
                RevertDependent = lists:member(dependent, Options),
                case {m_rsc:exists(SubjectId, Context), m_rsc:exists(ObjectId, Context)} of
                    {true, true} ->
                        m_edge:insert(SubjectId, Predicate, ObjectId, Context);
                    {true, false} when RevertDependent ->
                        case m_rsc:exists(Predicate, Context) of
                            true ->
                                case revert_if_dependent(ObjectId, Options, Context) of
                                    ok ->
                                        m_edge:insert(SubjectId, Predicate, ObjectId, Context);
                                    {error, _} ->
                                        ok
                                end;
                            false ->
                                ok
                        end;
                    {_, _} ->
                        ok
                end;
            ({SubjectId, Predicate, ObjectId}, delete) ->
                m_edge:delete(SubjectId, Predicate, ObjectId, Context)
        end,
        Recover).

%% @doc Revert the medium record that best matches the date of the reverted resource.
%% Fetch the medium record that existed around the time of the revision.
%% This is not exact science as it is not guaranteed that a resource revision
%% was saved when the medium record was created. So we fetch the medium record
%% that best matched, but could actually have been created much later.
revert_medium(Id, Created, Context) ->
    case z_db:qmap_row("
        select *
        from backup_medium_log
        where rsc_id = $1
          and (medium_deleted is null or medium_deleted >= $2)
        order by medium_created
        limit 1",
        [ Id, z_datetime:prev_second(Created, ?DELTA_MEDIUM_REVERT_MIN) ],
        Context)
    of
        {ok, MediumLog} ->
            revert_medium_1(MediumLog, Context);
        {error, _} ->
            % Might be a timing issue - check if there was a very
            % recent deleted medium record, if so, then take that one.
            case z_db:qmap_row("
                select *
                from backup_medium_log
                where rsc_id = $1
                  and medium_deleted >= $2
                order by medium_created
                limit 1",
                [ Id, z_datetime:prev_second(Created, ?DELTA_MEDIUM_REVERT_MAX) ],
                Context)
            of
                {ok, MediumLog} ->
                    revert_medium_1(MediumLog, Context);
                {error, _} = Error ->
                    Error
            end
    end.

revert_medium_1(#{
            <<"rsc_id">> := Id,
            <<"props">> := Medium,
            <<"medium_deleted">> := DeletedDate
        }, Context) ->
    case m_media:get(Id, Context) of
        CurrentMedium when CurrentMedium =:= Medium, DeletedDate =:= undefined ->
            % Still current medium record
            ok;
        undefined ->
            % No medium record - re-insert the revision
            m_media:recover_medium(Medium, Context);
        _Medium ->
            % Current one is different - delete it and
            % re-insert the revision
            m_media:delete(Id, Context),
            m_media:recover_medium(Medium, Context)
    end.


-spec revision_title(Id, Context) -> Title when
    Id :: m_rsc:resource_id(),
    Context :: z:context(),
    Title :: binary() | z:trans() | undefined | term().
revision_title(Id, Context) ->
    case z_db:q1("
        select data
        from backup_revision
        where rsc_id = $1
        order by created desc
        limit 1",
        [ Id ], Context)
    of
        Data when is_binary(Data) ->
            case binary_to_term(Data) of
                #{ <<"title">> := Title } -> Title;
                #{ <<"short_title">> := Title } -> Title;
                #{ <<"subtitle">> := Title } -> Title;
                Props when is_list(Props) -> proplists:get_value(title, Props);
                _ -> undefined
            end;
        undefined ->
            undefined
    end.

-spec list_deleted(OffsetLimit, Context) -> Result when
    OffsetLimit :: {non_neg_integer(), non_neg_integer()},
    Context :: z:context(),
    Result :: #search_result{}.
list_deleted({Offset, Limit}, Context) ->
    case z_acl:is_allowed(use, mod_admin, Context) of
        false -> #search_result{};
        true ->
            {AclSql, Args} = m_rsc_gone:acl_sql("g", Context),
            Where = ["g.category_id IS NOT NULL AND g.content_group_id IS NOT NULL AND (", AclSql, ")"],
            N = length(Args),
            {ok, Rs} = z_db:qmap_props([
                "select g.* from rsc_gone g where ", Where,
                " order by g.modified desc, g.id desc offset $", integer_to_list(N+1),
                " limit $", integer_to_list(N+2)],
                Args ++ [Offset-1, Limit], Context),
            Total = z_db:q1(["select count(*) from rsc_gone g where ", Where], Args, Context),
            #search_result{ result = Rs, total = Total, is_total_estimated = false }
    end.


save_deleted(_Id, undefined, _Context) ->
    ok;
save_deleted(Id, Props, Context) when is_integer(Id), is_map(Props) ->
    z_db:transaction(fun(Ctx) ->
        ok = save_revision(Id, Props, true, Ctx),
        % Keep the cached title consistent with the deletion revision.
        z_db:q("update rsc_gone set props_json = coalesce(props_json, '{}'::jsonb) || $2::jsonb
                where id = $1", [Id, ?DB_PROPS_JSON(maps:with([<<"title">>], Props))], Ctx),
        ok
    end, Context).

save_revision(Id, Props, Context) when is_integer(Id), is_map(Props) ->
    save_revision(Id, Props, false, Context).

save_revision(Id, #{ <<"version">> := Version } = Props, IsDeleted, Context) when is_integer(Id) ->
    LastVersion = z_db:q1("
        select version
        from backup_revision
        where rsc_id = $1
        order by created desc
        limit 1", [Id], Context),
    if
        Version =:= LastVersion, LastVersion =/= undefined, not IsDeleted ->
            ok;
        true ->
            UserId = z_acl:user(Context),
            Type = case IsDeleted of
                false ->
                    ?BACKUP_TYPE_PROPS;
                true ->
                    ?BACKUP_TYPE_PROPS_DELETED
            end,
            % Revision-only metadata, removed before updating resource properties.
            SavedProps = Props#{ <<"backup_uri_aliases">> => m_rsc:uri_aliases(Id, Context) },
            RevId = z_db:q1("
                insert into backup_revision
                    (rsc_id, type, version, user_id, user_name, data_type, data)
                values ($1, $2, $3, $4, $5, $6, $7)
                returning id",
                [
                    Id,
                    Type,
                    Version,
                    UserId,
                    z_string:truncatechars(
                        z_trans:lookup_fallback(
                            m_rsc:p_no_acl(UserId, title, Context),
                            Context),
                        60),
                    <<"erlang">>,
                    erlang:term_to_binary(SavedProps, [compressed])
                ],
                Context),
            case IsDeleted of
                true ->
                    z_db:q("
                        delete from backup_revision
                        where type = $1
                          and rsc_id = $2
                          and id <> $3
                        ",
                        [ ?BACKUP_TYPE_PROPS_DELETED, Id, RevId ],
                        Context);
                false ->
                    ok
            end,
            ok
    end.

% @doc Fetch a specific revision by its unique id.
-spec get_revision(RevisionId, Context) -> {ok, Revision} | {error, Reason} when
    RevisionId :: integer() | latest,
    Context :: z:context(),
    Revision :: map(),
    Reason :: enoent | term().
get_revision(RevId0, Context) ->
    RevId = z_convert:to_integer(RevId0),
    case z_db:qmap_row("select * from backup_revision where id = $1", [RevId], Context) of
        {ok, #{ <<"data">> := Data } = Row} ->
            case can_view(maps:get(<<"rsc_id">>, Row), Context) of
                true -> {ok, Row#{ <<"data">> => revision_props(Data) }};
                false -> {error, eacces}
            end;
        {error, _} = Error ->
            Error
    end.

list_revisions(undefined, _Context) ->
    [];
list_revisions(Id, Context) when is_integer(Id) ->
    z_db:q("
        select id, type, created, version, user_id, user_name
        from backup_revision
        where rsc_id = $1
        order by created desc", [Id], Context);
list_revisions(Id, Context) ->
    list_revisions(m_rsc:rid(Id, Context), Context).

list_revisions_assoc(undefined, _Context) ->
    [];
list_revisions_assoc(Id, Context) when is_integer(Id) ->
    z_db:assoc("
        select id, type, created, version, user_id, user_name
        from backup_revision
        where rsc_id = $1
        order by created desc", [Id], Context);
list_revisions_assoc(Id, Context) ->
    list_revisions_assoc(m_rsc:rid(Id, Context), Context).


%% @doc Add a new edge to the edge backup log. Separate insert/delete events
%% are logged, so that the state at a certain date can be recovered by replaying
%% the insert/delete events in reverse.
-spec edge_insert(SubjectId, Predicate, ObjectId, Context) -> ok when
    SubjectId :: integer(),
    Predicate :: atom(),
    ObjectId :: integer(),
    Context :: z:context().
edge_insert(SubjectId, Predicate, ObjectId, Context) ->
    z_db:q("
        insert into backup_edge_log
            (subject_id, predicate, object_id, is_insert)
        values
            ($1, $2, $3, true)",
        [ SubjectId, Predicate, ObjectId ],
        Context),
    ok.

%% @doc Add a deleted edge to the edge backup log. Separate insert/delete events
%% are logged, so that the state at a certain date can be recovered by replaying
%% the insert/delete events in reverse.
-spec edge_delete(SubjectId, Predicate, ObjectId, Context) -> ok when
    SubjectId :: integer(),
    Predicate :: atom(),
    ObjectId :: integer(),
    Context :: z:context().
edge_delete(SubjectId, Predicate, ObjectId, Context) ->
    z_db:q("
        insert into backup_edge_log
            (subject_id, predicate, object_id, is_insert)
        values
            ($1, $2, $3, false)",
        [ SubjectId, Predicate, ObjectId ],
        Context),
    ok.

%% @doc Register the (manual) insert of a medium record. Individual medium
%% records are registered by the resource-id and their creation date. Updates of
%% medium records are always only extra properties, an added preview file or
%% the definitive file after (e.g.) a video conversion. So it is always save
%% to overwrite the medium properties of the backup with the new properties.
-spec medium_insert(RscId, Props, Context) -> ok when
    RscId :: m_rsc:resource_id(),
    Props :: map(),
    Context :: z:context().
medium_insert(RscId, #{ <<"created">> := Created } = Props, Context) ->
    z_db:q("
        update backup_medium_log
        set medium_deleted = now()
        where rsc_id = $1
          and medium_deleted is null
          and medium_created <> $2
        ",
        [ RscId, Created ],
        Context),
    z_db:q("
        insert into backup_medium_log
            (rsc_id, props, medium_created)
        values
            ($1, $2, $3)
        on conflict (rsc_id, medium_created)
        do nothing",
        [ RscId, ?DB_PROPS(Props), Created ],
        Context),
    ok.

%% @doc Register the (manual) update of a medium record. Individual medium
%% records are registered by the resource-id and their creation date. If a
%% matching medium record could not be found then it is inserted.
-spec medium_update(RscId, Props, Context) -> ok when
    RscId :: m_rsc:resource_id(),
    Props :: map(),
    Context :: z:context().
medium_update(RscId, #{ <<"created">> := Created } = Props, Context) ->
    z_db:q("
        insert into backup_medium_log
            (rsc_id, props, medium_created)
        values
            ($1, $2, $3)
        on conflict (rsc_id, medium_created)
        do update
        set props = excluded.props",
        [ RscId, ?DB_PROPS(Props), Created ],
        Context),
    ok.

%% @doc Register the (manual) deletion of a medium record. Individual medium
%% records are registered by the resource-id and their creation date. If a
%% matching medium record could not be found then it is inserted. The deletion
%% is marked by setting the medium_deleted column to the current timestamp.
-spec medium_delete(RscId, Props, Context) -> ok when
    RscId :: m_rsc:resource_id(),
    Props :: map(),
    Context :: z:context().
medium_delete(RscId, #{ <<"created">> := Created } = Props, Context) ->
    z_db:q("
        insert into backup_medium_log
            (rsc_id, props, medium_created, medium_deleted)
        values
            ($1, $2, $3, now())
        on conflict (rsc_id, medium_created)
        do update
        set medium_deleted = excluded.medium_deleted",
        [ RscId, ?DB_PROPS(Props), Created ],
        Context),
    ok.

%% @doc Called on a (manual) rsc delete. Check if the resource has a medium
%% and if so, checks if the medium is in the medium backup log. This is
%% used to ensure that no medium records are missing. Especially useful with
%% older content or when mod_backup is enabled after the medium creation.
-spec medium_delete_check(Id, Context) -> ok when
    Id :: m_rsc:resource_id(),
    Context :: z:context().
medium_delete_check(Id, Context) ->
    case m_media:get(Id, Context) of
        undefined ->
            ok;
        #{ <<"created">> := Created } = Medium ->
            case z_db:q1("
                select count(*)
                from backup_medium_log
                where rsc_id = $1
                  and medium_created = $2",
                [Id, Created],
                Context)
            of
                0 -> medium_delete(Id, Medium, Context);
                _ -> ok
            end
    end.

%% @doc Deletes:
%% - any revision older than:
%%   mod_backup.revision_retention_months (defaults to 18 months);
%% - any user's resource revision older than:
%%   mod_backup.user_revision_retention_days (defaults to 90 days);
%% - any user's resource revision for users deleted for more than:
%%   mod_backup.user_deletion_retention_days (defaults to 30 days);
-spec periodic_cleanup(Context) -> ok when
    Context :: z:context().
periodic_cleanup(Context) ->
    % Commit removal of revisions and their cached deleted-page titles together.
    z_db:transaction(fun periodic_cleanup_1/1, Context).

periodic_cleanup_1(Context) ->
    Months = backup_config:retention_months(Context),
    Threshold = z_datetime:prev_month(calendar:universal_time(), Months),
    Expired = z_db:q("
        delete from backup_revision
        where created < $1
        returning rsc_id",
        [Threshold],
        Context),
    z_db:q("
        delete from backup_medium_log
        where medium_deleted < $1",
        [Threshold],
        Context),
    z_db:q("
        delete from backup_edge_log
        where timestamp < $1",
        [Threshold],
        Context),

    % Join with the 'identity' table to find revisions of user resources
    UserRevDays = backup_config:user_retention_days(Context),
    UserRevThreshold = z_datetime:prev_day(calendar:universal_time(), UserRevDays),
    % see 'm_identity:is_user/2':
    IdentityTypes = m_identity:user_types(Context),
    IdentityTypes1 = [ z_convert:to_binary(Idn) || Idn <- lists:usort(IdentityTypes) ],
    UserExpired = z_db:q("
        DELETE FROM backup_revision
        WHERE created < $1
        AND rsc_id IN (SELECT rsc_id FROM identity WHERE type = any($2))
        RETURNING rsc_id",
        [UserRevThreshold, IdentityTypes1],
        Context),

    % Join with 'rsc_gone' to find user resources that have been deleted
    UserDelDays = backup_config:deleted_user_retention_days(Context),
    UserDelThreshold = z_datetime:prev_day(calendar:universal_time(), UserDelDays),
    DeletedUserExpired = z_db:q("
        DELETE FROM backup_revision
        WHERE rsc_id IN (
            SELECT id FROM rsc_gone
            WHERE is_personal_data = true
            AND modified < $1
        )
        RETURNING rsc_id",
        [UserDelThreshold],
        Context),
    % Only check resources whose revisions were pruned in this transaction.
    PrunedIds = lists:usort([ Id || {Id} <- Expired ++ UserExpired ++ DeletedUserExpired ]),
    case PrunedIds of
        [] ->
            ok;
        _ ->
            z_db:q("update rsc_gone g
                    set props_json = props_json - 'title'
                    where g.id = any($1)
                      and props_json ? 'title'
                      and not exists (select 1 from backup_revision b where b.rsc_id = g.id)",
                [PrunedIds],
                Context)
    end,
    ok.



%% @doc Install the revisions table.
install(Context) ->
    case z_db:table_exists(backup_revision, Context) of
        false ->
            [] = z_db:q("
                    create table backup_revision (
                        id bigserial not null,
                        type character(1) not null,
                        rsc_id integer not null,
                        created timestamp with time zone not null default current_timestamp,
                        version integer,
                        user_id integer,
                        user_name character varying(80),
                        filename character varying(400),
                        note character varying(200),
                        data_type character varying(10) not null,
                        data bytea not null,

                        primary key (id)
                    )
                ", Context),
            [] = z_db:q("
                    create index backup_revision_id_created on backup_revision (rsc_id, created)
                ", Context),
            [] = z_db:q("
                    create index backup_revision_created_deleted_key
                    on backup_revision (created, type)
                    where type = 'D'
                ", Context),
            z_db:flush(Context);
        true ->
            case z_db:key_exists(backup_revision, backup_revision_created_deleted_key, Context) of
                true ->
                    ok;
                false ->
                    [] = z_db:q("
                            create index backup_revision_created_deleted_key
                            on backup_revision (created, type)
                            where type = 'D'
                        ", Context),
                    z_db:flush(Context)
            end
    end,
    case z_db:table_exists(backup_edge_log, Context) of
        false ->
            [] = z_db:q("
                    create table backup_edge_log (
                        id bigserial not null,
                        subject_id int not null,
                        predicate character varying(300),
                        object_id int not null,
                        is_insert boolean not null default true,
                        timestamp timestamp with time zone not null default current_timestamp,

                        primary key (id)
                    )
                ", Context),
            [] = z_db:q("
                    create index backup_edge_log_subject_id_created_key on backup_edge_log (subject_id, timestamp)
                ", Context),
            [] = z_db:q("
                    create index backup_edge_log_object_id_created_key on backup_edge_log (object_id, timestamp)
                ", Context),
            z_db:flush(Context);
        true ->
            ok
    end,
    case z_db:table_exists(backup_medium_log, Context) of
        false ->
            [] = z_db:q("
                    create table backup_medium_log (
                        id bigserial not null,
                        rsc_id int not null,
                        props bytea not null,
                        medium_created timestamp with time zone not null,
                        medium_deleted timestamp with time zone default null,

                        primary key (id),
                        constraint backup_medium_log_rsc_id_medium_created_key unique (rsc_id, medium_created)
                    )
                ", Context),
            z_db:flush(Context);
        true ->
            ok
    end,
    case z_db:function_exists("backup_medium_log_delete", Context) of
        false ->
            [] = z_db:q(backup_medium_log_delete(), Context),
            [] = z_db:q(backup_medium_log_delete_trigger(), Context),
            z_db:flush(Context);
        true ->
            ok
    end,
    ok.

backup_medium_log_delete() ->
    "
    CREATE OR REPLACE FUNCTION backup_medium_log_delete() RETURNS trigger AS $$
    begin
        if (tg_op = 'DELETE') then
            update backup_medium_log
            set medium_deleted = now()
            where rsc_id = old.id
              and medium_deleted is null;
        end if;
        return null;
    end;
    $$ LANGUAGE plpgsql
    ".

backup_medium_log_delete_trigger() ->
    "
    CREATE TRIGGER backup_medium_log_delete_trigger AFTER DELETE
    ON medium FOR EACH ROW EXECUTE PROCEDURE backup_medium_log_delete()
    ".

%% @doc Complete an obsolete queued migration. The deleted-page ACL migration
%% is started only by the mod_backup schema version 6 upgrade.
insert_deleted_revisions(_Context) ->
    ok.


%% @doc Revision data is available only to admin users who can edit the live or saved resource.
-spec can_view(Id, Context) -> boolean() when Id :: term(), Context :: z:context().
can_view(Id0, Context) ->
    Id = m_rsc:rid(Id0, Context),
    z_acl:is_allowed(use, mod_admin, Context)
    andalso case m_rsc:exists(Id, Context) of
        true -> z_acl:rsc_editable(Id, Context);
        false -> m_rsc_gone:is_editable(Id, Context)
    end.

%% @doc Resolve historical references for a confirmation dialog, after checking revision access.
-spec restore_options(RevId, Context) -> map() | undefined when
    RevId :: term(), Context :: z:context().
restore_options(RevId, Context) ->
    case get_revision(RevId, Context) of
        {ok, #{ <<"rsc_id">> := RscId, <<"data">> := Props }} ->
            Saved = case z_db:qmap_props_row("select * from rsc_gone where id = $1", [RscId], Context) of
                {ok, Gone} -> maps:get(<<"references">>, Gone, #{});
                _ -> #{}
            end,
            maps:from_list([
                {Key, reference_option(Key, Props, Saved, Context)}
                || Key <- [<<"category_id">>, <<"content_group_id">>] ]);
        _ -> undefined
    end.

reference_option(Key, Props, Saved, Context) ->
    Id = maps:get(Key, Props, undefined),
    Resolved = m_rsc_gone:followup(Id, Context),
    Label = case maps:get(Key, Saved, #{}) of
        #{ <<"id">> := Id } = L -> L;
        _ -> historical_label(Id, Context)
    end,
    Label#{ <<"id">> => Id, <<"suggested_id">> => Resolved,
            <<"is_missing">> => not m_rsc:exists(Id, Context) }.

restore_props(Id, Props, Options, Context) ->
    case can_view(Id, Context) of
        false -> {error, eacces};
        true ->
            case restore_references(Props, Options, Context) of
                {error, _} = Error -> Error;
                {ok, Props1} ->
                    Cat = maps:get(<<"category_id">>, Props1),
                    CG = maps:get(<<"content_group_id">>, Props1),
                    IsNew = not m_rsc:exists(Id, Context),
                    NeedsInsert = IsNew
                        orelse Cat =/= m_rsc:p_no_acl(Id, category_id, Context)
                        orelse CG =/= m_rsc:p_no_acl(Id, content_group_id, Context),
                    case not NeedsInsert orelse z_acl:is_allowed(insert,
                        #acl_rsc{ category = Cat, props = Props1 }, Context)
                    of
                        false -> {error, eacces};
                        true ->
                            % The archived update and target insert permissions were checked above.
                            % Keep placeholder insertion, update and tombstone removal atomic.
                            z_db:transaction(fun(Ctx) ->
                                z_db:q("select id from rsc_gone where id = $1 for update", [Id], Ctx),
                                case can_view(Id, Ctx) of
                                    false -> {rollback, {error, eacces}};
                                    true ->
                                        UpdateOptions = [is_import, {is_acl_check, not IsNew}],
                                        ResourceProps = maps:remove(<<"backup_uri_aliases">>, Props1),
                                        case m_rsc_update:update(Id, ResourceProps, UpdateOptions, Ctx) of
                                            {ok, _} = Ok ->
                                                ok = m_rsc:restore_uri_aliases(Id,
                                                    maps:get(<<"backup_uri_aliases">>, Props, []), Ctx),
                                                m_rsc_gone:delete(Id, Ctx),
                                                Ok;
                                            Error -> {rollback, Error}
                                        end
                                end
                            end, Context)
                    end
            end
    end.

restore_references(Props, Options, Context) ->
    Cat = target_reference(category_id, Props, Options, Context),
    CG = target_reference(content_group_id, Props, Options, Context),
    case is_integer(Cat) andalso m_rsc:is_a(Cat, category, Context)
        andalso is_integer(CG) andalso (m_rsc:is_a(CG, content_group, Context)
            orelse m_rsc:is_a(CG, acl_collaboration_group, Context))
    of
        false -> {error, missing_reference};
        true ->
            Props1 = Props#{ <<"category_id">> => Cat, <<"content_group_id">> => CG },
            {ok, lists:foldl(fun(Key, Acc) ->
                Ref = m_rsc_gone:followup(maps:get(Key, Props, undefined), Context),
                Acc#{ Key => Ref }
            end, Props1, [<<"creator_id">>, <<"modifier_id">>])}
    end.

target_reference(Key, Props, Options, Context) ->
    case proplists:lookup(Key, Options) of
        none ->
            Id = maps:get(atom_to_binary(Key, utf8), Props, undefined),
            case m_rsc:exists(Id, Context) of
                true -> Id;
                false -> undefined % Missing references always require explicit confirmation.
            end;
        {Key, Id} -> m_rsc:rid(Id, Context)
    end.

%% @doc Internal unfiltered reference labels. Callers must authorize the revision first,
%% or run as the background migration worker.
-spec historical_label(Id, Context) -> map() when
    Id :: integer() | undefined, Context :: z:context().
historical_label(undefined, _Context) -> #{};
historical_label(Id, Context) ->
    Props = case m_rsc:get_raw(Id, Context) of
        {error, enoent} -> latest_props(Id, Context);
        {ok, P} -> P
    end,
    maps:with([<<"name">>, <<"title">>], Props).

%% @doc Internal unfiltered revision properties for migration and authorized reference lookup.
-spec latest_props(Id, Context) -> map() when
    Id :: integer() | undefined, Context :: z:context().
latest_props(Id, Context) ->
    case z_db:q1("select data from backup_revision where rsc_id = $1 order by created desc, id desc limit 1",
                [Id], Context) of
        undefined -> #{};
        Data -> revision_props(Data)
    end.

revision_props(Data) ->
    case binary_to_term(Data) of
        Props when is_map(Props) -> Props;
        Props when is_list(Props) ->
            {ok, Map} = z_props:from_list(Props),
            Map
    end.
