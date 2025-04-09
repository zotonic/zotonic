%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012-2025 Marc Worrell
%% @doc Manage a resource's revisions, manages a table of edge changes
% and medium records.
%% @end

%% Copyright 2012-2025 Marc Worrell
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

-behaviour(zotonic_model).

-export([
    m_get/3,

    revert_resource/3,

    list_deleted/2,

    revision_title/2,
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

    periodic_cleanup/1,

    install/1,

    insert_deleted_revisions/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(BACKUP_TYPE_PROPS, $P).
-define(BACKUP_TYPE_PROPS_DELETED, $D).


%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"list">>, Id | Rest ], _Msg, Context) ->
    Id1 = m_rsc:rid(Id, Context),
    Revs = case z_acl:is_allowed(use, mod_backup, Context) orelse m_rsc:is_editable(Id1, Context) of
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
            case z_acl:is_allowed(use, mod_backup, Context) of
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
%% @todo Also revert deletion of all referred dependent resources (if the page was deleted).
revert_resource(Id, RevId, Context) ->
    case get_revision(RevId, Context) of
        {ok, #{
            <<"rsc_id">> := RscId,
            <<"data">> := Props,
            <<"created">> := Created
        }} when RscId =:= Id ->
            Exists = m_rsc:exists(Id, Context),
            case m_rsc_update:update(Id, Props, [ is_import ], Context) of
                {ok, NewId} ->
                    if
                        Exists -> ok;
                        true -> m_rsc_gone:delete(NewId, Context)
                    end,
                    _MaybeMissingDependent = revert_edges(Id, Exists, Created, Context),
                    revert_medium(Id, Created, Context),
                    z_depcache:flush(Id, Context),
                    ok;
                {error, _} = Error ->
                    Error
            end;
        {ok, _} ->
            {error, invalid};
        {error, _} ->
            {error, enoent}
    end.

%% @doc Replay all edges in reverse. This creates a view of all edges at a certain
%% moment in time. If the resource was deleted, then we also recover all referring edges.
revert_edges(Id, Exists, Created, Context) ->
    Objects = z_db:q("
        select subject_id, predicate, object_id, is_insert
        from backup_edge_log
        where subject_id = $1
          and timestamp >= $2
        order by id desc",
        [Id, Created],
        Context),
    Subjects = if
        Exists ->
            [];
        true ->
            z_db:q("
                select subject_id, predicate, object_id, is_insert
                from backup_edge_log
                where object_id = $1
                  and timestamp >= $2
                order by id desc",
                [Id, Created],
                Context)
    end,
    Edges = Objects ++ Subjects,
    % Incoming edges are only recovered if the resource did not exist.
    % Outgoing edges are always recovered.
    Recover = lists:foldl(
        fun
            ({SubjectId, Predicate, ObjectId, false}, Acc) when not Exists; Id =:= SubjectId ->
                Acc#{
                    {SubjectId, Predicate, ObjectId} => insert
                };
            ({SubjectId, Predicate, ObjectId, true}, Acc) when not Exists; Id =:= SubjectId ->
                Acc#{
                    {SubjectId, Predicate, ObjectId} => delete
                };
            (_E, Acc) ->
                Acc
        end,
        #{},
        Edges),
    maps:fold(
        fun
            ({SubjectId, Predicate, ObjectId} = E, insert, Acc) ->
                case m_rsc:exists(SubjectId, Context)
                    andalso m_rsc:exists(ObjectId, Context)
                of
                    true ->
                        m_edge:insert(SubjectId, Predicate, ObjectId, Context),
                        Acc;
                    false when Exists, SubjectId =:= Id ->
                        % Outgoing edge to missing resource.
                        [ E | Acc ];
                    false ->
                        Acc
                end;
            ({SubjectId, Predicate, ObjectId}, delete, Acc) ->
                m_edge:delete(SubjectId, Predicate, ObjectId, Context),
                Acc
        end,
        [],
        Recover).

%% @doc Revert the medium record that best matches the date of the revered resource.
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
        [ Id, Created ],
        Context)
    of
        {ok, #{
            <<"id">> := _RevId,
            <<"props">> := Medium,
            <<"medium_deleted">> := DeletedDate
        }} ->
            case m_media:get(Id, Context) of
                Medium when DeletedDate =:= undefined ->
                    % Still current medium record
                    ok;
                undefined ->
                    % No medium record - re-insert the revision
                    revert_medium_1(Medium, Context);
                _Medium ->
                    % Current one is different - delete it and
                    % re-insert the revision
                    m_media:delete(Id, Context),
                    revert_medium_1(Medium, Context)
            end;
        {error, _} = Error ->
            Error
    end.

revert_medium_1(Medium, Context) ->
    m_media:recover_medium(Medium, Context).


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
                Props when is_map(Props) -> maps:get(<<"title">>, Props);
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
    {ok, Rs} = z_db:qmap("
        select b.*
        from backup_revision b
            left join rsc r
            on r.id = b.rsc_id
        where b.type = $1
          and r.id is null
        order by b.created desc
        offset $2
        limit $3",
        [ ?BACKUP_TYPE_PROPS_DELETED, Offset-1, Limit ],
        Context),
    Total = z_db:q1("
        select count(*)
        from backup_revision b
            left join rsc r
            on r.id = b.rsc_id
        where b.type = $1
          and r.id is null",
        [ ?BACKUP_TYPE_PROPS_DELETED ],
        Context),
    Rs1 = lists:map(fun expand/1, Rs),
    #search_result{
        result = Rs1,
        total = Total,
        is_total_estimated = false
    }.

expand(#{
        <<"data_type">> := <<"erlang">>,
        <<"data">> := Data
    } = R) ->
    R#{
        <<"data_type">> => <<"term">>,
        <<"data">> => erlang:binary_to_term(Data)
    };
expand(R) ->
    R.


save_deleted(_Id, undefined, _Context) ->
    ok;
save_deleted(Id, Props, Context) when is_integer(Id), is_map(Props) ->
    save_revision(Id, Props, true, Context).

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
                    erlang:term_to_binary(Props, [compressed])
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
    RevisionId :: integer(),
    Context :: z:context(),
    Revision :: map(),
    Reason :: enoent | term().
get_revision(RevId0, Context) ->
    RevId = z_convert:to_integer(RevId0),
    case z_db:qmap_row("select * from backup_revision where id = $1", [RevId], Context) of
        {ok, #{ <<"data">> := Data } = Row} ->
            R1 = Row#{ <<"data">> => erlang:binary_to_term(Data) },
            {ok, R1};
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
    Months = backup_config:retention_months(Context),
    Threshold = z_datetime:prev_month(calendar:universal_time(), Months),
    z_db:q("
        delete from backup_revision
        where created < $1",
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
    z_db:q("
        DELETE FROM backup_revision
        WHERE created < $1
        AND rsc_id IN (SELECT rsc_id FROM identity WHERE type = any($2))",
        [UserRevThreshold, IdentityTypes1],
        Context),

    % Join with 'rsc_gone' to find user resources that have been deleted
    UserDelDays = backup_config:deleted_user_retention_days(Context),
    UserDelThreshold = z_datetime:prev_day(calendar:universal_time(), UserDelDays),
    z_db:q("
        DELETE FROM backup_revision
        WHERE rsc_id IN (
            SELECT id FROM rsc_gone
            WHERE is_personal_data = true
            AND modified < $1
        )",
        [UserDelThreshold],
        Context),
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
                    z_pivot_rsc:insert_task(?MODULE, insert_deleted_revisions, <<>>, Context),
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
    end.

insert_deleted_revisions(Context) ->
    ?LOG_INFO(#{
        in => zotonic_mod_backup,
        text => <<"Inserting deleted revisions for resources">>
    }),
    Rs = z_db:q("
        select rsc_gone.id, rsc_gone.modified, max(rev.id)
        from rsc_gone
        join backup_revision rev
            on rsc_gone.id = rev.rsc_id
        where rev.type = $1
        group by rsc_gone.id
        ",
        [ ?BACKUP_TYPE_PROPS ],
        Context),
    lists:foreach(
        fun({RscId, GoneDate, RevId}) ->
            case z_db:q1("
                select id
                from backup_revision
                where rsc_id = $1
                  and type = $2",
                [ RscId, ?BACKUP_TYPE_PROPS_DELETED ],
                Context)
            of
                undefined ->
                    {ok, R} = z_db:qmap_row("
                        select *
                        from backup_revision
                        where id = $1",
                        [ RevId ],
                        Context),
                    #{
                        <<"version">> := Version,
                        <<"user_id">> := UserId,
                        <<"user_name">> := Username,
                        <<"data">> := Data
                    } = R,
                    1 = z_db:q("
                        insert into backup_revision
                            (rsc_id, type, version, user_id, user_name, data_type, data, created)
                        values ($1, $2, $3, $4, $5, $6, $7, $8)
                        ", [
                            RscId,
                            ?BACKUP_TYPE_PROPS_DELETED,
                            Version,
                            UserId,
                            Username,
                            <<"erlang">>,
                            Data,
                            GoneDate
                        ],
                        Context);
                _DelRevId ->
                    ok
            end
        end,
        Rs).
