%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012-2023 Marc Worrell
%% @doc Manage a resource's revisions.
%% @end

%% Copyright 2012-2023 Marc Worrell
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

    list_deleted/2,

    save_deleted/3,
    save_revision/3,
    get_revision/2,
    list_revisions/2,
    list_revisions_assoc/2,

    periodic_cleanup/1,
    retention_months/1,
    user_retention_days/1,
    deleted_user_retention_days/1,

    install/1,

    insert_deleted_revisions/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(BACKUP_TYPE_PROPS, $P).
-define(BACKUP_TYPE_PROPS_DELETED, $D).

% Number of months we keep revisions
-define(BACKUP_REVISION_RETENTION_MONTHS, 18).
% Number of days we keep user revisions
-define(BACKUP_USER_REVISION_RETENTION_DAYS, 90).
% Number of days we keep user deletions
-define(BACKUP_USER_DELETION_RETENTION_DAYS, 30).


%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"list">>, Id | Rest ], _Msg, Context) ->
    Id1 = m_rsc:rid(Id, Context),
    Revs = case m_rsc:is_editable(Id1, Context) of
        true -> list_revisions_assoc(Id1, Context);
        false -> []
    end,
    {ok, {Revs, Rest}};
m_get([ <<"retention_months">> | Rest ], _Msg, Context) ->
    {ok, {retention_months(Context), Rest}};
m_get([ <<"user_retention_days">> | Rest ], _Msg, Context) ->
    {ok, {user_retention_days(Context), Rest}};
m_get([ <<"deleted_user_retention_days">> | Rest ], _Msg, Context) ->
    {ok, {deleted_user_retention_days(Context), Rest}};
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.


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
    case Version of
        LastVersion when LastVersion =/= undefined andalso not IsDeleted ->
            ok;
        _ ->
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


get_revision(RevId0, Context) ->
    RevId = z_convert:to_integer(RevId0),
    case z_db:assoc_row("select * from backup_revision where id = $1", [RevId], Context) of
        undefined ->
            {error, notfound};
        Row ->
            R1 = proplists:delete(data, Row),
            {ok, [ {data, erlang:binary_to_term(proplists:get_value(data, Row)) } | R1 ]}
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
    Months = retention_months(Context),
    Threshold = z_datetime:prev_month(calendar:universal_time(), Months),
    z_db:q("
        delete from backup_revision
        where created < $1",
        [Threshold],
        Context
    ),

    % Join with the 'identity' table to find revisions of user resources
    UserRevDays = user_retention_days(Context),
    UserRevThreshold = z_datetime:prev_day(calendar:universal_time(), UserRevDays),
    % see 'm_identity:is_user/2':
    IdentityTypes = m_identity:user_types(Context),
    IdentityTypes1 = [ z_convert:to_binary(Idn) || Idn <- lists:usort(IdentityTypes) ],
    z_db:q("
        DELETE FROM backup_revision
        WHERE created < $1
        AND rsc_id IN (SELECT rsc_id FROM identity WHERE type = any($2))",
        [UserRevThreshold, IdentityTypes1],
        Context
    ),

    % Join with 'rsc_gone' to find user resources that have been deleted
    UserDelDays = deleted_user_retention_days(Context),
    UserDelThreshold = z_datetime:prev_day(calendar:universal_time(), UserDelDays),
    z_db:q("
        DELETE FROM backup_revision
        WHERE rsc_id IN (
            SELECT id FROM rsc_gone
            WHERE is_personal_data = true
            AND modified < $1
        )",
        [UserDelThreshold],
        Context
    ),
    ok.

%% @doc Return the number of months we keep revisions. Defaults to 18 months.
%% Uses the configuration mod_backup.revision_retention_months
-spec retention_months(Context) -> Months when
    Context :: z:context(),
    Months :: pos_integer().
retention_months(Context) ->
    case z_convert:to_integer(m_config:get_value(mod_backup, revision_retention_months, Context)) of
        undefined ->
            ?BACKUP_REVISION_RETENTION_MONTHS;
        N ->
            max(N, 1)
    end.

%% @doc Return the number of days we keep user resource's revisions.
%% Defaults to 3 months (90 days).
%% Uses the configuration mod_backup.user_revision_retention_days
-spec user_retention_days(Context) -> Days when
    Context :: z:context(),
    Days :: pos_integer().
user_retention_days(Context) ->
    case z_convert:to_integer(m_config:get_value(mod_backup, user_revision_retention_days, Context)) of
        undefined ->
            ?BACKUP_USER_REVISION_RETENTION_DAYS;
        N ->
            max(N, 1)
    end.

%% @doc Return the number of days we keep user resource's revisions of deleted users.
%% Defaults to 1 month (30 days), which is also the maximum allowed.
%% Uses the configuration mod_backup.user_deletion_retention_days
-spec deleted_user_retention_days(Context) -> Days when
    Context :: z:context(),
    Days :: pos_integer().
deleted_user_retention_days(Context) ->
    case z_convert:to_integer(m_config:get_value(mod_backup, user_deletion_retention_days, Context)) of
        undefined ->
            ?BACKUP_USER_DELETION_RETENTION_DAYS;
        N when N =< 0 ->
            ?BACKUP_USER_DELETION_RETENTION_DAYS;
        N ->
            min(30, N)
    end.

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
