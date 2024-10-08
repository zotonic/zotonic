%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%% @doc Manage a resource's revisions.

%% Copyright 2012 Marc Worrell
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

-behaviour(gen_model).

-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,

    save_revision/3,
    get_revision/2,
    list_revisions/2,
    list_revisions_assoc/2,

    periodic_cleanup/1,
    retention_months/1,
    user_retention_days/1,
    deleted_user_retention_days/1,

    install/1
]).

-include("zotonic.hrl").

-define(BACKUP_TYPE_PROPS, $P).


% Number of months we keep revisions
-define(BACKUP_REVISION_RETENTION_MONTHS, 18).
% Number of days we keep user revisions
-define(BACKUP_USER_REVISION_RETENTION_DAYS, 90).
% Number of days we keep user deletions
-define(BACKUP_USER_DELETION_RETENTION_DAYS, 30).


m_find_value(list, #m{value=undefined} = M, _Context) ->
    M#m{value=list};
m_find_value(Id, #m{value=list}, Context) when is_integer(Id) ->
    case m_rsc:is_editable(Id, Context) of
        true ->
            list_revisions_assoc(Id, Context);
        false ->
            []
    end;
m_find_value(retention_months, _M, Context) ->
    retention_months(Context);
m_find_value(user_retention_days, _M, Context) ->
    user_retention_days(Context);
m_find_value(deleted_user_retention_days, _M, Context) ->
    deleted_user_retention_days(Context);
m_find_value(_X, #m{}, _Context) ->
    undefined.

m_to_list(_m, _Context) ->
    [].

m_value(_m, _Context) ->
    undefined.



save_revision(Id, Props, Context) ->
    Version = proplists:get_value(version, Props),
    LastVersion = z_db:q1("select version from backup_revision where rsc_id = $1 order by created desc limit 1", [Id], Context),
    case Version of
        LastVersion when LastVersion =/= undefined ->
            ok;
        _ ->
            UserId = z_acl:user(Context),
            1 = z_db:q("
                insert into backup_revision
                    (rsc_id, type, version, user_id, user_name, data_type, data)
                values ($1, $2, $3, $4, $5, $6, $7)
                ", [
                    Id,
                    ?BACKUP_TYPE_PROPS,
                    proplists:get_value(version, Props),
                    UserId,
                    z_string:truncate(
                        z_trans:lookup_fallback(
                            m_rsc:p_no_acl(UserId, title, Context),
                            Context),
                        60),
                    "erlang",
                    erlang:term_to_binary(Props, [compressed])
                ],
                Context),
            ok
    end.


get_revision(RevId, Context) ->
    case z_db:assoc_row("select * from backup_revision where id = $1", [RevId], Context) of
        undefined ->
            {error, notfound};
        Row ->
            R1 = proplists:delete(data, Row),
            {ok, [ {data, erlang:binary_to_term(proplists:get_value(data, Row)) } | R1 ]}
    end.

list_revisions(Id, Context) ->
    z_db:q("
        select id, type, created, version, user_id, user_name
        from backup_revision
        where rsc_id = $1
        order by created desc", [Id], Context).

list_revisions_assoc(Id, Context) ->
    z_db:assoc("
        select id, type, created, version, user_id, user_name
        from backup_revision
        where rsc_id = $1
        order by created desc", [Id], Context).


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
    z_db:q("
        DELETE FROM backup_revision
        WHERE created < $1
        AND rsc_id IN (SELECT rsc_id FROM identity WHERE type in ('username_pw', 'openid'))",
        [UserRevThreshold],
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
            ok;
        true ->
            ok
    end.
