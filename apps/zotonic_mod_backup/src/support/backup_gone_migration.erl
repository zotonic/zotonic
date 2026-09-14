%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Migrate deleted-resource ACL fields and display metadata in bounded batches.
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

-module(backup_gone_migration).
-moduledoc("
Resumable migration of deleted-resource ACL fields and display metadata.
Progress is kept in a separate table which is dropped upon completion.
This internal worker reads revisions without user ACL filtering.").

-export([start/1, resume/1, migrate/2]).

% -include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Store resumable migration progress separately from permanent resource data.
%% A fixed upper bound lets the migration finish while new deletions keep arriving.
%% Called inside the schema transaction; scheduling must wait until it commits.
-spec start(Context) -> ok when Context :: z:context().
start(Context) ->
    [] = z_db:q("create table if not exists backup_gone_migration (
        id boolean primary key default true check (id),
        last_id bigint not null default 0,
        max_id bigint not null
    )", Context),
    z_db:q("insert into backup_gone_migration (id, max_id)
            values (true, coalesce((select max(id) from rsc_gone), 0))
            on conflict (id) do nothing", Context),
    z_db:flush(Context),
    ok.

%% @doc Schedule committed progress after upgrade or on normal site startup.
%% Backup sites retain the cursor until they restart in a normal environment.
-spec resume(Context) -> ok when Context :: z:context().
resume(Context) ->
    case m_site:environment(Context) =/= backup
        andalso z_db:has_connection(Context)
        andalso z_db:table_exists(backup_gone_migration, Context)
    of
        true ->
            case z_pivot_rsc:insert_task(?MODULE, migrate, <<>>, [0], Context) of
                {ok, _} -> ok;
                {error, backup} -> ok
            end;
        false ->
            ok
    end.

%% @doc At most 100 tombstones per task invocation. Progress commits with each row;
%% retries use the saved cursor, including tasks queued by older code versions.
-spec migrate(After, Context) -> ok | {delay, integer(), list()} when
    After :: non_neg_integer(), Context :: z:context().
migrate(_After, Context) ->
    case z_db:table_exists(backup_gone_migration, Context) of
        false -> ok;
        true ->
            {After, Max} = z_db:q_row("select last_id, max_id from backup_gone_migration where id = true", Context),
            Rows = z_db:q("
                    select id
                    from rsc_gone
                    where id > $1
                      and id <= $2
                    order by id
                    limit 100",
                    [After, Max], Context),
            lists:foreach(fun({Id}) ->
                ok = z_db:transaction(fun(Ctx) ->
                    ok = migrate_gone_row(Id, Ctx),
                    z_db:q("update backup_gone_migration set last_id = $1 where id = true", [Id], Ctx),
                    ok
                end, Context)
            end, Rows),
            case Rows of
                [] ->
                    [] = z_db:q("drop table backup_gone_migration", Context),
                    z_db:flush(Context),
                    ok;
                _ -> {delay, 1, [element(1, lists:last(Rows))]}
            end
    end.

migrate_gone_row(Id, Context) ->
    % Alias insertion needs a resource FK lock. Acquire it before the tombstone,
    % matching deletion and restoration, even when the live row is absent.
    LiveId = z_db:q1("select id from rsc where id = $1 for update", [Id], Context),
    case z_db:qmap_row("select * from rsc_gone where id = $1 for update", [Id], Context) of
        {error, enoent} -> ok;
        {ok, Gone} ->
            case LiveId of
                Id ->
                    ok = m_rsc:remember_uri(Id, maps:get(<<"uri">>, Gone, undefined), Context),
                    m_rsc_gone:delete(Id, Context),
                    ok;
                undefined ->
                    ok = migrate_gone_props(Id, Gone, Context),
                    % Do not resurrect a title if the revisions were already pruned.
                    z_db:q("update rsc_gone
                            set props_json = props_json - 'title'
                            where id = $1 and not exists (
                                select 1 from backup_revision where rsc_id = $1
                            )", [Id], Context),
                    ok
            end
    end.

migrate_gone_props(_Id, #{ <<"category_id">> := Cat, <<"content_group_id">> := CG }, _Context)
    when is_integer(Cat), is_integer(CG) -> ok;
migrate_gone_props(Id, Gone, Context) ->
    Props = m_backup_revision:latest_props(Id, Context),
    Labels = maps:from_list([
        {Key, (m_backup_revision:historical_label(maps:get(Key, Props, undefined), Context))#{
            <<"id">> => maps:get(Key, Props, undefined) }}
        || Key <- [<<"category_id">>, <<"content_group_id">>] ]),
    Snapshot = (m_rsc_gone:snapshot(Props, Context))#{
        <<"references">> => Labels,
        % Nearby edge deletions cannot establish ownership at deletion time.
        % Legacy tombstones retain creator-based ownership only.
        <<"author_ids">> => [],
        <<"deleter_id">> => maps:get(<<"modifier_id">>, Gone, undefined) },
    Update = (maps:with([<<"category_id">>, <<"content_group_id">>, <<"visible_for">>,
                        <<"creator_id">>, <<"version">>], Props))#{
        <<"props_json">> => Snapshot },
    {ok, _} = z_db:update(rsc_gone, Id, Update, Context),
    ok.
