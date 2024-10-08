%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell
%% @doc This server will install the database when started. It will always return ignore to the supervisor.
%% This server should be started after the database pool but before any database queries will be done.
%% @end

%% Copyright 2009-2024 Marc Worrell
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

-module(z_install_update).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_server).

%% gen_server exports
-export([
    start_link/1, init/1,
    handle_call/3, handle_cast/2,
    handle_info/2, code_change/3,
    terminate/2]).


-record(state, { site :: atom(), site_props :: list() }).

-include_lib("kernel/include/logger.hrl").


%%====================================================================
%% API
%%====================================================================

-spec start_link(list()) -> ignore | {error, database|term()}.
%% @doc Install zotonic on the databases in the PoolOpts, skips when already installed.
start_link(SiteProps) when is_list(SiteProps) ->
    gen_server:start_link(?MODULE, SiteProps, []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init(SiteProps) ->
    {site, Site} = proplists:lookup(site, SiteProps),
    self() ! install_check,
    {ok, #state{ site = Site, site_props = SiteProps }}.

handle_call(Msg, _From, State) ->
    {reply, {unknown_call, Msg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(install_check, State) ->
    case install_check(State#state.site_props) of
        ok ->
            ok = z_site_sup:install_done(State#state.site),
            {noreply, State, hibernate};
        {error, Reason} ->
            ?LOG_EMERGENCY(#{
                in => zotonic_core,
                text => <<"Site install check failed">>,
                result => error,
                reason => Reason
            }),
            {stop, installfail, State}
    end.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


%%====================================================================
%% Internal
%%====================================================================

%% Check if the config table exists, if so then assume that all is ok
-spec install_check( proplists:proplist() ) -> ok | {error, nodbinstall | database | term()}.
install_check(SiteProps) ->
    {site, Site} = proplists:lookup(site, SiteProps),
    logger:set_process_metadata(#{
        site => Site,
        module => ?MODULE
    }),
    Context = z_context:new(Site),
    case z_db:has_connection(Context) of
        true ->
            maybe_drop_db(Context),
            check_db_and_upgrade(Context, 1);
        false ->
            ok
    end.

-spec check_db_and_upgrade( z:context(), integer() ) -> ok | {error, nodbinstall | database | term()}.
check_db_and_upgrade(Context, Tries) when Tries =< 2 ->
    case z_db_pool:test_connection(Context) of
        ok ->
            DbOptions = proplists:delete(dbpassword, z_db_pool:get_database_options(Context)),
            case {z_db:table_exists(config, Context), z_config:get(dbinstall)} of
                {false, false} ->
                    ?LOG_ERROR("config table does not exist and dbinstall is false; not installing"),
                    {error, nodbinstall};
                {false, _} ->
                    %% Install database
                    ?LOG_NOTICE(#{
                        text => <<"Installing database with db">>,
                        in => zotonic_core,
                        options => DbOptions
                    }),
                    z_install:install(Context),
                    ok;
                {true, _} ->
                    %% Normal startup, do upgrade / check
                    ok = z_db:transaction(
                            fun(Context1) ->
                                C = z_db_pgsql:get_raw_connection(Context1),
                                Database = proplists:get_value(dbdatabase, DbOptions),
                                Schema = proplists:get_value(dbschema, DbOptions),
                                ok = upgrade(C, Database, Schema),
                                ok = upgrade_models(Context),
                                ok = sanity_check(C, Database, Schema)
                            end,
                            Context),
                    ok
            end;
        {error, nodatabase} = Error ->
            % No database configured, this is ok, proceed as normal (without db)
            ?LOG_ERROR(#{
                text => <<"Database connection failure: no database configured">>,
                in => zotonic_core,
                result => error,
                reason => nodatabase,
                site => z_context:site(Context)
            }),
            Error;
        {error, econnrefused} = Error ->
            ?LOG_ERROR(#{
                text => <<"Database connection failure: connection refused">>,
                in => zotonic_core,
                site => z_context:site(Context),
                result => error,
                reason => econnrefused
            }),
            Error;
        {error, Reason} ->
            ?LOG_WARNING(#{
                text => "Database connection failure",
                in => zotonic_core,
                result => error,
                reason => Reason,
                site => z_context:site(Context)
            }),
            case z_config:get(dbcreate) of
                false ->
                    ?LOG_ERROR(#{
                        text => <<"Database does not exist and dbcreate is false; not creating">>,
                        in => zotonic_core,
                        result => error,
                        reason => nodbcreate,
                        site => z_context:site(Context)
                    }),
                    {error, nodbcreate};
                _Else ->
                    case z_db:prepare_database(Context) of
                        ok ->
                            ?LOG_NOTICE(#{
                                text => <<"Retrying install check after db creation.">>,
                                in => zotonic_core,
                                site => z_context:site(Context)
                            }),
                            check_db_and_upgrade(Context, Tries+1);
                        {error, PrepReason} = Error ->
                            ?LOG_ERROR(#{
                                text => <<"Could not create the database and schema.">>,
                                in => zotonic_core,
                                site => z_context:site(Context),
                                result => error,
                                reason => PrepReason
                            }),
                            Error
                    end
                end
    end;
check_db_and_upgrade(_Context, _Tries) ->
    ?LOG_ERROR("Could not connect to database and db creation failed"),
    {error, database}.

maybe_drop_db(Context) ->
    DbOptions = z_db_pool:database_options(z_context:site(Context), m_site:all(Context)),
    case proplists:get_value(dbdropschema, DbOptions, false) of
        true ->
            case z_db_pool:test_connection(Context) of
                ok ->
                    ?LOG_WARNING(#{
                        text => <<"Dropping existing schema because of 'dbdropschema' is set.">>,
                        in => zotonic_core,
                        schema => proplists:get_value(dbschema, DbOptions),
                        database => proplists:get_value(dbdatabase, DbOptions),
                        site => z_context:site(Context)
                    }),
                    ok = z_db:drop_schema(Context),
                    ok;
                {error, _} ->
                    ok
            end;
        false ->
            ok
    end.

has_table(C, Table, Database, Schema) ->
    {ok, _, [{HasTable}]} = epgsql:equery(C, "
            select count(*)
            from information_schema.tables
            where table_catalog = $1
              and table_name = $3
              and table_schema = $2
              and table_type = 'BASE TABLE'", [Database, Schema, Table]),
    HasTable =:= 1.


%% Check if a column in a table exists by querying the information schema.
has_column(C, Table, Column, Database, Schema) ->
    {ok, _, [{HasColumn}]} = epgsql:equery(C, "
            select count(*)
            from information_schema.columns
            where table_catalog = $1
              and table_schema = $2
              and table_name = $3
              and column_name = $4", [Database, Schema, Table, Column]),
    HasColumn =:= 1.

get_column_type(C, Table, Column, Database, Schema) ->
    {ok, _, [{ColumnType}]} = epgsql:equery(C, "
            select data_type
            from information_schema.columns
            where table_catalog = $1
              and table_schema = $2
              and table_name = $3
              and column_name = $4", [Database, Schema, Table, Column]),
    ColumnType.

is_column_nullable(C, Table, Column, Database, Schema) ->
    {ok, _, [{IsNullable}]} = epgsql:equery(C, "
            select is_nullable
            from information_schema.columns
            where table_catalog = $1
              and table_schema = $2
              and table_name = $3
              and column_name = $4", [Database, Schema, Table, Column]),
    case IsNullable of
        <<"YES">> -> true;
        <<"NO">> -> false
    end.

%% Check if a constraint in a table exists by querying the information schema.
has_constraint(C, Table, Constraint, Database, Schema) ->
    {ok, _, [{HasConstraint}]} = epgsql:equery(C, "
            select count(*)
            from information_schema.table_constraints
            where constraint_catalog = $1
              and constraint_schema = $2
              and table_name = $3
              and constraint_name = $4", [Database, Schema, Table, Constraint]),
    HasConstraint >= 1.


%% Upgrade older Zotonic versions.
upgrade(C, Database, Schema) ->
    % Ancient versions - this should be cleaned up.
    ok = install_acl(C, Database, Schema),
    ok = install_identity_is_verified(C, Database, Schema),
    ok = install_identity_verify_key(C, Database, Schema),
    ok = drop_visitor(C, Database, Schema),
    ok = extent_mime(C, Database, Schema),
    ok = install_task_due(C, Database, Schema),
    ok = install_module_schema_version(C, Database, Schema),
    ok = install_geocode(C, Database, Schema),
    ok = install_rsc_gone(C, Database, Schema),
    ok = install_rsc_page_path_log(C, Database, Schema),
    ok = upgrade_config_schema(C, Database, Schema),
    % 0.10.x
    ok = install_medium_log(C, Database, Schema),
    ok = install_pivot_location(C, Database, Schema),
    % 0.12.x
    ok = install_edge_log(C, Database, Schema),
    ok = fix_timestamptz(C, Database, Schema),
    % 0.12.5
    ok = install_content_group_dependent(C, Database, Schema),
    ok = convert_category_hierarchy(C, Database, Schema),

    % 0.22.0
    ok = add_edge_log_details(C, Database, Schema),

    % 0.82
    ok = check_category_id_key(C, Database, Schema),

    % 1.0
    ok = set_default_visible_for(C, Database, Schema),
    ok = drop_persist(C, Database, Schema),
    ok = publication_start_nullable(C, Database, Schema),
    ok = key_changes_v1_0(C, Database, Schema),
    ok = rsc_language(C, Database, Schema),
    ok = task_queue_error_count(C, Database, Schema),
    ok = identity_expires(C, Database, Schema),
    ok = rsc_unfindable(C, Database, Schema),
    ok = rsc_pivot_log(C, Database, Schema),
    ok = medium_size_bigint(C, Database, Schema),
    ok = media_frame_count(C, Database, Schema),
    ok = identity_log(C, Database, Schema),
    ok.


-spec upgrade_models( z:context() ) -> ok.
upgrade_models(Context) ->
    ok = m_rsc_gone:install(Context),
    ok = m_rsc_import:install(Context).


upgrade_config_schema(C, Database, Schema) ->
    case get_column_type(C, "config", "value", Database, Schema) of
        <<"text">> ->
            ok;
        _ ->
            {ok,[],[]} = epgsql:squery(C, "alter table config alter column value type text"),
            ok
    end.


install_acl(C, Database, Schema) ->
    %% Remove group, rsc_group, group_id
    HasRscGroup = has_table(C, "rsc_group", Database, Schema),
    HasGroup = has_table(C, "group", Database, Schema),
    case HasRscGroup andalso HasGroup of
        true ->
            epgsql:squery(C, "alter table rsc drop column group_id cascade"),
            epgsql:squery(C, "drop table rsc_group cascade"),
            epgsql:squery(C, "drop table \"group\" cascade"),
            epgsql:squery(C, "delete from module where name='mod_admin_group'"),
            {ok, 1} = epgsql:equery(C, "insert into module (name, is_active) values ($1, true)", ["mod_acl_adminonly"]),
            ok;
        false ->
            ok
    end.


drop_persist(C, Database, Schema) ->
    case has_table(C, "persistent", Database, Schema) of
        true ->
            case has_table(C, "comment", Database, Schema) of
                true ->
                    {ok, _, _} = epgsql:squery(C, "alter table comment drop constraint if exists fk_comment_persistent_id"),
                    {ok, _, _} = epgsql:squery(C, "alter index if exists fki_comment_persistent_id rename to comment_persistent_id_key"),
                    ok;
                false ->
                    ok
            end,
            {ok, _, _} = epgsql:squery(C, "drop table persistent"),
            ok;
        false ->
            ok
    end.

install_rsc_page_path_log(C, Database, Schema) ->
    case has_table(C, "rsc_page_path_log", Database, Schema) of
        false ->
            {ok, [], []} = epgsql:squery(C, z_install:rsc_page_path_log()),
            epgsql:squery(C, z_install:rsc_page_path_log_fki()),
            ok;
        true ->
            case epgsql:equery(C,
                             "select count(*)
                              from information_schema.referential_constraints
                              where constraint_catalog = $1
                                and constraint_schema = $2
                                and constraint_name = 'rsc_page_path_log_fkey'",
                             [Database, Schema])
            of
                {ok, [_], [{1}]} ->
                    {ok, [], []} = epgsql:squery(C, "ALTER TABLE rsc_page_path_log "
                                        "DROP CONSTRAINT rsc_page_path_log_fkey, "
                                        "ADD CONSTRAINT fk_rsc_page_path_log_id FOREIGN KEY (id) "
                                        "    REFERENCES rsc(id)"
                                        "    ON UPDATE CASCADE ON DELETE CASCADE"),
                    epgsql:squery(C, z_install:rsc_page_path_log_fki()),
                    ok;
                {ok, [_], [{0}]} ->
                    ok
            end
    end.


drop_visitor(C, Database, Schema) ->
    case has_table(C, "visitor_cookie", Database, Schema) of
        true ->
            epgsql:squery(C, "drop table visitor_cookie cascade"),
            epgsql:squery(C, "drop table visitor cascade"),
            ok;
        false ->
            ok
    end.


extent_mime(C, Database, Schema) ->
    {ok, _, [{Length}]} = epgsql:equery(C, "
            select character_maximum_length
                                       from information_schema.columns
                                       where table_catalog = $1
                                       and table_schema = $2
                                       and table_name = $3
                                       and column_name = $4", [Database, Schema, "medium", "mime"]),
    case Length < 128 of
        true ->
            {ok, [], []} = epgsql:squery(C, "alter table medium alter column mime type character varying(128)");
        false ->
            nop
    end,
    ok.


install_identity_is_verified(C, Database, Schema) ->
    case has_column(C, "identity", "is_verified", Database, Schema) of
        true ->
            ok;
        false ->
            {ok, [], []} = epgsql:squery(C, "alter table identity "
                                        "add column is_verified boolean not null default false"),
            {ok, [], []} = epgsql:squery(C, "update identity set is_verified = true where key = 'username_pw'"),
            ok
    end.

install_identity_verify_key(C, Database, Schema) ->
    case has_column(C, "identity", "verify_key", Database, Schema) of
        true ->
            ok;
        false ->
            {ok, [], []} = epgsql:squery(C, "alter table identity "
                                        "add column verify_key character varying(32), "
                                        "add constraint identity_verify_key_unique UNIQUE (verify_key)"),
            ok
    end.


install_task_due(C, Database, Schema) ->
    case has_column(C, "pivot_task_queue", "due", Database, Schema) of
        true ->
            ok;
        false ->
            {ok, [], []} = epgsql:squery(C, "alter table pivot_task_queue add column due timestamp with time zone"),
            ok
    end.


install_module_schema_version(C, Database, Schema) ->
    case has_column(C, "module", "schema_version", Database, Schema) of
        true ->
            ok;
        false ->
            {ok, [], []} = epgsql:squery(C, "alter table module add column schema_version int "),
            Predefined = ["mod_twitter", "mod_mailinglist", "mod_menu", "mod_survey", "mod_acl_simple_roles", "mod_contact"],
            [
             {ok, _} = epgsql:equery(C, "UPDATE module SET schema_version=1 WHERE name=$1 AND is_active=true", [M]) || M <- Predefined
            ],
            ok
    end.

%% make sure the geocode is a bigint (psql doesn't have unsigned bigint)
install_geocode(C, Database, Schema) ->
    case get_column_type(C, "rsc", "pivot_geocode", Database, Schema) of
        <<"character varying">> ->
            {ok, [], []} = epgsql:squery(C, "alter table rsc drop column pivot_geocode"),
            {ok, [], []} = epgsql:squery(C, "alter table rsc add column pivot_geocode bigint,"
                                        " add column pivot_geocode_qhash bytea"),
            {ok, [], []} = epgsql:squery(C, "CREATE INDEX rsc_pivot_geocode_key ON rsc (pivot_geocode)"),
            ok;
        <<"bigint">> ->
            %% 0.9dev was missing a column definition in the z_install.erl
            case has_column(C, "rsc", "pivot_geocode_qhash", Database, Schema) of
                true ->
                    ok;
                false ->
                    {ok, [], []} = epgsql:squery(C, "alter table rsc add column pivot_geocode_qhash bytea"),
                    ok
            end
    end.

%% Install the table tracking deleted (or moved) resources
install_rsc_gone(C, Database, Schema) ->
    case has_table(C, "rsc_gone", Database, Schema) of
        false ->
            install_rsc_gone_1(C);
        true ->
            case has_column(C, "rsc_gone", "new_id", Database, Schema) of
                false ->
                    _ = epgsql:squery(C, "DROP TABLE rsc_gone"),
                    install_rsc_gone_1(C);
                true ->
                    ok
            end
    end.

install_rsc_gone_1(C) ->
    {ok,[],[]} = epgsql:squery(C, "create table rsc_gone ( "
                              "  id bigint not null,"
                              "  new_id bigint,"
                              "  new_uri character varying(250),"
                              "  version int not null, "
                              "  uri character varying(250),"
                              "  name character varying(80),"
                              "  page_path character varying(80),"
                              "  is_authoritative boolean NOT NULL DEFAULT true,"
                              "  creator_id bigint,"
                              "  modifier_id bigint,"
                              "  created timestamp with time zone NOT NULL DEFAULT now(),"
                              "  modified timestamp with time zone NOT NULL DEFAULT now(),"
                              "  CONSTRAINT rsc_gone_pkey PRIMARY KEY (id)"
                              ")"),
    {ok, [], []} = epgsql:squery(C, "CREATE INDEX rsc_gone_name ON rsc_gone(name)"),
    {ok, [], []} = epgsql:squery(C, "CREATE INDEX rsc_gone_page_path ON rsc_gone(page_path)"),
    {ok, [], []} = epgsql:squery(C, "CREATE INDEX rsc_gone_modified ON rsc_gone(modified)"),
    ok.

%% Table with all uploaded filenames, used to ensure unique filenames in the upload archive
install_medium_log(C, Database, Schema) ->
    case has_table(C, "medium_log", Database, Schema) of
        false ->
            {ok,[],[]} = epgsql:squery(C, z_install:medium_log_table()),
            {ok,[],[]} = epgsql:squery(C, z_install:medium_update_function()),
            {ok,[],[]} = epgsql:squery(C, z_install:medium_update_trigger()),
            {ok, _} = epgsql:squery(C,
                                   "
                                insert into medium_log (usr_id, filename, created)
                                   select r.creator_id, m.filename, m.created
                                   from medium m join rsc r on r.id = m.id
                                   where m.filename is not null
                                   and m.filename <> ''
                                   and m.is_deletable_file
                                   "),
            {ok, _} = epgsql:squery(C,
                                   "
                                insert into medium_log (usr_id, filename, created)
                                   select r.creator_id, m.preview_filename, m.created
                                   from medium m join rsc r on r.id = m.id
                                   where m.preview_filename is not null
                                   and m.preview_filename <> ''
                                   and m.is_deletable_preview
                                   "),
            ok;
        true ->
            ok
    end.


install_pivot_location(C, Database, Schema) ->
    Added = lists:foldl(fun(Col, Acc) ->
                          case has_column(C, "rsc", Col, Database, Schema) of
                              true ->
                                  Acc;
                              false ->
                                  {ok, [], []} = epgsql:squery(C, "alter table rsc add column " ++ Col ++ " float"),
                                  true
                          end
                        end,
                        false,
                        ["pivot_location_lat", "pivot_location_lng"]),
    case Added of
        true ->
            {ok, [], []} = epgsql:squery(C, "CREATE INDEX rsc_pivot_location_key ON rsc (pivot_location_lat, pivot_location_lng)"),
            ok;
        false ->
            ok
    end.



%% Log all edge changes. This log is polled to issue edge-change notifications.
install_edge_log(C, Database, Schema) ->
    case has_table(C, "edge_log", Database, Schema) of
        false ->
            {ok,[],[]} = epgsql:squery(C, z_install:edge_log_table()),
            {ok,[],[]} = epgsql:squery(C, z_install:edge_log_function()),
            {ok,[],[]} = epgsql:squery(C, z_install:edge_log_trigger()),
            ok;
         true ->
            ok
    end.


%% Perform some simple sanity checks
sanity_check(C, _Database, _Schema) ->
    ensure_module_active(C, "mod_authentication"),
    ok.

ensure_module_active(C, Module) ->
    case epgsql:equery(C, "select is_active from module where name = $1", [Module]) of
        {ok, _, [{true}]} ->
            ok;
        {ok, _, [{false}]} ->
            {ok, 1} = epgsql:equery(C, "update module set is_active = true where name = $1", [Module]);
        _ ->
            {ok, 1} = epgsql:equery(C, "insert into module (name, is_active) values ($1, true)", [Module])
    end.

%% Ensure that all timestamp columns have a time zone
fix_timestamptz(C, Database, Schema) ->
    [   fix_timestamptz_column(C, Table, Column, Database, Schema)
        || {Table, Column} <- get_timestamp_without_timezone_columns(C, Database, Schema)
    ],
    ok.

fix_timestamptz_column(C, Table, Col, Database, Schema) ->
    ?LOG_NOTICE(#{
        text => <<"Adding time zone to column">>,
        in => zotonic_core,
        database => Database,
        schema => Schema,
        table => Table,
        column => Col
    }),
    {ok, [], []} = epgsql:squery(C, "alter table \""++binary_to_list(Table)++"\" alter column \""++binary_to_list(Col)++"\" type timestamp with time zone"),
    ok.

get_timestamp_without_timezone_columns(C, Database, Schema) ->
    {ok, _, Cols} = epgsql:equery(C, "
                                   select table_name, column_name
                                   from information_schema.columns
                                   where table_catalog = $1
                                     and table_schema = $2
                                     and data_type = 'timestamp without time zone'",
                                [Database, Schema]),
    Cols.


%% 0.12.5: Add content groups for the content- and user-group based ACL modules
install_content_group_dependent(C, Database, Schema) ->
    case has_column(C, "rsc", "content_group_id", Database, Schema) of
        true ->
            ok;
        false ->
            ?LOG_NOTICE(#{
                text => <<"Adding rsc.is_dependent and rsc.content_group_id">>,
                in => zotonic_core,
                database => Database,
                schema => Schema
            }),
            {ok, [], []} = epgsql:squery(C,
                              "ALTER TABLE rsc "
                              "ADD COLUMN is_dependent BOOLEAN NOT NULL DEFAULT false,"
                              "ADD COLUMN content_group_id INT,"
                              "ADD CONSTRAINT fk_rsc_content_group_id FOREIGN KEY (content_group_id) "
                              "    REFERENCES rsc(id)"
                              "    ON UPDATE CASCADE ON DELETE SET NULL"),
            {ok, [], []} = epgsql:squery(C, "CREATE INDEX fki_rsc_content_group_id ON rsc (content_group_id)"),
            ok
    end.

convert_category_hierarchy(C, Database, Schema) ->
    case has_table(C, "hierarchy", Database, Schema) of
        false ->
            {ok, [], []} = epgsql:squery(C, z_install:hierarchy_table()),
            {ok, [], []} = epgsql:squery(C, z_install:hierarchy_index_1()),
            {ok, [], []} = epgsql:squery(C, z_install:hierarchy_index_2()),

            {ok, _} = epgsql:squery(C, "
                insert into hierarchy
                    (name, id, parent_id, nr, lvl, lft, rght)
                select '$category', id, parent_id, nr, lvl, lft, rght
                from category
              "),
            _ = epgsql:squery(C, "drop table category cascade"),
            ok;
        true ->
            ok
    end.

add_edge_log_details(C, Database, Schema) ->
    {ok, [], []} = epgsql:squery(C, z_install:edge_log_function()),
    case has_column(C, "edge_log", "logged", Database, Schema) of
        true ->
            ok;
        false ->
            {ok, [], []} = epgsql:squery(C, "drop table edge_log"),
            {ok, [], []} = epgsql:squery(C, z_install:edge_log_table()),
            ok
    end.

set_default_visible_for(C, Database, Schema) ->
    {ok, _, [{Default}]} = epgsql:equery(C, "
            select column_default
            from information_schema.columns
            where table_catalog = $1
              and table_schema = $2
              and table_name = 'rsc'
              and column_name = 'visible_for'
            ", [Database, Schema]),
    case Default of
        <<"1">> ->
            {ok, [], []} = epgsql:squery(C,
                  "ALTER TABLE rsc "
                  "ALTER COLUMN visible_for SET DEFAULT 0"),
            ok;
        <<"0">> ->
            ok
    end.

publication_start_nullable(C, Database, Schema) ->
    case is_column_nullable(C, "rsc", "publication_start", Database, Schema) of
        true -> ok;
        false ->
            {ok, [], []} = epgsql:squery(
                C,
                "ALTER TABLE rsc "
                    "ALTER COLUMN publication_start DROP NOT NULL, "
                    "ALTER COLUMN publication_start DROP DEFAULT"
            ),
            ok
    end.


key_changes_v1_0(C, Database, Schema) ->
    % Identity table:

    % - changed 'is_unique' flag to not null default false
    % - added prefix key on type+key for 'like' queries
    case is_column_nullable(C, "identity", "is_unique", Database, Schema) of
        false ->
            ok;
        true ->
            ?LOG_NOTICE(#{
                text => <<"Upgrade: changing is_unique on identity table">>,
                in => zotonic_core,
                database => Database,
                schema => Schema,
                table => identity
            }),
            {ok, [], []} = epgsql:squery(C, "
                ALTER TABLE identity
                DROP CONSTRAINT identity_type_key_unique"),
            {ok, _} = epgsql:squery(C, "
                UPDATE identity
                SET is_unique = false
                WHERE is_unique IS NULL
                "),
            {ok, [], []} = epgsql:squery(C, "
                ALTER TABLE identity
                    ALTER COLUMN is_unique SET NOT NULL,
                    ALTER COLUMN is_unique SET DEFAULT false
            "),
            {ok, [], []} = epgsql:squery(C, "
                CREATE UNIQUE INDEX identity_type_key_unique
                ON identity (type, key) WHERE (is_unique)
            "),
            {ok, [], []} = epgsql:squery(C, "
                DROP INDEX IF EXISTS identity_type_key_key"),
            {ok, [], []} = epgsql:squery(C, "
                CREATE INDEX identity_type_key_key
                ON identity using btree (type, key collate ucs_basic text_pattern_ops)
            ")
    end,

    % Rsc table

    % Drop keys
    {ok, [], []} = epgsql:squery(C, "DROP INDEX IF EXISTS fki_rsc_created"),
    {ok, [], []} = epgsql:squery(C, "DROP INDEX IF EXISTS rsc_pivot_gender_key"),
    {ok, [], []} = epgsql:squery(C, "DROP INDEX IF EXISTS rsc_pivot_date_start_key"),
    {ok, [], []} = epgsql:squery(C, "DROP INDEX IF EXISTS rsc_pivot_date_end_key"),

    % New keys
    case has_constraint(C, "rsc", "fk_rsc_category_id", Database, Schema) of
        true ->
            ok;
        false ->
            ?LOG_NOTICE(#{
                text => <<"Upgrade: adding indices to rsc table">>,
                in => zotonic_core,
                database => Database,
                schema => Schema,
                table => rsc
            }),
            {ok, [], []} = epgsql:squery(C, "
                ALTER TABLE rsc ADD CONSTRAINT fk_rsc_category_id
                    FOREIGN KEY (category_id) REFERENCES rsc (id)
                    ON UPDATE CASCADE ON DELETE RESTRICT
            "),

            {ok, [], []} = epgsql:squery(C, "CREATE INDEX IF NOT EXISTS fki_rsc_category_id ON rsc (category_id)"),
            {ok, [], []} = epgsql:squery(C, "CREATE INDEX IF NOT EXISTS rsc_modified_category_nr_key ON rsc (modified, pivot_category_nr)"),
            {ok, [], []} = epgsql:squery(C, "CREATE INDEX IF NOT EXISTS rsc_created_category_nr_key ON rsc (created, pivot_category_nr)"),
            {ok, [], []} = epgsql:squery(C, "CREATE INDEX IF NOT EXISTS rsc_pivot_date_start_category_nr_key ON rsc (pivot_date_start, pivot_category_nr)"),
            {ok, [], []} = epgsql:squery(C, "CREATE INDEX IF NOT EXISTS rsc_pivot_date_end_category_nr_key ON rsc (pivot_date_end, pivot_category_nr)"),
            {ok, [], []} = epgsql:squery(C, "CREATE INDEX IF NOT EXISTS rsc_publication_start_category_nr_key ON rsc (publication_start, pivot_category_nr)"),
            {ok, [], []} = epgsql:squery(C, "CREATE INDEX IF NOT EXISTS rsc_publication_end_category_nr_key ON rsc (publication_end, pivot_category_nr)")
    end,
    ok.


rsc_language(C, Database, Schema) ->
    case has_column(C, "rsc", "language", Database, Schema) of
        true ->
            ok;
        false ->
            ?LOG_NOTICE(#{
                text => <<"Upgrade: adding language column to rsc table">>,
                in => zotonic_core,
                database => Database,
                schema => Schema,
                table => rsc
            }),
            {ok, [], []} = epgsql:squery(C, "alter table rsc "
                                        "add column language character varying(16)[] not null default '{}'"),
            {ok, [], []} = epgsql:squery(C, "CREATE INDEX rsc_language_key ON rsc USING gin(language)"),

            {ok, _} = epgsql:equery(C, "
                            insert into pivot_task_queue (module, function, key)
                            values ('z_install_update_task', 'init_language', '')
                            ",
                            []),
            ok
    end.

task_queue_error_count(C, Database, Schema) ->
    case has_column(C, "pivot_task_queue", "error_count", Database, Schema) of
        true ->
            ok;
        false ->
            ?LOG_NOTICE(#{
                text => <<"Upgrade: adding error_count column pivot_task_queue">>,
                in => zotonic_core,
                database => Database,
                schema => Schema,
                table => pivot_task_queue
            }),
            {ok, [], []} = epgsql:squery(C, "alter table pivot_task_queue "
                                        "add column error_count integer not null default 0"),


            {ok, _, Tasks} = epgsql:equery(C, "
                    select id, props
                    from pivot_task_queue
                    ", []),

            lists:foreach(
                fun({TaskId, Props}) ->
                    case task_error_ct(z_db_pgsql:decode_value(Props)) of
                        N when is_integer(N), N > 0 ->
                            {ok, _} = epgsql:equery(C, "
                                            update pivot_task_queue
                                            set error_count = $1
                                            where id = $2
                                            ",
                                            [ N, TaskId ]);
                        _ ->
                            ok
                    end
                end,
                Tasks)
    end.

task_error_ct(#{ <<"error_ct">> := Count }) ->
    Count;
task_error_ct(Props) when is_list(Props) ->
    % deprecated task queue entries
    proplists:get_value(error_ct, Props, 0);
task_error_ct(_) ->
    0.


identity_expires(C, Database, Schema) ->
    case has_column(C, "identity", "expires", Database, Schema) of
        true ->
            ok;
        false ->
            ?LOG_NOTICE(#{
                text => <<"Upgrade: adding expires column to identity">>,
                in => zotonic_core,
                database => Database,
                schema => Schema,
                table => identity
            }),
            {ok, [], []} = epgsql:squery(C,
                                    "alter table identity "
                                    "add column expires timestamp with time zone"),
            {ok, [], []} = epgsql:squery(C,
                                    "CREATE INDEX identity_expires_type_key ON identity (expires, type)"),

            {ok, _} = epgsql:squery(C, "
                            update identity
                            set expires = created,
                                prop1 = ''
                            where prop1 = 'expired'
                              and type = 'username_pw'
                            "),
            ok
    end.


rsc_unfindable(C, Database, Schema) ->
    case has_column(C, "rsc", "is_unfindable", Database, Schema) of
        true ->
            ok;
        false ->
            ?LOG_NOTICE(#{
                text => <<"Upgrade: adding is_unfindable column to rsc">>,
                in => zotonic_core,
                database => Database,
                schema => Schema,
                table => rsc
            }),
            {ok, [], []} = epgsql:squery(C,
                                    "alter table rsc "
                                    "add column is_unfindable boolean not null default false"),
            {ok, [], []} = epgsql:squery(C,
                                    "CREATE INDEX rsc_is_unfindable_key ON rsc (is_unfindable)"),
            ok
    end.


media_frame_count(C, Database, Schema) ->
    case has_column(C, "medium", "frame_count", Database, Schema) of
        true ->
            ok;
        false ->
            ?LOG_NOTICE(#{
                text => <<"Upgrade: adding frame_count column to medium">>,
                in => zotonic_core,
                database => Database,
                schema => Schema,
                table => rsc
            }),
            {ok, [], []} = epgsql:squery(C,
                                    "alter table medium "
                                    "add column frame_count int"),
            ok
    end.

rsc_pivot_log(C, Database, Schema) ->
    case has_table(C, "rsc_pivot_log", Database, Schema) of
        false ->
            {ok,[],[]} = epgsql:squery(C, z_install:rsc_pivot_log_table()),
            {ok,[],[]} = epgsql:squery(C, z_install:rsc_pivot_log_index_1()),
            {ok,[],[]} = epgsql:squery(C, z_install:rsc_pivot_log_index_2()),
            {ok,[],[]} = epgsql:squery(C, z_install:rsc_pivot_log_function()),
            {ok,[],[]} = epgsql:squery(C, z_install:rsc_pivot_log_trigger()),

            {ok, _} = epgsql:squery(C, "
                            insert into rsc_pivot_log (rsc_id, due, is_update)
                            select rsc_id, coalesce(due, now()), is_update
                            from rsc_pivot_queue
                            "),
            epgsql:squery(C, "drop trigger if exists rsc_update_queue_trigger on rsc cascade"),
            epgsql:squery(C, "drop function if exists rsc_pivot_update"),
            epgsql:squery(C, "drop table if exists rsc_pivot_queue cascade"),
            ok;
        true ->
            ok
    end.

identity_log(C, Database, Schema) ->
    case has_table(C, "identity_log", Database, Schema) of
        false ->
            SqlList = z_install:identity_log_table(),
            lists:foreach(
                fun(Sql) ->
                    {ok, [], []} = epgsql:squery(C, Sql)
                end,
                SqlList);
        true ->
            ok
    end.

check_category_id_key(C, _Database, _Schema) ->
    {ok, [], []} = epgsql:squery(
        C,
        "CREATE INDEX IF NOT EXISTS fki_rsc_category_id ON rsc (category_id)"
    ),
    ok.

medium_size_bigint(C, Database, Schema) ->
    case get_column_type(C, "medium", "size", Database, Schema) of
        <<"bigint">> ->
            ok;
        _ ->
            {ok,[],[]} = epgsql:squery(C, "alter table medium alter column size type bigint"),
            ok
    end.

