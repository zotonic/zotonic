%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-17
%%
%% @doc This server will install the database when started. It will always return ignore to the supervisor.
%% This server should be started after the database pool but before any database queries will be done.

%% Copyright 2009 Marc Worrell
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

-module(z_installer).
-author("Marc Worrell <marc@worrell.nl").

%% gen_server exports
-export([start_link/1]).

-include_lib("zotonic.hrl").

%%====================================================================
%% API
%%====================================================================
%% @spec start_link(SiteProps) -> {ok,Pid} | ignore | {error,Error}
%% @doc Install zotonic on the databases in the PoolOpts, skips when already installed.
start_link(SiteProps) when is_list(SiteProps) ->
    install_check(SiteProps),
    ignore.

install_check(SiteProps) ->
    %% Check if the config table exists, if so then assume that all is ok
    Name     = proplists:get_value(host, SiteProps),
    Database = case proplists:get_value(dbdatabase, SiteProps, inherit) of
                   inherit ->
                       pgsql_pool:get_database_opt(database, Name);
                   Db ->
                       Db
               end,

    case Database of
        none ->
            ignore;
        _ ->
            {ok, Schema} = pgsql_pool:get_database_opt(schema, Name),
            {ok, C} = pgsql_pool:get_connection(Name),
            {ok, [], []} = pgsql:squery(C, "BEGIN"),

            ok = z_install:pre_install(Name, SiteProps),

            case has_table(C, "config", Database, Schema) of
                false ->
                    {ok, [], []} = pgsql:squery(C, "COMMIT"),
                    pgsql_pool:return_connection(Name, C),

                    {ok, User} = pgsql_pool:get_database_opt(username, Name),
                    {ok, Host} = pgsql_pool:get_database_opt(host, Name),
                    {ok, Port} = pgsql_pool:get_database_opt(port, Name),
                    lager:warning("~p: Installing database ~s.~s at ~s@~s:~s", 
                                  [
                                   Name,
                                   z_convert:to_list(Database),
                                   z_convert:to_list(Schema),
                                   z_convert:to_list(User),
                                   z_convert:to_list(Host),
                                   z_convert:to_list(Port)
                                  ]),
                    z_install:install(Name);
                true -> 
                    ok = upgrade(C, Database, Schema),
                    sanity_check(C, Database, Schema),

                    {ok, [], []} = pgsql:squery(C, "COMMIT"),
                    pgsql_pool:return_connection(Name, C)
            end
    end.

has_table(C, Table, Database, Schema) ->    
    {ok, HasTable} = pgsql:equery1(C, "
            select count(*) 
            from information_schema.tables 
            where table_catalog = $1 
              and table_name = $3 
              and table_schema = $2
              and table_type = 'BASE TABLE'", [Database, Schema, Table]),
    HasTable =:= 1.


%% Check if a column in a table exists by querying the information schema.
has_column(C, Table, Column, Database, Schema) ->
    {ok, HasColumn} = pgsql:equery1(C, "
            select count(*) 
            from information_schema.columns 
            where table_catalog = $1 
              and table_schema = $2
              and table_name = $3 
              and column_name = $4", [Database, Schema, Table, Column]),
    HasColumn =:= 1.

get_column_type(C, Table, Column, Database, Schema) ->
    {ok, ColumnType} = pgsql:equery1(C, "
            select data_type
            from information_schema.columns 
            where table_catalog = $1 
              and table_schema = $2
              and table_name = $3 
              and column_name = $4", [Database, Schema, Table, Column]),
    ColumnType.
    

%% Upgrade older Zotonic versions.
upgrade(C, Database, Schema) ->
    ok = install_acl(C, Database, Schema),
    ok = install_identity_is_verified(C, Database, Schema),
    ok = install_identity_verify_key(C, Database, Schema),
    ok = install_persist(C, Database, Schema),
    ok = drop_visitor(C, Database, Schema),
    ok = extent_mime(C, Database, Schema),
    ok = install_task_due(C, Database, Schema),
    ok = install_module_schema_version(C, Database, Schema),
    ok = install_geocode(C, Database, Schema),
    ok.


install_acl(C, Database, Schema) ->
    %% Remove group, rsc_group, group_id
    HasRscGroup = has_table(C, "rsc_group", Database, Schema),
    HasGroup = has_table(C, "group", Database, Schema),
    case HasRscGroup andalso HasGroup of
        true ->
            pgsql:squery(C, "alter table rsc drop column group_id cascade"),
            pgsql:squery(C, "drop table rsc_group cascade"),
            pgsql:squery(C, "drop table \"group\" cascade"),
            pgsql:squery(C, "delete from module where name='mod_admin_group'"),
            {ok, 1} = pgsql:equery(C, "insert into module (name, is_active) values ($1, true)", ["mod_acl_adminonly"]),
            ok;
        false ->
            ok
    end.


install_persist(C, Database, Schema) ->
    case has_table(C, "persistent", Database, Schema) of
        false ->
            {ok,[],[]} = pgsql:squery(C, "create table persistent ( "
                            "  id character varying(32) not null,"
                            "  props bytea,"
                            "  created timestamp with time zone NOT NULL DEFAULT now(),"
                            "  modified timestamp with time zone NOT NULL DEFAULT now(),"
                            "  CONSTRAINT persistent_pkey PRIMARY KEY (id)"
                            ")"),
            ok;
        true ->
            ok
    end.


drop_visitor(C, Database, Schema) ->
    case has_table(C, "visitor_cookie", Database, Schema) of
        true ->
            {ok, _N} = pgsql:squery(C, 
                    "insert into persistent (id,props) "
                    "select c.cookie, v.props from visitor_cookie c join visitor v on c.visitor_id = v.id"),
            pgsql:squery(C, "drop table visitor_cookie cascade"),
            pgsql:squery(C, "drop table visitor cascade"),
            ok;
        false ->
            ok
    end.


extent_mime(C, Database, Schema) ->
    {ok, Length} = pgsql:equery1(C, "
            select character_maximum_length 
            from information_schema.columns 
            where table_catalog = $1 
              and table_schema = $2
              and table_name = $3 
              and column_name = $4", [Database, Schema, "medium", "mime"]),
    case Length < 128 of
        true ->
            {ok, [], []} = pgsql:squery(C, "alter table medium alter column mime type character varying(128)");
        false ->
            nop
    end,
    ok.
    

install_identity_is_verified(C, Database, Schema) ->
    case has_column(C, "identity", "is_verified", Database, Schema) of
        true -> 
            ok;
        false ->
            {ok, [], []} = pgsql:squery(C, "alter table identity "
                                           "add column is_verified boolean not null default false"),
            {ok, [], []} = pgsql:squery(C, "update identity set is_verified = true where key = 'username_pw'"),
            ok
    end.

install_identity_verify_key(C, Database, Schema) ->
    case has_column(C, "identity", "verify_key", Database, Schema) of
        true -> 
            ok;
        false ->
            {ok, [], []} = pgsql:squery(C, "alter table identity "
                                           "add column verify_key character varying(32), "
                                           "add constraint identity_verify_key_unique UNIQUE (verify_key)"),
            ok
    end.


install_task_due(C, Database, Schema) ->
    case has_column(C, "pivot_task_queue", "due", Database, Schema) of
        true -> 
            ok;
        false ->
            {ok, [], []} = pgsql:squery(C, "alter table pivot_task_queue add column due timestamp "),
            ok
    end.


install_module_schema_version(C, Database, Schema) ->
    case has_column(C, "module", "schema_version", Database, Schema) of
        true -> 
            ok;
        false ->
            {ok, [], []} = pgsql:squery(C, "alter table module add column schema_version int "),
            Predefined = ["mod_twitter", "mod_mailinglist", "mod_menu", "mod_survey", "mod_acl_simple_roles", "mod_contact"],
            [
                {ok, _} = pgsql:equery(C, "UPDATE module SET schema_version=1 WHERE name=$1 AND is_active=true", [M]) || M <- Predefined
            ],
            ok
    end.

%% make sure the geocode is an unsigned bigint
install_geocode(C, Database, Schema) ->
    case get_column_type(C, "rsc", "pivot_geocode", Database, Schema) of
        <<"character varying">> ->
            {ok, [], []} = pgsql:squery(C, "alter table rsc drop column pivot_geocode"),
            {ok, [], []} = pgsql:squery(C, "alter table rsc add column pivot_geocode bigint,"
                                                          " add column pivot_geocode_qhash bytea"),
            {ok, [], []} = pgsql:squery(C, "CREATE INDEX rsc_pivot_geocode_key ON rsc (pivot_geocode)"),
            ok;
        <<"bigint">> ->
            ok
    end.



% Perform some simple sanity checks
sanity_check(C, _Database, _Schema) ->
    ensure_module_active(C, "mod_authentication"),
    ok.

ensure_module_active(C, Module) ->
    case pgsql:equery(C, "select is_active from module where name = $1", [Module]) of
        {ok, _, [{true}]} ->
            ok;
        {ok, _, [{false}]} ->
            {ok, 1} = pgsql:equery(C, "update module set is_active = true where name = $1", [Module]);
        _ ->
            {ok, 1} = pgsql:equery(C, "insert into module (name, is_active) values ($1, true)", [Module])
    end.
