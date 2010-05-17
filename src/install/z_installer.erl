%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-17
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
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Install zotonic on the databases in the PoolOpts, skips when already installed.
start_link(SiteProps) when is_list(SiteProps) ->
    install_check(SiteProps),
    ignore.

install_check(SiteProps) ->
    % Check if the config table exists, if so then assume that all is ok
    Name     = proplists:get_value(host, SiteProps),
    Database = proplists:get_value(dbdatabase, SiteProps),
    Schema   = proplists:get_value(dbschema, SiteProps, "public"),

    z_install:pre_install(Name, SiteProps),
	
    case has_table("config", Name, Database, Schema) of
        false ->
            ?LOG("Installing database ~p@~p:~p ~p", [
                        proplists:get_value(dbuser, SiteProps),
                        proplists:get_value(dbhost, SiteProps),
                        proplists:get_value(dbport, SiteProps),
                        Database
                        ]),
            z_install:install(Name);
        true -> 
			ok = upgrade(Name, Database, Schema),
			sanity_check(Name, Database, Schema)
    end.


%% Check if a table exists by querying the information schema.
has_table(Table, Name, Database, Schema) ->
    {ok, C}  = pgsql_pool:get_connection(Name),
    {ok, HasTable} = pgsql:equery1(C, "
            select count(*) 
            from information_schema.tables 
            where table_catalog = $1 
              and table_name = $3 
              and table_schema = $2
              and table_type = 'BASE TABLE'", [Database, Schema, Table]),
	pgsql_pool:return_connection(Name, C),
	HasTable == 1.

%% Check if a column in a table exists by querying the information schema.
has_column(Table, Column, Name, Database, Schema) ->
    {ok, C}  = pgsql_pool:get_connection(Name),
    {ok, HasColumn} = pgsql:equery1(C, "
            select count(*) 
            from information_schema.columns 
            where table_catalog = $1 
              and table_schema = $2
              and table_name = $3 
              and column_name = $4", [Database, Schema, Table, Column]),
	pgsql_pool:return_connection(Name, C),
	HasColumn == 1.


%% Upgrade older Zotonic versions.
upgrade(Name, Database, Schema) ->
    ok = install_acl(Name, Database, Schema),
    ok = install_identity_verified(Name, Database, Schema),
    ok.


install_acl(Name, Database, Schema) ->
	%% Remove group, rsc_group, group_id
	HasRscGroup = has_table("rsc_group", Name, Database, Schema),
	HasGroup = has_table("group", Name, Database, Schema),
	case HasRscGroup andalso HasGroup of
		true ->
		    {ok, C}  = pgsql_pool:get_connection(Name),
			{ok, [], []} = pgsql:squery(C, "BEGIN"),
			pgsql:squery(C, "alter table rsc drop column group_id cascade"),
			pgsql:squery(C, "drop table rsc_group cascade"),
			pgsql:squery(C, "drop table \"group\" cascade"),
			pgsql:squery(C, "delete from module where name='mod_admin_group'"),
			{ok, 1} = pgsql:equery(C, "insert into module (name, is_active) values ($1, true)", ["mod_acl_adminonly"]),
			{ok, [], []} = pgsql:squery(C, "COMMIT"),
        	pgsql_pool:return_connection(Name, C),
			ok;
		false ->
			ok
	end.

install_identity_verified(Name, Database, Schema) ->
    case has_column("identity", "is_verified", Name, Database, Schema) of
        true -> 
            ok;
        false ->
            {ok, C}  = pgsql_pool:get_connection(Name),
			{ok, [], []} = pgsql:squery(C, "BEGIN"),
			A = pgsql:squery(C, "alter table identity "
			                "add column is_verified boolean not null default false, "
			                "add column verify_key character varying(32), "
			                "add constraint identity_verify_key_unique UNIQUE (verify_key)"),
			?DEBUG(A),
			pgsql:squery(C, "update identity set is_verified = true where key = 'username_pw'"),
			{ok, [], []} = pgsql:squery(C, "COMMIT"),
        	pgsql_pool:return_connection(Name, C),
			ok
    end.
    

% Perform some simple sanity checks
sanity_check(Name, _Database, _Schema) ->
    {ok, C}  = pgsql_pool:get_connection(Name),
	{ok, [], []} = pgsql:squery(C, "BEGIN"),
    ensure_module_active(C, "mod_authentication"),
	{ok, [], []} = pgsql:squery(C, "COMMIT"),
	pgsql_pool:return_connection(Name, C),
    ok.
	


ensure_module_active(C, Module) ->
    case pgsql:equery(C, "select is_active from module where name = $1", [Module]) of
        {ok, _, [{true}]} ->
            ok;
        {ok, _, [{false}]} ->
            {ok, 1} = pgsql:equery(C, "update module set is_active = 1 where name = $1", [Module]);
        _ ->
    		{ok, 1} = pgsql:equery(C, "insert into module (name, is_active) values ($1, true)", [Module])
    end.
