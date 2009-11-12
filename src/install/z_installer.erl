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
    {ok, C}  = pgsql_pool:get_connection(Name),
    {ok, HasConfig} = pgsql:equery1(C, "
            select count(*) 
            from information_schema.tables 
            where table_catalog = $1 
              and table_name = 'config' 
              and table_type = 'BASE TABLE'", [Database]),
    pgsql_pool:return_connection(Name, C),

    case HasConfig of
        0 ->
            ?LOG("Installing database ~p@~p:~p ~p", [
                        proplists:get_value(dbuser, SiteProps),
                        proplists:get_value(dbhost, SiteProps),
                        proplists:get_value(dbport, SiteProps),
                        Database
                        ]),
            z_install:install(Name);
        1 -> 
            ok
    end.

    