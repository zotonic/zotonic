%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2016 Arjan Scherpenisse
%%
%% @doc Run site-specific tests in an isolated database schema

%% Copyright 2016 Arjan Scherpenisse <arjan@miraclethings.nl>
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

-module(z_sitetest).
-author("Arjan Scherpenisse <arjan@miraclethings.nl>").

-export([run/1, run/2]).

-include_lib("zotonic/include/zotonic.hrl").
-include_lib("epgsql/include/pgsql.hrl").

%% @doc Run all *_sitetest.erl tests for the given site.
run(Site) when is_atom(Site) ->
    run(Site, find_sitetest_modules(Site)).

%% @doc Run the given _sitetest eunit modules for the given site.
run(Site, Modules) when is_atom(Site), is_list(Modules) ->
    %% Stop the site
    ok = maybe_stop_site(Site, z_sites_manager:get_site_status(Site)),
    timer:sleep(500),

    %% Override the site config to set the test schema
    ok = configure_test_schema(Site),

    %% And make sure its not there yet
    ok = ensure_drop_test_schema(Site),

    %% Now start the site and wait for it
    ok = start_site(Site),

    %% Run the tests
    Result = eunit:test(Modules, [verbose]),

    %% Start the site with the regular schema again
    z_sites_manager:put_site_config_overrides(Site, []),
    z_sites_manager:restart(Site),

    Result.


maybe_stop_site(Site, {ok, running}) ->
    ok = z_sites_manager:stop(Site);
maybe_stop_site(_, {ok, stopped}) ->
    ok.


%% @doc Configure the site config override to set the test schema.
configure_test_schema(Site) ->
    Schema = "z_sitetest",
    z_sites_manager:put_site_config_overrides(Site, [{dbschema, Schema}]).

%% @doc Drop the site's datbase schema
ensure_drop_test_schema(Site) ->
    {ok, Config} = z_sites_manager:get_site_config(Site),
    Database = proplists:get_value(dbdatabase, Config),
    Schema = proplists:get_value(dbschema, Config),
    {ok, Conn} = open_connection(Database, Config),
    ok = drop_schema(Site, Conn, Schema),
    close_connection(Conn).




%% @doc Drop a schema
-spec drop_schema(atom(), pgsql:connection(), string()) -> ok | {error, term()}.
drop_schema(Site, Connection, Schema) ->
    case pgsql:equery(
           Connection, 
           "DROP SCHEMA \"" ++ Schema ++ "\" CASCADE"
          ) of  
        {ok, _, _} ->
            ok;
        {error, {error, error, <<"3F000">>, _, _}} ->
            ok;
        {error, Reason} = Error ->
            lager:error("[~p] z_sitetest: error while dropping schema ~p: ~p", [Site, Schema, Reason]),
            Error
    end.    


open_connection(DatabaseName, Options) ->
    pgsql:connect(
      proplists:get_value(dbsite, Options),
      proplists:get_value(dbuser, Options),
      proplists:get_value(dbpassword, Options),
      [
       {port, proplists:get_value(dbport, Options)},
       {database, DatabaseName}
      ]
     ).

close_connection(Connection) ->
    pgsql:close(Connection).


%% @doc Start the site, and wait for it to be fully booted.
start_site(Site) ->
    ok = z_sites_manager:start(Site),
    timer:sleep(100), %% Sleep is needed to ensure the translation server has started
    Context = z_context:new(Site),
    ok = z_sites_manager:await_startup(Context).


%% @doc Filter the list of beam files to find all sitetest modules
%% given a certain site atom.
find_sitetest_modules(Site) when is_atom(Site) ->
    SiteStr = z_convert:to_list(Site),
    lists:flatten(
      [
       [ z_convert:to_atom(filename:basename(File, ".beam"))
         || File <- filelib:wildcard(Path ++ "/" ++ SiteStr ++ "_*_sitetest.beam")
       ]
       || Path <- code:get_path()]).
