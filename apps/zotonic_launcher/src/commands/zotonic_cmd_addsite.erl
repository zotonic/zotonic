%%%-------------------------------------------------------------------
%%% @author M <tantemelki@gmail.com>
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%
%% @copyright 2017
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%	 http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%%
%% usage zotonic generate-edoc
%%
%%% @end
%%% Created : 18. Aug 2017 10:37 PM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_addsite).
-author("Melki").

%% API
-export([run/1]).

-include("zotonic_escript_helper.hrl").

-define(SKEL, blog).
-define(DBHOST, "127.0.0.1").
-define(DBPORT, 5432).
-define(DBUSER, zotonic).
-define(DBPASSWORD, zotonic).
-define(DBDATABASE, zotonic).
-define(DBSCHEMA, public).
-define(ADMINPASSWORD, admin).

usage() ->
    io:format("Usage: zotonic-addsite [options] <site_name> ~n~n"),
    io:format(" -s <skel>     Skeleton site (one of 'blog', 'basesite', 'empty', 'nodb'; default: ~s~n", [?SKEL]),
    io:format(" -H <host>     Site's hostname (default: <sitename.dev>) ~n"),
    io:format(" -L            Create the site in the current directory and symlink it into ~n"),
    io:format(" -g <remote>   Create a git repository in the site and push it to the given remote ~n~n"),
    io:format(" -h <host>     Database host (default: ~s) ~n", [?DBHOST]),
    io:format(" -p <port>     Database port (default: ~p) ~n", [?DBPORT]),
    io:format(" -u <user>     Database user (default: ~s) ~n", [?DBUSER]),
    io:format(" -P <pass>     Database password (default: ~s) ~n", [?DBPASSWORD]),
    io:format(" -d <name>     Database name (default: ~s) ~n", [?DBDATABASE]),
    io:format(" -n <schema>   Database schema (default: ~s) ~n", [?DBSCHEMA]),
    io:format(" -a <pass>     Admin password (default: ~s) ~n", [?ADMINPASSWORD]).

run([]) ->
    usage();

run(_Arg0) ->
    erlang:error(not_implemented).
