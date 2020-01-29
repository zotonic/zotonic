%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020 Marc Worrell
%% @doc Test if we can connect to the configured database.

%% Copyright 2020 Marc Worrell
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

-module(zotonic_cmd_connectdb).
-author("Marc Worrell").

%% API
-export([run/1]).

run(_) ->
    case zotonic_command:get_target_node() of
        {ok, Target} ->
            CfgFiles = zotonic_launcher_config:config_files(Target),
            case zotonic_launcher_config:read_configs(CfgFiles) of
                {ok, #{ zotonic := Cfg }} ->
                    try_connect(Cfg);
                {ok, _} ->
                    try_connect(#{});
                {error, _} = Error ->
                    zotonic_command:format_error(Error)
            end;
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end.

try_connect(Cfg) ->
    Options = [
        {dbhost, get_value(dbhost, Cfg, "localhost")},
        {dbport, get_value(dbport, Cfg, 5432)},
        {dbpassword, get_value(dbpassword, Cfg, "zotonic")},
        {dbuser, get_value(dbuser, Cfg, "zotonic")},
        {dbdatabase, get_value(dbdatabase, Cfg, "zotonic")},
        {dbschema, get_value(dbschema, Cfg, "public")},
        {dbdriver, get_value(dbdriver, Cfg, z_db_pool:db_driver_default())}
    ],
    show_options(Options),
    DbDriver = proplists:get_value(dbdriver, Options),
    DbDriver:ensure_all_started(),
    V = DbDriver:test_connection(Options),
    show_result(V, Options).

show_options(Options) ->
    io:format("Trying to connect using options:~n~n"),
    lists:foreach(
        fun({K, V}) ->
            io:format("  ~p: ~p~n", [ K, V ])
        end,
        Options),
    io:format("~n").

show_result(ok, _Options) ->
    io:format("ok~n");
show_result({error, noschema}, Options) ->
    io:format(standard_error, "Error: connect ok but schema ~p does not exist.~n", [ proplists:get_value(dbschema, Options) ]),
    halt(1);
show_result(Error, _Options) ->
    zotonic_command:format_error(Error).

get_value(K, Cfg, Default) ->
    case maps:get(K, Cfg, Default) of
        <<>> -> Default;
        "" -> Default;
        undefined -> Default;
        V -> V
    end.
