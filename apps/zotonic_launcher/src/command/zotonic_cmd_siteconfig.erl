%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc Print the site configuration

%% Copyright 2019 Marc Worrell
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

-module(zotonic_cmd_siteconfig).
-author("Blaise").

%% API
-export([run/1]).

run([ Site ]) ->
    case zotonic_command:get_target_node() of
        {ok, Target} ->
            SiteName = list_to_atom(Site),
            ConfigFiles = z_sites_config:config_files(Target, SiteName),
            io:format("Site config for ~p at ~p:~n", [ SiteName, Target ]),
            io:format("=====================================~n"),
            case z_sites_config:read_configs(ConfigFiles) of
                {ok, Map} ->
                    CfgList = lists:sort( maps:to_list(Map) ),
                    lists:foreach(
                        fun({Key, Value}) ->
                            io:format("~n~p: ", [ Key ]),
                            zotonic_cmd_config:pretty_print_value(Key, Value)
                        end,
                        CfgList),
                    io:format("~n~n");
                {error, _} = Error ->
                    zotonic_command:format_error(Error)
            end;
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end;

run(_) ->
    io:format("USAGE: siteconfig <site_name>~n"),
    halt().
