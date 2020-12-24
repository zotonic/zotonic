%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019-2020 Marc Worrell
%% @doc Print the site configuration

%% Copyright 2019-2020 Marc Worrell
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
-export([info/0, run/1]).

info() ->
    "Show the configuration of a site.".

run([ Site ]) ->
    case zotonic_command:get_target_node() of
        {ok, Target} ->
            SiteName = list_to_atom(Site),
            ConfigFiles = z_sites_config:config_files(Target, SiteName),
            io:format("Site config for ~p at ~p:~n", [ SiteName, Target ]),
            io:format("=====================================~n"),
            case z_sites_config:read_configs(ConfigFiles) of
                {ok, SiteConfig} ->
                    case global_configs(Target) of
                        {ok, GlobalConfig} ->
                            SiteConfig1 = z_sites_config:merge_global_configs(SiteName, SiteConfig, GlobalConfig),
                            CfgList = lists:sort( maps:to_list(SiteConfig1) ),
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
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end;

run(_) ->
    io:format("USAGE: siteconfig <site_name>~n"),
    halt().

global_configs(Target) ->
    ZotonicConfigFiles = zotonic_launcher_config:zotonic_config_files(Target),
    zotonic_launcher_config:read_configs(ZotonicConfigFiles).
