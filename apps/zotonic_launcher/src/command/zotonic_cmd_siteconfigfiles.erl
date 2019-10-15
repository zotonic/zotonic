%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc List all configuration files for a site

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

-module(zotonic_cmd_siteconfigfiles).
-author("Blaise").

%% API
-export([run/1]).

run([ Site ]) ->
    case zotonic_command:net_start() of
        ok ->
            SiteName = list_to_atom(Site),
            ConfigFiles = zotonic_command:rpc(z_sites_config, config_files, [ SiteName ]),
            {ok, Target} = zotonic_command:get_target_node(),
            io:format("Site config files for ~p at ~p:~n", [ SiteName, Target ]),
            lists:foreach(
                fun(F) ->
                    io:format("- ~s~n", [F])
                end,
                ConfigFiles);
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end;

run(_) ->
    io:format("USAGE: siteconfigfiles <site_name>~n"),
    halt().
