%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc Test if the config files are syntactically ok

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

-module(zotonic_cmd_configtest).
-author("Marc Worrell").

%% API
-export([run/1]).

run(_) ->
    case zotonic_command:get_target_node() of
        {ok, Target} ->
            CfgFiles = zotonic_launcher_config:config_files(Target),
            case zotonic_launcher_config:read_configs(CfgFiles) of
                {ok, _Cfg} ->
                    io:format("ok~n");
                {error, _} = Error ->
                    zotonic_command:format_error(Error)
            end;
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end.
