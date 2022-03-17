%%%-------------------------------------------------------------------
%%% @author Blaise
%%% @doc
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
%%% @end
%%% Created : 18. Dec 2017 11:49 AM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_logtail).
-author("Blaise").

%% API
-export([info/0, run/1]).

info() ->
    "Show the last lines of the error and console logs.".

run(["error"]) -> tail("error.log");
run(["crash"]) -> tail("crash.log");
run(_) -> tail("console.log").

tail(File) ->
    case zotonic_command:net_start() of
        ok ->
            case zotonic_command:get_target_node() of
                {ok, Target} ->
                    _ = zotonic_launcher_app:load_configs(Target),
                    LogDir = z_config:get(log_dir),
                    LogFile = filename:join([ LogDir, File ]),
                    Cmd = "tail -n 500 \"" ++ z_filelib:os_escape(LogFile) ++ "\"",
                    Res = os:cmd(Cmd),
                    io:format("~ts~n", [ Res ]);
                {error, _} = Error ->
                    zotonic_command:format_error(Error)
            end;
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end.
