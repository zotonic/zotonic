%%%-------------------------------------------------------------------
%%% @author fabu
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
%%%
%%% @end
%%% Created : 18. Dec 2017 3:53 PM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_start).
-author("Blaise").

%% API
-export([run/1]).

-include("../../include/zotonic_command.hrl").

run(_) ->
    ZotonicApp = filename:join(?ZOTONIC, "_build/default/lib/zotonic_core/ebin/zotonic_core.app"),
    Target = list_to_atom(?NODENAME ++ "@" ++ ?NODEHOST),
    case filelib:is_file(ZotonicApp) of
        true ->
            io:format("Starting zotonic ~s ...", [Target]),
            net_kernel:start([Target, shortnames]),
            zotonic_launcher_config:load_configs(),
            zotonic:start(),
            receive
                {'EXIT', _, _} ->
                    ok
            end,
            case zotonic:ping() of
                pong ->
                     io:format("OK ~n");
                pang ->
                    io:format("Something went wrong while starting Zotonic. Please check the log files ~s~n",
                        [ filename:join(?ZOTONIC, "priv/log") ])
            end;
        false ->
            io:format("Cannot start Zotonic: first build Zotonic using 'make'. ~n"),
            halt()
    end.

% Build erl command - execute it in the background

% erlang:open_port({spawn, Command}, [])
% erlang:open_port({spawn_executable, Command}, [ {args, [...]}, hide ])  ---- use os:find_executable("erl")
