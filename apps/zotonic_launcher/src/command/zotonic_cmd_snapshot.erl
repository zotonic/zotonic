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
%%%
%%% @end
%%% Created : 18. Dec 2017 3:47 PM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_snapshot).
-author("Blaise").

%% API
-export([run/1]).

-include("zotonic_command.hrl").

run([]) ->
    io:format("USAGE: snapshot [site_nmae] ~n"),
    io:format("USAGE: See ZotonicCommand.txt ~n~n");

run(Site) ->
    os:cmd("mkdir -p " ++ Site ++ "/" ++ Site ++ "/files/snapshot pg_dump zotonic_"
        ++ Site ++ "\> " ++ Site ++ "/" ++ Site ++ "/files/snapshot/zotonic_"
        ++ Site ++ ".sql pushd " ++ Site ++ "/" ++ Site ++ " > /dev/null
        hg init hg add hg commit -m 'Content Snapshot' popd > /dev/null").
