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
%%% Created : 18. Dec 2017 4:42 PM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_update).
-author("Blaise").

%% API
-export([info/0, run/1]).

-include("../../include/zotonic_command.hrl").

info() ->
    "Update the server. Compiles and loads any new code, flushes caches and rescans all modules.".

run(_) ->
    case zotonic_command:net_start() of
        ok ->
            io:format("Updating zotonic .."),
            Res = zotonic_command:rpc(zotonic, update, []),
            io:format("~n~p~n", [ Res ]);
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end.
