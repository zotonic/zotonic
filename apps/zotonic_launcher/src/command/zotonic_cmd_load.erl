%%%-------------------------------------------------------------------
%%% @author Blaise
%%%Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%% @doc
%%%
%%% @end
%%% Created : 13. Dec 2017 6:59 PM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_load).
-author("Blaise").

%% API
-export([info/0, run/1]).

info() ->
    "Force reload of an Erlang module.".

run(_) ->
    case zotonic_command:net_start() of
        ok ->
            Res = zotonic_command:rpc(zotonic_filehandler_compile, ld, []),
            io:format("~p~n", [ Res ]);
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end.
