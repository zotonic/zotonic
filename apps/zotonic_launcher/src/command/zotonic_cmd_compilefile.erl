%%%-------------------------------------------------------------------
%%% @author Blaise
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%Licensed under the Apache License, Version 2.0 (the "License");
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
%%% Created : 13. Dec 2017 7:26 PM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_compilefile).
-author("Blaise").

%% API
-export([run/1]).

-include("zotonic_command.hrl").

usage() ->
    io:format("Usage: compilefile [files...] ~n"),
    halt().

run([]) ->
    usage();

run(File) ->
    Filename = list_to_atom(File),
    net_kernel:start([zotonic_compilefile, shortnames]),
    erlang:set_cookie(node(), erlang:get_cookie()),
    Target = list_to_atom(?NODENAME ++ "@" ++ ?NODEHOST),

    Res = rpc:call(Target, zotonic_compile, compile_options, [Filename]),
    io:format("~p~n", [Res]).
