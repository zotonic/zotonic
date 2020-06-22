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
%%% Created : 18. Dec 2017 12:09 PM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_shell).
-author("Blaise").

%% API
-export([run/1]).

-include("../../include/zotonic_command.hrl").

run(_) ->
    case zotonic_command_nodename:nodename_target( list_to_atom(?DEFAULT_NODENAME) ) of
        {ok, {shortnames, Nodename}} ->
            shell_command("-sname", Nodename);
        {ok, {longnames, Nodename}} ->
            shell_command("-name", Nodename);
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end.

shell_command(SOpt, TargetNode) ->
    TempNode = os:getpid()
        ++ z_convert:to_list(z_ids:identifier(3))
        ++ "-" ++ atom_to_list(TargetNode),
    Cmd = "erl " ++ SOpt ++ " \"" ++ TempNode
        ++ "\" -remsh \"" ++ atom_to_list(TargetNode) ++ "\""
        ++ " -hidden",
    io:format("~s", [ Cmd ]).
