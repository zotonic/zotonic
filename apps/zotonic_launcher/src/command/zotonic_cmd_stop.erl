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
%%% Created : 18. Dec 2017 4:26 PM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_stop).
-author("Blaise").

%% API
-export([run/1]).

-include("../../include/zotonic_command.hrl").

run(_) ->
    case zotonic_command:net_start() of
        ok ->
            case zotonic_command:get_target_node() of
                {ok, Target} ->
                    io:format("Stopping zotonic ~p ..", [Target]),
                    case net_adm:ping(Target) of
                        pong ->
                            rpc:call(Target, init, stop, []),
                            wait_stopped(Target, timestamp() + ?MAXWAIT);
                        pang ->
                            io:format(" Not running~n"),
                            halt()
                    end;
                {error, _} = Error ->
                    zotonic_command:format_error(Error)
            end;
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end.

wait_stopped( Target, Till ) ->
    case timestamp() > Till of
        false ->
            case net_adm:ping(Target) of
                pong ->
                    io:format("."),
                    timer:sleep(1000),
                    wait_stopped( Target, Till );
                pang ->
                    io:format(" OK~n")
            end;
        true ->
            io:format(" Still not stopped after ~p seconds~n", [ ?MAXWAIT ]),
            halt(1)
    end.

timestamp() ->
    {A, B, _} = os:timestamp(),
    A * 1000000 + B.
