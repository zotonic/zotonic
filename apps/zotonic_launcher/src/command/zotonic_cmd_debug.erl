%%%-------------------------------------------------------------------
%%% @author Blaise
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
%%% @doc
%%%
%%% @end
%%% Created : 13. Dec 2017 7:55 PM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_debug).
-author("Blaise").

%% API
-export([run/1]).

-include("../../include/zotonic_command.hrl").

run(_) ->
    ZotonicApp = string:concat(?ZOTONIC, "/_build/default/lib/zotonic_core/ebin/zotonic_core.app"),
    Target = list_to_atom(?NODENAME ++ "@" ++ ?NODEHOST),
    case filelib:is_file(ZotonicApp) of
        true ->
            io:format("Starting Zotonic ~s..", [Target]),
            _OldUser = kill_old_user(),
            _ = supervisor:terminate_child(kernel_sup, user),
            _ = user_drv:start(),
            ok = wait_until_user_started(3000),
            NewUser = whereis(user),

            Shell = NewUser,
            % Shell = user_drv:start(),
            % ok = wait_until_user_started(3000)
            % true = erlang:link(Shell),
            net_kernel:start([Target, shortnames]),
            zotonic_launcher_config:load_configs(),
            zotonic:start(),

            receive
                {'EXIT', Shell, _} ->
                    ok
            end;
        false ->
            io:format("Cannot start Zotonic: first build Zotonic using 'make'. ~n")
    end.


%% Timeout is a period to wait before giving up
wait_until_user_started(0) ->
    erlang:error(timeout);
wait_until_user_started(Timeout) ->
    case whereis(user) of
        %% if user is not yet registered wait a tenth of a second and try again
        undefined -> timer:sleep(100), wait_until_user_started(Timeout - 100);
        _ -> ok
    end.

kill_old_user() ->
    OldUser = whereis(user),
    %% terminate the current user's port, in a way that makes it shut down,
    %% but without taking down the supervision tree so that the escript doesn't
    %% fully die
    [P] = [P || P <- element(2,process_info(whereis(user), links)), is_port(P)],
    user ! {'EXIT', P, normal}, % pretend the port died, then the port can die!
    exit(P, kill),
    wait_for_port_death(1000, P),
    OldUser.

wait_for_port_death(N, _) when N < 0 ->
    %% This risks displaying a warning!
    whatever;
wait_for_port_death(N, P) ->
    case erlang:port_info(P) of
        undefined ->
            ok;
        _ ->
            timer:sleep(10),
            wait_for_port_death(N-10, P)
    end.

