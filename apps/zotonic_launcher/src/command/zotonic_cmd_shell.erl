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
    net_kernel:start([zotonic_shell, shortnames]),
    erlang:set_cookie(node(), erlang:get_cookie()),
    Target = list_to_atom(?NODENAME ++ "@" ++ ?NODEHOST),
    _ = supervisor:terminate_child(kernel_sup, user),
    Shell = user_drv:start(['tty_sl -c -e', {Target, shell, start, []}]),
    true = erlang:link(Shell),
    receive
        {'EXIT', Shell, _} ->
            ok
    end.
