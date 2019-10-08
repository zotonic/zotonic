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
%%% Created : 18. Dec 2017 4:07 PM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_startsite).
-author("Blaise").

%% API
-export([run/1]).

-include("../../include/zotonic_command.hrl").

run([ Site ]) ->
    SiteName = list_to_atom(Site),
    net_kernel:start([zotonic_startsite, shortnames]),
    erlang:set_cookie(node(), erlang:get_cookie()),
    Target = list_to_atom(?NODENAME ++ "@" ++ ?NODEHOST),

    io:format("Starting site ~p on zotonic ~p~n", [SiteName, Target]),
    Res = rpc:call(Target, z, shell_startsite, [SiteName]),
    io:format("~p~n", [Res]);

run(_) ->
    io:format("USAGE: startsite [site_name] ~n"),
    halt().
