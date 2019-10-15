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
%%% Created : 18. Dec 2017 4:13 PM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_status).
-author("Blaise").

%% API
-export([run/1]).

run(_) ->
    case zotonic_command:net_start() of
        ok ->
            Sites = zotonic_command:rpc(z_sites_manager, get_sites, []),
            {ok, Target} = zotonic_command:get_target_node(),
            io:format("Running: ~p~n~n", [ Target ]),
            io:format("Sites Status: ~n"),
            io:format("============================= ~n"),
            List = maps:to_list(Sites),
            lists:foreach(
                fun({Site, Status}) ->
                    io:format("~s ~p~n", [pad(Site), Status])
                end,
                List),
            io:format("~n");
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end.

pad(Site) ->
    S = atom_to_list(Site),
    case length(S) of
        N when N < 20 ->
            S ++ pad(20 - N, "");
        _ ->
            S
    end.

pad(N, Acc) when N =< 0 -> Acc;
pad(N, Acc) -> pad(N-1, [ 32 | Acc ]).