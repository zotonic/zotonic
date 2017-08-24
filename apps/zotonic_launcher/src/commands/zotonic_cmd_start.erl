%%%-------------------------------------------------------------------
%%% @author Melki
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Aug 2017 9:45 AM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_start).
-author("Melki").

%% API
-export([run/1]).

-include("zotonic_escript_helper.hrl").

run(_) ->
    ZotonicApp = string:concat(?ZOTONIC, "/_build/default/lib/zotonic_core/ebin/zotonic_core.app"),
    Target = list_to_atom(?NODENAME ++ "@" ++ ?NODEHOST),
    case filelib:is_file(ZotonicApp) of
        true ->
            io:format("Starting Zotonic ~s..", [Target]),
            zotonic_launcher_config:load_configs(),
            net_kernel:start([Target, shortnames]),
            zotonic:start(),
            case zotonic:ping() of
                pong ->
                    io:format("OK ~n");
                pang ->
                    io:format("Something went wrong while starting Zotonic. Please check the log files ~s~n",
                        [string:concat(?ZOTONIC, "/priv/log")])
            end;
        false ->
            io:format("Building Zotonic for the first time.~n")
    end.
