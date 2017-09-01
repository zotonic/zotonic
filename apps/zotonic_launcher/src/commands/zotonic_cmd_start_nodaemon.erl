%%%-------------------------------------------------------------------
%%% @author Melki
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Aug 2017 9:45 AM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_start_nodaemon).
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
            net_kernel:start([Target, shortnames]),
            zotonic_launcher_config:load_configs(),
            zotonic:start(),

            receive
                {'EXIT', _, _} ->
                    ok
            end;
        false ->
            io:format("Building Zotonic for the first time.~n"),
            make:all()
    end.
