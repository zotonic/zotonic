%%%-------------------------------------------------------------------
%%% @author Melki
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Aug 2017 1:19 PM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_debug).
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
            Shell = user_drv:start(),
            true = erlang:link(Shell),
            net_kernel:start([Target, shortnames]),
            zotonic_launcher_config:load_configs(),
            zotonic:start(),

            receive
                {'EXIT', Shell, _} ->
                    ok
            end;
        false ->
            io:format("Building Zotonic for the first time.~n"),
            make:all()
    end.
