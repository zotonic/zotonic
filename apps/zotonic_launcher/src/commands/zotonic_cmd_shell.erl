%%%-------------------------------------------------------------------
%%% @author Melki
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Aug 2017 9:39 AM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_shell).
-author("Melki").

%% API
-export([run/1]).

-include("zotonic_escript_helper.hrl").


run(_) ->
    net_kernel:start([zotonic_shell, shortnames]),
    erlang:set_cookie(node(), erlang:get_cookie()),
    Target = list_to_atom(?NODENAME ++ "@" ++ ?NODEHOST),

    case zotonic:ping() of
        pong ->
            Shell = user_drv:start(['tty_sl -c -e', {Target, shell, start, []}]),
            true = erlang:link(Shell),
            receive
                {'EXIT', Shell, _} ->
                    ok
            end
    end.
