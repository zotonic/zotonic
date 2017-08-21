%%%-------------------------------------------------------------------
%%% @author Melki
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Aug 2017 9:27 AM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_etop).
-author("Melki").

%% API
-export([run/1]).

-include("zotonic_escript_helper.hrl").


run(_) ->
    CONSOLE_LINES = list_to_integer(os:cmd("echo -n $(tput lines)")),
    LINES = CONSOLE_LINES - 11,

    net_kernel:start([zotonic_etop, shortnames]),
    erlang:set_cookie(node(), erlang:get_cookie()),
    Target = list_to_atom(?NODENAME ++ "@" ++ ?NODEHOST),
    Res = rpc:call(Target, etop, start, [{lines, LINES}, {tracing, off}]),
    io:format("~p~n", [Res]).
