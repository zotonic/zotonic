%%%-------------------------------------------------------------------
%%% @author Melki
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Aug 2017 9:52 AM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_wait).
-author("Melki").

%% API
-export([run/1]).

-include("zotonic_escript_helper.hrl").

-define(MAXWAIT, 30).

run(_) ->
    net_kernel:start([zotonic_status, shortnames]),
    erlang:set_cookie(node(), erlang:get_cookie()),
    Target = list_to_atom(?NODENAME ++ "@" ++ ?NODEHOST),

    io:format("Waiting for Zotonic to start: "),
    case rpc:call(Target, zotonic, ping, []) of
        pong  ->
            io:format("Started ~n");
        _ ->
            io:format("Zotonic not started in ~p~n", [?MAXWAIT])
    end.
