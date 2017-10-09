%%%-------------------------------------------------------------------
%%% @author Melki
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Aug 2017 9:48 AM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_stop).
-author("Melki").

%% API
-export([run/1]).

-include("zotonic_escript_helper.hrl").


run(_) ->
    net_kernel:start([zotonic_stop, shortnames]),
    erlang:set_cookie(node(), erlang:get_cookie()),
    Target = list_to_atom(?NODENAME ++ "@" ++ ?NODEHOST),
    io:format("Stopping Zotonic ~p..", [Target]),
    case net_adm:ping(Target) of
        pong  ->
            rpc:call(Target, init, stop, []),
            io:format("Ok~n");
        pang ->
            halt()
    end.
