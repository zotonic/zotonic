%%%-------------------------------------------------------------------
%%% @author Melki
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Aug 2017 9:50 AM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_update).
-author("Melki").

%% API
-export([run/1]).

-include("zotonic_escript_helper.hrl").


run(_) ->
    net_kernel:start([zotonic_update, shortnames]),
    erlang:set_cookie(node(), erlang:get_cookie()),
    Target = list_to_atom(?NODENAME ++ "@" ++ ?NODEHOST),
    io:format("Updating Zotonic ~p~n", [Target]),
    rpc:call(Target, zotonic, update, []),
    io:format("Update:~p", [Target]),
    io:format("~n").
