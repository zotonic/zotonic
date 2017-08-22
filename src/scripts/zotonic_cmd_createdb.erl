%%%-------------------------------------------------------------------
%%% @author Melki
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Aug 2017 9:20 AM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_createdb).
-author("Melki").

%% API
-export([run/1]).

-include("zotonic_escript_helper.hrl").

run([]) ->
    io:format("USAGE: createdb ~s"),
    io:format("USAGE: See ZotonicCommands.txt ~n");

run(Site) ->
    SiteName = list_to_atom(Site),
    net_kernel:start([zotonic_createdb, shortnames]),
    erlang:set_cookie(node(), erlang:get_cookie()),
    Target = list_to_atom(?NODENAME ++ "@" ++ ?NODEHOST),

    Res = rpc:call(Target, z_db, prepare_database, [SiteName]),
    io:format("~p~n", [Res]).
