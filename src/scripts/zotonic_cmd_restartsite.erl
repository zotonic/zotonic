%%%-------------------------------------------------------------------
%%% @author Melki
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Aug 2017 9:34 AM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_restartsite).
-author("Melki").

%% API
-export([run/1]).

-include("zotonic_escript_helper.hrl").

run([]) ->
    io:format("USAGE: restartsite [site_name]"),
    halt();

run(Site) ->
    SiteName = list_to_atom(Site),
    net_kernel:start([zotonic_restartsite, shortnames]),
    erlang:set_cookie(node(), erlang:get_cookie()),
    Target = list_to_atom(?NODENAME ++ "@" ++ ?NODEHOST),
    io:format("Restarting site ~p on zotonic ~p~n", [SiteName, Target]),
    Res = rpc:call(Target, z, shell_restartsite, [SiteName]),
    io:format("~p~n", [Res]).
