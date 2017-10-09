%%%-------------------------------------------------------------------
%%% @author Melki
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Aug 2017 9:31 AM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_logtail).
-author("Melki").

%% API
-export([run/1]).

-include("zotonic_escript_helper.hrl").


run(_) ->
    net_kernel:start([zotonic_logtail, shortnames]),
    erlang:set_cookie(node(), erlang:get_cookie()),
    Target = list_to_atom(?NODENAME ++ "@" ++ ?NODEHOST),

    Res = rpc:call(Target, z_access_syslog, start_link, []),
    io:format("~p~n", [Res]).
