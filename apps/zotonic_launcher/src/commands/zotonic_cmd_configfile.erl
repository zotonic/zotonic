%%%-------------------------------------------------------------------
%%% @author Melki
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Aug 2017 10:49 PM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_configfile).
-author("Melki").

%% API
-export([run/1]).

-include("zotonic_escript_helper.hrl").

run(_) ->
    net_kernel:start([zotonic_comfigfile, shortnames]),
    erlang:set_cookie(node(), erlang:get_cookie()),
    Target = list_to_atom(?NODENAME ++ "@" ++ ?NODEHOST),

    [Res] = rpc:call(Target, zotonic_launcher_config, zotonic_config_file, []),
    io:format("~p~n", [list_to_atom(Res)]).
