%%%-------------------------------------------------------------------
%%% @author Melki
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Aug 2017 9:49 AM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_stopsite).
-author("Melki").

%% API
-export([run/1]).

-include("zotonic_escript_helper.hrl").


run(Site) ->
    SiteName = list_to_atom(Site),

    case string:is_empty(Site) of
        true  ->
            io:format("USAGE: ~p [site_name] ~n", [list_to_atom(escript:script_name())]),
            halt();
        false ->
            net_kernel:start([zotonic_stopsite, shortnames]),
            erlang:set_cookie(node(), erlang:get_cookie()),
            Target = list_to_atom(?NODENAME ++ "@" ++ ?NODEHOST),
            io:format("Stopping site ~p on zotonic ~p~n", [SiteName, Target]),
            rpc:call(Target, z, shell_stopsite, [SiteName]),
            io:format("~n")
    end.
