%%%-------------------------------------------------------------------
%%% @author Melki
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Aug 2017 9:42 AM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_sitetest).
-author("Melki").

%% API
-export([run/1]).

-include("zotonic_escript_helper.hrl").


run(Site) ->
    SiteName = list_to_atom(Site),

    case string:is_empty(Site) of
        true  ->
            io:format("Usage: zotonic sitetest [Sitename]"),
            halt();
        false ->
            net_kernel:start([zotonic_sitetest, shortnames]),
            erlang:set_cookie(node(), erlang:get_cookie()),
            Target = list_to_atom(?NODENAME ++ "@" ++ ?NODEHOST),
            io:format("Stopping site ~p on zotonic ~p~n", [SiteName, Target]),
            rpc:call(Target, z_sitetest, run, [SiteName]),
            io:format("~n")
    end.
