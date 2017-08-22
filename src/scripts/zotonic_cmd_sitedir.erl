%%%-------------------------------------------------------------------
%%% @author Melki
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Aug 2017 9:40 AM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_sitedir).
-author("Melki").

%% API
-export([run/1]).

-include("zotonic_escript_helper.hrl").

run([]) ->
    io:format("USAGE: Schema_name ~n"),
    io:format("USAGE: See ZotonicCommands.txt ~N"),
    halt();

run(Site) ->
    SiteName = list_to_atom(Site),
    net_kernel:start([zotonic_sitedir, shortnames]),
    erlang:set_cookie(node(), erlang:get_cookie()),
    Target = list_to_atom(?NODENAME ++ "@" ++ ?NODEHOST),
    Path = rpc:call(Target, z_path, site_dir, [SiteName]),
    io:format("~s~n~n", [Path]).
