%%%-------------------------------------------------------------------
%%% @author Melki
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Aug 2017 1:21 PM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_snapshot).
-author("Melki").

%% API
-export([run/1]).

-include("zotonic_escript_helper.hrl").

run([]) ->
    io:format("USAGE: snapshot [site_name] ~n"),
    io:format("USAGE: See ZotonicCommands.txt ~n~n");

run(Site) ->
    os:cmd("mkdir -p " ++Site ++ "/" ++ Site ++ "/files/snapshot
	pg_dump zotonic_" ++ Site ++ "\
			> " ++ Site ++ "/" ++ Site ++ "/files/snapshot/zotonic_" ++ Site ++ ".sql
	pushd " ++ Site ++ "/" ++ Site ++ " > /dev/null
	hg init
	hg add
	hg commit -m 'Content Snapshot'
	popd > /dev/null").
