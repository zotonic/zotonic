%%%-------------------------------------------------------------------
%%% @author Melki
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Aug 2017 9:38 AM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_runtests).
-author("Melki").

%% API
-export([run/1]).

-include("zotonic_escript_helper.hrl").

run(_) ->
    net_kernel:start([zotonic_testsandbox, shortnames]),

    io:format("Running the following tests in the testsandbox ~n"),
    apply(zotonic, await_startup, []),
    io:format("~n").
