%%%-------------------------------------------------------------------
%%% @author Melki
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Aug 2017 10:13 AM
%%%-------------------------------------------------------------------
-module(zotonic_release).
-author("Melki").

%% API
-export([run/0]).

-include_lib("zotonic_core/include/zotonic_release.hrl").


run() ->
    io:format("Zotonic ~s~n", [?ZOTONIC_VERSION]).
