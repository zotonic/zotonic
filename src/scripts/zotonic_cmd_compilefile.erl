%%%-------------------------------------------------------------------
%%% @author Melki
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Aug 2017 10:42 PM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_compilefile).
-author("Melki").

%% API
-export([run/1]).

-include("zotonic_escript_helper.hrl").

usage() ->
    io:format("Usage: compilefile [files...] ~n"),
    halt().

run([]) ->
    usage();

run(File) ->
    FileName = list_to_atom(File),
    net_kernel:start([zotonic_compilefile, shortnames]),
    erlang:set_cookie(node(), erlang:get_cookie()),
    Target = list_to_atom(?NODENAME ++ "@" ++ ?NODEHOST),

    Res = rpc:call(Target, zotonic_compile, compile_options, [FileName]),
    io:format("~p~n", [Res]).
