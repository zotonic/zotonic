%%%-------------------------------------------------------------------
%%% @author Melki
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Aug 2017 9:36 AM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_rpc).
-author("Melki").

%% API
-export([run/1]).

-include("zotonic_escript_helper.hrl").


run([Module, Func, Args]) ->
    net_kernel:start([zotonic_rpc, shortnames]),
    erlang:set_cookie(node(), erlang:get_cookie()),
    Target = list_to_atom(?NODENAME ++ "@" ++ ?NODEHOST),
    ModuleName = list_to_atom(Module),
    FuncName = list_to_atom(Func),

    rpc:call(Target, ModuleName, FuncName, [Args]).
