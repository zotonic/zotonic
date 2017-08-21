%%%-------------------------------------------------------------------
%%% @author Melki
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Aug 2017 10:32 PM
%%%-------------------------------------------------------------------
-author("Melki").

-define(NODENAME, "zotonic001").
-define(ZOTONIC, get_zotonic_path()).
-define(NODEHOST, get_node_host()).
-define(ZOTONIC_SCRIPTS, ?ZOTONIC ++ "/src/scripts").

get_zotonic_path() ->
    {ok, CurrentDir} = file:get_cwd(),
    _Dir = CurrentDir.

get_node_host() ->
    {ok, HostName} = inet:gethostname(),
    _NodeHost = HostName.

