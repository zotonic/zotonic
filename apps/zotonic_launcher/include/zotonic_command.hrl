%%%-------------------------------------------------------------------
%%% @author Blaise
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Dec 2017 6:52 PM
%%%-------------------------------------------------------------------
-author("Blaise").

-define(NODENAME, "zotonic001").
-define(ZOTONIC, get_zotonic_dir()).
-define(NODEHOST, get_node_host()).
-define(MAXWAIT, 30).

get_zotonic_dir() ->
    {ok, CurrentDir} = file:get_cwd(),
    _Dir = CurrentDir.

get_node_host() ->
    {ok, HostName} = inet:gethostname(),
    _NodeHost = HostName.
