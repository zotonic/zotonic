%% @doc Supervisor for file processes, these processes cache and maintain file information for 
%% other file services.  They can also resize, combine files and compress files.

-module(z_file_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-export([
    ensure_file/4
    ]).

-define(SERVER, ?MODULE).

ensure_file(Path, Root, OptFilters, Context) ->
    case z_file_entry:where(Path, Context) of
        Pid when is_pid(Pid) ->
            lookup_file(Pid);
        undefined ->
            case supervisor:start_child(?SERVER, [Path, Root, OptFilters, Context]) of
                {ok, Pid} ->
                    lookup_file(Pid);
                {error,{already_started, Pid}} ->
                    lookup_file(Pid)
            end
    end.

lookup_file(Pid) ->
    z_file_entry:lookup(Pid).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Element = {z_file_entry, {z_file_entry, start_link, []},
               temporary, brutal_kill, worker, [z_file_entry]},
    Children = [Element],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

