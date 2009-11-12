-module(epgsql_pool).

-behaviour(supervisor).

-export([start_pool/3]).
-export([start_link/1, init/1]).

%% -- client interface --

start_pool(Name, Size, Opts) ->
    Pool = {Name, {pgsql_pool, start_link, [Name, Size, Opts]}, permanent, 2000, worker, dynamic},
    supervisor:start_child(?MODULE, Pool).

%% -- supervisor implementation --

%% @spec start_link(Opts) -> ServerRet
%% @doc API for starting the supervisor.
start_link(Opts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Opts).


%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init(Opts) ->
    Pools = [
            {Name, {pgsql_pool, start_link, [Name, Size, PoolOpts]}, permanent, 2000, worker, dynamic}
        ||  {Name, Size, PoolOpts} <- Opts 
    ],
    {ok, {{one_for_one, 2, 60}, Pools}}.

