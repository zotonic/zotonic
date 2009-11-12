-module(pgsql_pool).

-export([start_link/2, start_link/3, stop/1]).
-export([get_connection/1, get_connection/2, return_connection/2]).
-export([get_database/1]).

-export([init/1, code_change/3, terminate/2]). 
-export([handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {id, connections, monitors, waiting, opts}).

 %% -- client interface --

opts(Opts) ->
    Defaults = [{host, "localhost"},
                {port, 5432},
                {password, ""},
                {username, "zotonic"},
                {database, "zotonic"}],
    Opts2 = lists:ukeysort(1, proplists:unfold(Opts)),
    proplists:normalize(lists:ukeymerge(1, Opts2, Defaults), []).


start_link(Size, Opts) ->
    gen_server:start_link(?MODULE, {undefined, Size, opts(Opts)}, []).

start_link(undefined, Size, Opts) ->
    start_link(Size, Opts);
start_link(Name, Size, Opts) ->
    gen_server:start_link({local, Name}, ?MODULE, {Name, Size, opts(Opts)}, []).

stop(P) ->
    gen_server:cast(P, stop).

get_connection(P) ->
    get_connection(P, 1000).
    
get_connection(P, Timeout) ->
    gen_server:cast(P, {get_connection, self()}),
    receive
        {pgsql_pool, P, {connection, C}} ->
            P ! {pgsql_pool, P, {ack, self(), C}},
            {ok, C}
    after
        Timeout ->
            gen_server:cast(P, {cancel_wait, self()}),
            {error, timeout}
    end.

return_connection(P, C) ->
    gen_server:call(P, {return_connection, C}).


get_database(P) ->
    {ok, C} = get_connection(P),
    {ok, Db} = pgsql_connection:database(C),
    return_connection(P, C),
    {ok, Db}.

%% -- gen_server implementation --

init({Name, Size, Opts}) ->
    process_flag(trap_exit, true),

    case Name of
        undefined -> Id = self();
        _Name     -> Id = Name
    end,
    Connections = connect(Size, Opts),

    State = #state{
      id          = Id,
      opts        = Opts,
      connections = Connections,
      monitors    = [],
      waiting     = queue:new()},
    {ok, State}.

handle_call({return_connection, C},  _From, State) ->
    #state{monitors = Monitors} = State,
    case lists:keytake(C, 1, Monitors) of
        {value, {C, M}, Monitors2} ->
            erlang:demonitor(M),
            {reply, ok, return(C, State#state{monitors = Monitors2})};
        false ->
            {reply, ok, State}
    end;

handle_call(Request, _From, State) ->
    {stop, {unsupported_call, Request}, State}.

handle_cast({get_connection, Pid}, State) ->
    #state{connections = Connections, waiting = Waiting} = State,
    case Connections of
        [C | T] -> {noreply, deliver(Pid, C, State#state{connections = T})};
        []      -> {noreply, State#state{waiting = queue:in(Pid, Waiting)}}
    end;

handle_cast({cancel_wait, Pid}, State) ->
    #state{waiting = Waiting} = State,
    Waiting2 = queue:filter(fun(P) -> P =/= Pid end, Waiting),
    {noreply, State#state{waiting = Waiting2}};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Request, State) ->
    {stop, {unsupported_cast, Request}, State}.

handle_info({'DOWN', M, process, _Pid, _Info}, State) ->
    #state{monitors = Monitors} = State,
    case lists:keytake(M, 2, Monitors) of
        {value, {C, M}, Monitors2} ->
            State2 = return(C, State#state{monitors = Monitors2}),
            {noreply, State2};
        false ->
            {noreply, State}
    end;

handle_info({'EXIT', Pid, _Reason}, State) ->
    #state{opts = Opts, connections = Connections, monitors = Monitors} = State,
    Connections2 = lists:delete(Pid, Connections),
    F = fun({C, M}) when C == Pid -> erlang:demonitor(M), false;
           ({_, _})               -> true
        end,
    Monitors2 = lists:filter(F, Monitors),
    [C] = connect(1, Opts),
    State2 = return(C, State#state{connections = Connections2, monitors = Monitors2}),
    {noreply, State2};

handle_info({pgsql_pool, P, {ack, Pid, _C}}, #state{id = P} = State) ->
    error_logger:error_msg("pgsql_pool ~p received late ack from ~p~n", [P, Pid]),
    {noreply, State};

handle_info(Info, State) ->
    {stop, {unsupported_info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.

%% -- internal functions --

connect(N, Opts) ->
    connect(N, Opts, []).

connect(0, _Opts, Acc) ->
    Acc;
connect(N, Opts, Acc) ->
    Host     = proplists:get_value(host, Opts),
    Username = proplists:get_value(username, Opts),
    Password = proplists:get_value(password, Opts),
    {ok, C} = pgsql:connect(Host, Username, Password, Opts),
    connect(N - 1, Opts, [C | Acc]).

deliver(Pid, C, State) ->
    #state{id = Id, connections = Connections, monitors = Monitors} = State,
    Pid ! {pgsql_pool, Id, {connection, C}},
    receive
        {pgsql_pool, Id, {ack, Pid, C}} ->
            M = erlang:monitor(process, Pid),
            Monitors2 = [{C, M} | Monitors],
            State#state{monitors = Monitors2}
    after
        100 ->
            State#state{connections = [C | Connections]}
    end.

return(C, State) ->
    #state{connections = Connections, waiting = Waiting} = State,
    case queue:out(Waiting) of
        {{value, Pid}, Waiting2} ->
            State2 = deliver(Pid, C, State),
            State2#state{waiting = Waiting2};
        {empty, _Waiting} ->
            Connections2 = [C | Connections],
            State#state{connections = Connections2}
    end.
