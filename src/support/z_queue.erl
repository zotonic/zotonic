%% @doc A generic task queue with prioritized, scheduled and batched execution
%%      and failure handling through exponential backoff.
-module(z_queue).

-export([
    queue/2,
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-behaviour(gen_server).

-record(state, {
    host :: atom()
}).

-include_lib("zotonic/include/zotonic.hrl").

-define(TICK, 50).
-define(TABLE, "queue").

start_link(SiteProps) ->
    {host, Host} = proplists:lookup(host, SiteProps),
    Name = z_utils:name_for_host(?MODULE, Host),
    gen_server:start_link({local, Name}, ?MODULE, Host, []).

init(Host) ->
    install(z_context:new(Host)),
    erlang:send_after(?TICK, self(), tick),
    {ok, #state{host = Host}}.

%% @doc Add a task to the queue.
-spec queue(#task{}, z:context()) -> ok.
queue(#task{due = Due, callback = Callback, prio = Prio, batch = Batch, data = Data}, Context) ->
    Props = [
        {callback, callback(Callback)},
        {prio, prio_to_int(Prio)},
        {due, due(Due)},
        {data, data(Data)},
        {batch, Batch}
    ],
    {ok, _} = z_db:insert(?TABLE, Props, Context),
    ok.

%% @doc Dequeue and execute the task(s) at the head of the queue.
dequeue(Context) ->
    case get_tasks(Context) of
        undefined ->
            ok;
        [#task{callback = {M, F}} | _] = Tasks ->
            try
                erlang:apply(M, F, [Tasks, Context]) of
                    ok ->
                        succeed(Tasks, Context);
                    retry ->
                        fail(Tasks, Context)
            catch
                _:Error ->
                    lager:error("Callback ~p fails with ~p", [{M, F}, Error]),
                    fail(Tasks, Context)
            end
    end.

data(undefined) ->
    undefined;
data(Data) ->
    term_to_binary(Data).

callback({M, F}) ->
    <<(z_convert:to_binary(M))/binary, ":", (z_convert:to_binary(F))/binary>>.

due(Seconds) when is_integer(Seconds) ->
    calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(calendar:universal_time() + Seconds)
    );
due(DateTime) when is_tuple(DateTime) ->
    DateTime.

prio_to_int(high) -> 0;
prio_to_int(low) -> 2.

%% @doc Get first tasks from the stack.
%%      Tasks are ordered by prio (high to low) and due date.
-spec get_tasks(z:context()) -> undefined | [#task{}].
get_tasks(Context) ->
    case z_db:assoc_row(
        "select * from " ++ ?TABLE ++ "
        where due is null or due < current_timestamp
        order by prio, due asc
        limit 1",
        Context
    ) of
        undefined ->
            undefined;
        Task ->
            batch(task(Task), Context)
    end.

%% @doc Try to batch the task by finding similar tasks.
-spec batch(#task{}, z:context()) -> [#task{}].
batch(#task{batch = undefined} = Task, _Context) ->
    [Task];
batch(#task{batch = BatchSize, callback = Callback}, Context) ->
    Tasks = z_db:assoc_props(
        "select * from " ++ ?TABLE ++ "
        where due is null or due < current_timestamp
        and callback = $1
        and batch >= $2
        order by prio, due asc
        limit $2",
        [callback(Callback), BatchSize],
        Context
    ),
    [task(Task) || Task <- Tasks].

task(Props) ->
    Callback = list_to_tuple(
        [list_to_atom(binary_to_list(L)) || L <- binary:split(proplists:get_value(callback, Props), <<":">>)]
    ),
    
    #task{
        id = proplists:get_value(id, Props),
        batch = proplists:get_value(batch, Props),
        prio = proplists:get_value(prio, Props),
        key = proplists:get_value(key, Props),
        due = proplists:get_value(due, Props),
        data = decode_binary(proplists:get_value(data, Props)),
        callback = Callback
    }.

decode_binary(undefined) ->
    undefined;
decode_binary(Binary) ->
    binary_to_term(Binary).

succeed(Tasks, Context) ->
    {ok, _Count} = z_db:equery("delete from " ++ ?TABLE ++ where_in(Tasks), Context).

%% @doc Very simple exponential backoff, without randomization.
-spec fail([#task{}], z:context()) -> ok.
fail(Tasks, Context) ->
    {ok, _Count} = z_db:equery("update " ++ ?TABLE ++ "
        set due = due + (power(2, failures) || ' minutes')::interval, failures = failures + 1 "
        ++ where_in(Tasks),
        Context
    ),
    ok.

where_in(Tasks) ->
    Ids = [integer_to_list(Id) || #task{id = Id} <- Tasks],
    " where id in (" ++ string:join(Ids, ",") ++ ")".

handle_call(_Request, _From, _State) ->
    erlang:error(not_implemented).

handle_cast(_Request, _State) ->
    erlang:error(not_implemented).

handle_info(tick, #state{host = Host} = State) ->
    Context = z_context:new(Host),
    dequeue(Context),
    erlang:send_after(?TICK, self(), tick),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

install(Context) ->
    case z_db:table_exists(?TABLE, Context) of
        false ->
            ok = z_db:create_table(
            ?TABLE,
            [
                #column_def{name = id, type = "serial", is_nullable = false, primary_key = true},
                #column_def{name = key, type = "character varying(100)", is_nullable = false, default = "''"},
                #column_def{name = due, type = "timestamp with time zone"},
                #column_def{name = callback, type = "character varying(500)", is_nullable = false},
                #column_def{name = prio, type = "integer", is_nullable = false},
                #column_def{name = batch, type = "integer"},
                #column_def{name = data, type = "bytea"},
                #column_def{name = failures, type = "integer", default = "0"}
            ],
            Context
        );
        true ->
            nop
    end.
