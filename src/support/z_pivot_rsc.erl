%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2015 Marc Worrell
%% @doc Pivoting server for the rsc table. Takes care of full text indices. Polls the pivot queue for any changed resources.

%% Copyright 2009-2015 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(z_pivot_rsc).
-author("Marc Worrell <marc@worrell.nl").
-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
    poll/1,
    pivot/2,
    pivot_delay/1,
    pivot_resource_update/4,
    pivot_resource/2,

    queue_all/1,
    insert_queue/2,

    get_pivot_title/1,
    get_pivot_title/2,

    insert_task/3,
    insert_task/4,
    insert_task/5,
    insert_task_after/6,
    get_task/1,
    get_task/2,
    get_task/3,
    get_task/4,
    delete_task/3,
    delete_task/4,

    stemmer_language/1,
    cleanup_tsv_text/1,
    pg_lang/1,
    pg_lang_extra/1,
    get_pivot_data/2,

    define_custom_pivot/3,
    lookup_custom_pivot/4
]).

-include("zotonic.hrl").

% Interval (in seconds) to check if there are any items to be pivoted.
-define(PIVOT_POLL_INTERVAL_FAST, 2).
-define(PIVOT_POLL_INTERVAL_SLOW, 20).

% How many PIVOT_POLL_INTERVAL_SLOW we will skip on SQL errors (like timeouts)
-define(BACKOFF_POLL_ERROR, 4).

% Number of queued ids taken from the queue at one go
-define(POLL_BATCH, 50).

%% Minimum day, inserted for date start search ranges
-define(EPOCH_START, {{-4700,1,1},{0,0,0}}).

%% Max number of characters for a tsv vector.
-define(MAX_TSV_LEN, 30000).

%% Max number of task retries
-define(TASK_MAX_RETRY, 10).


-record(state, {
    site,
    is_initial_delay = true,
    is_pivot_delay = false,
    backoff_counter = 0
}).


%% @doc Poll the pivot queue for the database in the context
%% @spec poll(Context) -> void()
poll(Context) ->
    gen_server:cast(Context#context.pivot_server, poll).


%% @doc An immediate pivot request for a resource
-spec pivot(integer(), #context{}) -> ok.
pivot(Id, Context) ->
    gen_server:cast(Context#context.pivot_server, {pivot, Id}).

%% @doc Delay the next pivot, useful when performing big updates
-spec pivot_delay(#context{}) -> ok.
pivot_delay(Context) ->
    gen_server:cast(Context#context.pivot_server, pivot_delay).


%% @doc Return a modified property list with fields that need immediate pivoting on an update.
pivot_resource_update(Id, UpdateProps, RawProps, Context) ->
    Props = lists:foldl(fun(Key, All) ->
                                case proplists:is_defined(Key, UpdateProps) of
                                    false ->
                                        [{Key, proplists:get_value(Key, RawProps)}|All];
                                    true ->
                                        All
                                end
                        end, UpdateProps, [date_start, date_end, title]),

    {DateStart, DateEnd} = pivot_date(Props),
    PivotTitle = truncate(get_pivot_title(Props), 60),
    Props1 = [
        {pivot_date_start, DateStart},
        {pivot_date_end, DateEnd},
        {pivot_date_start_month_day, month_day(DateStart)},
        {pivot_date_end_month_day, month_day(DateEnd)},
        {pivot_title, PivotTitle}
        | Props
    ],
    z_notifier:foldr(#pivot_update{id=Id, raw_props=RawProps}, Props1, Context).

month_day(undefined) -> undefined;
month_day(?EPOCH_START) -> undefined;
month_day(?ST_JUTTEMIS) -> undefined;
month_day({{_Y,M,D}, _}) -> M*100+D.


%% @doc Rebuild the search index by queueing all resources for pivot.
queue_all(Context) ->
    erlang:spawn(fun() ->
                    queue_all(0, Context)
                 end).

queue_all(FromId, Context) ->
    case z_db:q("select id from rsc where id > $1 order by id limit 1000", [FromId], Context) of
        [] ->
            done;
        Rs ->
            Ids = [ Id || {Id} <- Rs ],
            do_insert_queue(Ids, Context),
            queue_all(lists:last(Ids), Context)
    end.

%% @doc Insert a rsc_id in the pivot queue
insert_queue(Id, Context) when is_integer(Id) ->
    insert_queue([Id], Context);
insert_queue(Ids, Context) when is_list(Ids) ->
    gen_server:cast(Context#context.pivot_server, {insert_queue, Ids}).

%% @doc Insert a slow running pivot task. For example syncing category numbers after an category update.
insert_task(Module, Function, Context) ->
    insert_task(Module, Function, undefined, [], Context).

%% @doc Insert a slow running pivot task. Use the UniqueKey to prevent double queued tasks.
insert_task(Module, Function, UniqueKey, Context) ->
    insert_task(Module, Function, UniqueKey, [], Context).

%% @doc Insert a slow running pivot task with unique key and arguments.
insert_task(Module, Function, undefined, Args, Context) ->
    insert_task(Module, Function, z_ids:id(), Args, Context);
insert_task(Module, Function, UniqueKey, Args, Context) ->
    insert_task_after(undefined, Module, Function, UniqueKey, Args, Context).

%% @doc Insert a slow running pivot task with unique key and arguments that should start after Seconds seconds.
insert_task_after(SecondsOrDate, Module, Function, UniqueKey, Args, Context) ->
    z_db:transaction(fun(Ctx) -> insert_transaction(SecondsOrDate, Module, Function, UniqueKey, Args, Ctx) end, Context).

    insert_transaction(SecondsOrDate, Module, Function, UniqueKey, Args, Context) ->
        Due = to_utc_date(SecondsOrDate),
        UniqueKeyBin = z_convert:to_binary(UniqueKey),
        Fields = [
            {module, Module},
            {function, Function},
            {key, UniqueKeyBin},
            {args, Args},
            {due, Due}
        ],
        case z_db:q1("select id
                      from pivot_task_queue
                      where module = $1 and function = $2 and key = $3",
                     [Module, Function, UniqueKeyBin],
                     Context)
        of
            undefined ->
                z_db:insert(pivot_task_queue, Fields, Context);
            Id when is_integer(Id) ->
                case Due of
                    undefined -> nop;
                    _ -> z_db:update(pivot_task_queue, Id, Fields, Context)
                end,
                {ok, Id}
        end.


get_task(Context) ->
    z_db:assoc("
            select *
            from pivot_task_queue",
            Context).

get_task(Module, Context) ->
    z_db:assoc("
            select *
            from pivot_task_queue
            where module = $1",
            [Module],
            Context).

get_task(Module, Function, Context) ->
    z_db:assoc("
            select *
            from pivot_task_queue
            where module = $1 and function = $2",
            [Module, Function],
            Context).

get_task(Module, Function, UniqueKey, Context) ->
    UniqueKeyBin = z_convert:to_binary(UniqueKey),
    z_db:assoc_row("
            select *
            from pivot_task_queue
            where module = $1 and function = $2 and key = $3",
            [Module, Function, UniqueKeyBin],
            Context).

to_utc_date(undefined) ->
    undefined;
to_utc_date(N) when is_integer(N) ->
    calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + N);
to_utc_date({Y,M,D} = YMD) when is_integer(Y), is_integer(M), is_integer(D) ->
    {YMD,{0,0,0}};
to_utc_date({{Y,M,D},{H,I,S}} = Date) when is_integer(Y), is_integer(M), is_integer(D), is_integer(H), is_integer(I), is_integer(S) ->
    Date.


delete_task(Module, Function, Context) ->
    z_db:q("delete from pivot_task_queue where module = $1 and function = $2",
           [Module, Function],
           Context).

delete_task(Module, Function, UniqueKey, Context) ->
    UniqueKeyBin = z_convert:to_binary(UniqueKey),
    z_db:q("delete from pivot_task_queue where module = $1 and function = $2 and key = $3",
           [Module, Function, UniqueKeyBin],
           Context).


%%====================================================================
%% API
%%====================================================================
%% @spec start_link(SiteProps) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(SiteProps) ->
    {host, Host} = proplists:lookup(host, SiteProps),
    Name = z_utils:name_for_host(?MODULE, Host),
    gen_server:start_link({local, Name}, ?MODULE, Host, []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Host) ->
    lager:md([
        {site, Host},
        {module, ?MODULE}
      ]),
    timer:send_after(?PIVOT_POLL_INTERVAL_SLOW*1000, poll),
    {ok, #state{site=Host, is_initial_delay=true, is_pivot_delay=false}}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Poll the queue for the default host
handle_cast(poll, #state{is_initial_delay=true} = State) ->
    {noreply, State};
handle_cast(poll, State) ->
    try
        do_poll(z_context:new(State#state.site)),
        {noreply, State}
    catch
        ?WITH_STACKTRACE(Type, Err, Stack)
            lager:error("Poll error ~p:~p, backing off pivoting. Stack: ~p", [ Type, Err, Stack ]),
            {noreply, State#state{ backoff_counter = ?BACKOFF_POLL_ERROR }}
    end;

%% @doc Insert an id into the queue.
handle_cast({insert_queue, Ids}, State) when is_list(Ids) ->
    do_insert_queue(Ids, z_context:new(State#state.site)),
    z_utils:flush_message({'$gen_cast', {insert_queue, Ids}}),
    {noreply, State};

%% @doc Requests for immediate pivot of a resource
handle_cast({pivot, Id}, #state{ is_initial_delay = true } = State) ->
    do_insert_queue([Id], z_context:new(State#state.site)),
    {noreply, State};
handle_cast({pivot, Id}, #state{ backoff_counter = Ct } = State) when Ct > 0 ->
    do_insert_queue([Id], z_context:new(State#state.site)),
    {noreply, State};
handle_cast({pivot, Id}, State) ->
    do_pivot(Id, z_context:new(State#state.site)),
    {noreply, State};

%% @doc Delay the next pivot, useful when performing big updates
handle_cast(pivot_delay, State) ->
    {noreply, State#state{is_pivot_delay=true}};

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
handle_info(poll, #state{ is_pivot_delay = true } = State) ->
    timer:send_after(?PIVOT_POLL_INTERVAL_SLOW*1000, poll),
    {noreply, State#state{ is_pivot_delay = false}};
handle_info(poll, #state{backoff_counter = Ct} = State) when Ct > 0 ->
    timer:send_after(?PIVOT_POLL_INTERVAL_SLOW*1000, poll),
    {noreply, State#state{ backoff_counter = Ct - 1 }};
handle_info(poll, State) ->
    try
        case do_poll(z_context:new(State#state.site)) of
            true ->  timer:send_after(?PIVOT_POLL_INTERVAL_FAST*1000, poll);
            false -> timer:send_after(?PIVOT_POLL_INTERVAL_SLOW*1000, poll)
        end,
        {noreply, State#state{ is_initial_delay = false }}
    catch
        ?WITH_STACKTRACE(Type, Err, Stack)
            lager:error("Pivot error ~p:~p, backing off pivoting. Stack: ~p", [ Type, Err, Stack ]),
            timer:send_after(?PIVOT_POLL_INTERVAL_SLOW*1000, poll),
            {noreply, State#state{ backoff_counter = ?BACKOFF_POLL_ERROR }}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

%% @doc Insert a list of ids into the pivot queue.
do_insert_queue(Ids, Context) when is_list(Ids) ->
    F = fun(Ctx) ->
        z_db:q("lock table rsc_pivot_queue in share row exclusive mode", Ctx),
        lists:foreach(
            fun(Id) ->
                case z_db:q1("select id from rsc where id = $1", [Id], Ctx) of
                    Id ->
                        case z_db:q("update rsc_pivot_queue set serial = serial + 1 where rsc_id = $1", [Id], Ctx) of
                            1 -> ok;
                            0 ->
                                z_db:q("
                                    insert into rsc_pivot_queue (rsc_id, due, is_update)
                                    select id, current_timestamp, true from rsc where id = $1",
                                    [Id], Ctx)
                        end;
                    undefined ->
                        ok
                end
            end,
            Ids)
    end,
    case z_db:transaction(F, Context) of
        ok ->
            ok;
        {rollback, Reason} ->
            % Retry in 100msec
            lager:error("pivot: rollback during pivot queue insert: ~p", [Reason]),
            timer:apply_after(100, ?MODULE, insert_queue, [Ids, Context]);
        {error, Reason} ->
            lager:error("pivot: error during pivot queue insert: ~p", [Reason])
    end.


%% @doc Poll a database for any queued updates.
do_poll(Context) ->
    DidTask = do_poll_task(Context),
    do_poll_queue(Context) or DidTask.

do_poll_task(Context) ->
    execute_task( poll_task(Context), Context ).

execute_task({TaskId, Module, Function, _Key, Args, RetryCt, Props}, Context) ->
    try
        case erlang:apply(Module, Function, z_convert:to_list(Args) ++ [Context]) of
            {delay, Delay} ->
                Due = if
                        is_integer(Delay) ->
                            calendar:gregorian_seconds_to_datetime(
                                calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + Delay);
                        is_tuple(Delay) ->
                            Delay
                      end,
                z_db:q("update pivot_task_queue set due = $1 where id = $2", [Due, TaskId], Context);
            {delay, Delay, NewArgs} ->
                Due = if
                        is_integer(Delay) ->
                            calendar:gregorian_seconds_to_datetime(
                                calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + Delay);
                        is_tuple(Delay) ->
                            Delay
                      end,
                Fields = [
                    {due, Due},
                    {args, NewArgs}
                ],
                z_db:update(pivot_task_queue, TaskId, Fields, Context);
            _OK ->
                z_db:q("delete from pivot_task_queue where id = $1", [TaskId], Context)
        end
    catch
        ?WITH_STACKTRACE(error, undef, Trace)
            lager:error("Undefined task, aborting: ~p:~p(~p) ~p",
                        [Module, Function, Args, Trace]),
            z_db:q("delete from pivot_task_queue where id = $1", [TaskId], Context);
        ?WITH_STACKTRACE(Error, Reason, Trace)
            lager:error("Task ~p failed (~p:~p): ~p:~p(~p) ~p",
                        [TaskId, Error, Reason, Module, Function, Args, Trace]),
            task_schedule_retry(TaskId, RetryCt, Props, Context)
    end,
    true;
execute_task(empty, _Context) ->
    false.


task_schedule_retry(TaskId, RetryCt, _Props, Context) when RetryCt >= ?TASK_MAX_RETRY ->
    z_db:q("delete from pivot_task_queue where id = $1", [TaskId], Context);
task_schedule_retry(TaskId, RetryCt, Props, Context) ->
    Props1 = [
        {error_ct, RetryCt+1}
        | proplists:delete(error_ct, Props)
    ],
    Due = calendar:gregorian_seconds_to_datetime(
                                calendar:datetime_to_gregorian_seconds(calendar:universal_time())
                                + task_retry_backoff(RetryCt)),
    lager:info("Task ~p set to retry on ~p", [ TaskId, Due ]),
    z_db:q("
        update pivot_task_queue
        set due = $1,
            props = $2
        where id = $3",
        [ Due, ?DB_PROPS(Props1), TaskId ],
        Context).

task_retry_backoff(0) -> 10;
task_retry_backoff(1) -> 1800;
task_retry_backoff(2) -> 7200;
task_retry_backoff(3) -> 14400;
task_retry_backoff(4) -> 12 * 3600;
task_retry_backoff(N) -> (N-4) * 24 * 3600.


%% @doc Pivot queued resource ids
do_poll_queue(Context) ->
    do_pivot_queued( fetch_queue(Context), Context ).

%% @doc Pivot a specific id, delete its queue record if present
do_pivot(Id, Context) ->
    Serial = fetch_queue_id(Id, Context),
    do_pivot_queued([ {Id, Serial} ], Context).

do_pivot_queued([], _Context) ->
    false;
do_pivot_queued(Qs, Context) ->
    F = fun(Ctx) ->
                [ {Id, catch pivot_resource(Id, Ctx)} || {Id,_Serial} <- Qs]
        end,
    case z_db:transaction(F, Context) of
        {rollback, PivotError} ->
            lager:error("Pivot error: ~p: ~p",
                        [PivotError, Qs]);
        L when is_list(L) ->
            lists:map(fun({Id, _Serial}) ->
                            IsA = m_rsc:is_a(Id, Context),
                            z_notifier:notify(#rsc_pivot_done{id=Id, is_a=IsA}, Context),
                            % Flush the resource, as some synthesized attributes might depend on the pivoted fields.
                            % @todo Only do this if some fields are changed
                            m_rsc_update:flush(Id, Context)
                      end, Qs),
            lists:map(fun({_Id, ok}) -> ok;
                         ({Id,Error}) -> log_error(Id, Error, Context) end,
                      L),
            delete_queue(Qs, Context)
    end,
    true.

log_error(Id, Error, _Context) ->
    lager:warning("Pivot error ~p: ~p", [Id, Error]).

%% @doc Fetch the next task uit de task queue, if any.
poll_task(Context) ->
    case z_db:q_row("select id, module, function, key, props
                     from pivot_task_queue
                     where due is null
                        or due < current_timestamp
                     order by due asc
                     limit 1", Context)
    of
        {Id, Module, Function, Key, Props} ->
            Args = proplists:get_value(args, Props, []),
            RetryCt = proplists:get_value(error_ct, Props, 0),
            {Id, z_convert:to_atom(Module), z_convert:to_atom(Function), Key, Args, RetryCt, Props};
        undefined ->
            empty
    end.

%% @doc Fetch the next batch of ids from the queue. Remembers the serials, as a new
%% pivot request might come in while we are pivoting.
%% @spec fetch_queue(Context) -> [{Id,Serial}]
fetch_queue(Context) ->
    z_db:q("select rsc_id, serial from rsc_pivot_queue where due < current_timestamp - '10 second'::interval order by is_update, due limit $1", [?POLL_BATCH], Context).

%% @doc Fetch the serial of id's queue record
fetch_queue_id(Id, Context) ->
    z_db:q1("select serial from rsc_pivot_queue where rsc_id = $1", [Id], Context).

%% @doc Delete the previously queued ids iff the queue entry has not been updated in the meanwhile
delete_queue(Qs, Context) ->
    F = fun(Ctx) ->
        lists:foreach(
            fun({Id, Serial}) ->
                delete_queue(Id, Serial, Ctx)
            end,
            Qs)
    end,
    z_db:transaction(F, Context).

%% @doc Delete a specific id/serial combination
delete_queue(_Id, undefined, _Context) ->
    ok;
delete_queue(Id, Serial, Context) ->
    z_db:q("delete from rsc_pivot_queue where rsc_id = $1 and serial = $2", [Id,Serial], Context).



%% @doc Pivot a resource, collect all texts for indexing and some extra to be indexed fields.
%% @todo Also add the property tag/values
%% @spec pivot_resource(Id, Context) -> void()
pivot_resource(Id, Context) ->
    pivot_resource_1(Id, get_pivot_rsc(Id, Context), Context).

pivot_resource_1(_Id, {error, enoent}, _Context) ->
    ok;
pivot_resource_1(Id, {ok, R}, Context) ->
    {ObjIds, CatIds, [TA,TB,TC,TD]} = get_pivot_data(Id, R, Context),

    StemmerLanguage = stemmer_language(Context),
    {SqlA, ArgsA} = to_tsv(TA, $A, [], StemmerLanguage),
    {SqlB, ArgsB} = to_tsv(TB, $B, ArgsA, StemmerLanguage),
    {SqlC, ArgsC} = to_tsv(TC, $C, ArgsB, StemmerLanguage),
    {SqlD, ArgsD} = to_tsv(TD, $D, ArgsC, StemmerLanguage),

    TsvObj = [ [" zpo",integer_to_list(OId)] || OId <- ObjIds ],
    TsvCat = [ [" zpc",integer_to_list(CId)] || CId <- CatIds ],
    TsvIds = list_to_binary([TsvObj,TsvCat]),

    PropsPrePivoted = z_pivot_rsc:pivot_resource_update(Id, [], m_rsc:get_raw(Id, Context), Context),

    Rtsv = z_db:q1("select to_tsvector($1)", [TsvIds], Context),
    TsvSql = [SqlA, " || ", SqlB, " || ", SqlC, " || ", SqlD],
    Tsv  = z_db:q1(iolist_to_binary(["select ", TsvSql]), ArgsD, Context),

    KVs = [
        {pivot_tsv, Tsv},
        {pivot_rtsv, Rtsv},
        {pivot_street, truncate(proplists:get_value(address_street_1, R), 120)},
        {pivot_city, truncate(proplists:get_value(address_city, R), 100)},
        {pivot_postcode, truncate(proplists:get_value(address_postcode, R), 30)},
        {pivot_state, truncate(proplists:get_value(address_state, R), 50)},
        {pivot_country, truncate(proplists:get_value(address_country, R), 80)},
        {pivot_first_name, truncate(proplists:get_value(name_first, R), 100)},
        {pivot_surname, truncate(proplists:get_value(name_surname, R), 100)},
        {pivot_gender, truncate(proplists:get_value(gender, R), 1)},
        {pivot_date_start, proplists:get_value(pivot_date_start, PropsPrePivoted)},
        {pivot_date_end, proplists:get_value(pivot_date_end, PropsPrePivoted)},
        {pivot_date_start_month_day, proplists:get_value(pivot_date_start_month_day, PropsPrePivoted)},
        {pivot_date_end_month_day, proplists:get_value(pivot_date_end_month_day, PropsPrePivoted)},
        {pivot_title, proplists:get_value(pivot_title, PropsPrePivoted)},
        {pivot_location_lat, get_float(location_lat, R)},
        {pivot_location_lng, get_float(location_lng, R)}
    ],

    KVsFolded = z_notifier:foldr(#pivot_fields{id=Id, rsc=R}, KVs, Context),

    % Check which fields are changed, update only those
    case lists:filter(fun({K,V}) when is_list(V) -> proplists:get_value(K, R) =/= iolist_to_binary(V);
                         ({K,undefined}) -> proplists:get_value(K, R) =/= undefined;
                         ({K,V}) when is_atom(V) -> proplists:get_value(K, R) =/= z_convert:to_binary(V);
                         ({K,V}) -> proplists:get_value(K, R) =/= V
                      end,
                      KVsFolded)
    of
        [] ->
            % No fields changed, nothing to do
            nop;
        KVsChanged ->
            % Make Sql update statement for the changed fields
            {Sql, Args} = lists:foldl(fun({K,V}, {Sq,As}) ->
                                         {[Sq,
                                            case As of [] -> []; _ -> $, end,
                                            z_convert:to_list(K),
                                            " = $", integer_to_list(length(As)+1)
                                          ],
                                          [V|As]}
                                      end,
                                      {"update rsc set ",[]},
                                      KVsChanged),

            z_db:q1(iolist_to_binary([Sql, " where id = $", integer_to_list(length(Args)+1)]),
                    lists:reverse([Id|Args]),
                    Context)
    end,

    CustomPivots = z_notifier:map(#custom_pivot{id=Id}, Context),
    lists:foreach(
            fun
                (undefined) -> ok;
                (none) -> ok;
                ({error, _} = Error) ->
                    lager:error("Error return from custom pivot of ~p, error: ~p",
                                [Id, Error]);
                (Res) ->
                    update_custom_pivot(Id, Res, Context)
            end,
            CustomPivots),
    ok.


%% Make the setweight(to_tsvector()) parts of the update statement
to_tsv([], _Level, Args, _StemmingLanguage) ->
    {"tsvector('')", Args};
to_tsv(List, Level, Args, StemmingLanguage) ->
    {Sql1, Args1} = lists:foldl(
        fun ({_Lang,Text}, {Sql, As}) ->
            N   = length(As) + 1,
            CleanedText = cleanup_tsv_text(z_html:unescape(z_html:strip(Text))),
            Truncated = z_string:truncate(CleanedText, ?MAX_TSV_LEN, <<>>),
            As1 = As ++ [Truncated],
            {[["setweight(to_tsvector('pg_catalog.",StemmingLanguage,"', $",integer_to_list(N),"), '",Level,"')"] | Sql], As1}
        end,
        {[], Args},
        List),
    {z_utils:combine(" || ", Sql1), Args1}.

cleanup_tsv_text(Text) when is_binary(Text) ->
    lists:foldl(
        fun (R, Acc) ->
            binary:replace(Acc, R, <<" ">>, [global])
        end,
        z_string:sanitize_utf8(Text),
        [<<0>>, <<"-">>, <<"/">>]).

get_float(K, Ps) ->
    case proplists:get_value(K, Ps) of
        undefined -> undefined;
        L when is_list(L); is_binary(L) -> z_convert:to_float(iolist_to_binary(L));
        F when is_float(F) -> F;
        N when is_integer(N) -> N * 1.0
    end.

truncate(undefined, _Len) -> undefined;
truncate(S, Len) -> iolist_to_binary(
                        z_string:trim(
                            z_string:to_lower(
                                truncate_1(S, Len, Len)))).

truncate_1(_S, 0, _Bytes) ->
    "";
truncate_1(S, Utf8Len, Bytes) ->
    case z_string:truncate(S, Utf8Len, "") of
        T when length(T) > Bytes -> truncate_1(T, Utf8Len-1, Bytes);
        L -> L
    end.


%% @doc Fetch the date range from the record
pivot_date(R) ->
    DateStart = z_datetime:undefined_if_invalid_date(proplists:get_value(date_start, R)),
    DateEnd   = z_datetime:undefined_if_invalid_date(proplists:get_value(date_end, R)),
    pivot_date1(DateStart, DateEnd).

    pivot_date1(S, E) when not is_tuple(S) andalso not is_tuple(E) ->
        {undefined, undefined};
    pivot_date1(S, E) when not is_tuple(S) andalso is_tuple(E) ->
        { ?EPOCH_START, E};
    pivot_date1(S, E) when is_tuple(S) andalso not is_tuple(E) ->
        {S, ?ST_JUTTEMIS};
    pivot_date1(S, E) when is_tuple(S) andalso is_tuple(E) ->
        {S, E}.


%% @doc Fetch the first title from the record for sorting.
get_pivot_title(Id, Context) ->
    z_string:truncate( z_string:to_lower(get_pivot_title([{title, m_rsc:p(Id, title, Context)}])), 50, <<>>).

get_pivot_title(Props) ->
    case proplists:get_value(title, Props) of
        {trans, []} ->
            "";
        {trans, [{_, Text}|_]} ->
            z_string:to_lower(Text);
        T ->
            z_string:to_lower(T)
    end.


%% @doc Return the data for the pivoter
get_pivot_rsc(Id, Context) ->
    case z_db:assoc_props_row("select * from rsc where id = $1", [Id], Context) of
        undefined -> {error, enoent};
        FullRecord -> {ok, z_notifier:foldl(pivot_rsc_data, FullRecord, Context)}
    end.


%% get_pivot_data {objids, catids, [ta,tb,tc,td]}
get_pivot_data(Id, Context) ->
    case get_pivot_rsc(Id, Context) of
        {ok, Rsc} ->
            get_pivot_data(Id, Rsc, Context);
        {error, _} ->
            {[], [], []}
    end.

get_pivot_data(Id, Rsc, Context) ->
    R = z_notifier:foldr(#pivot_get{id=Id}, Rsc, Context),
    {A,B} = lists:foldl(fun(Res,Acc) -> fetch_texts(Res, Acc, Context) end, {[],[]}, R),
    {ObjIds, ObjTexts} = related(Id, Context),
    {CatIds, CatTexts} = category(proplists:get_value(category_id, R), Context),
    Split = [ (split_lang(Ts, Context)) || Ts <- [A, [], B++CatTexts, ObjTexts] ],
    {ObjIds, CatIds, [ [ {Lng,list_to_binary(z_utils:combine(32, Ts))} || {Lng,Ts} <- Ps] || Ps <- Split ]}.


%% @doc Split texts into different languages
split_lang(Texts, Context) ->
    Dict = split_lang(Texts, dict:new(), Context),
    dict:to_list(Dict).

split_lang([], Dict, _Context) -> Dict;
split_lang([{trans, Texts}|Rest], Dict, Context) ->
    Dict2 = lists:foldl(fun({Lang,Text}, D) -> add_lang(Lang, z_html:strip(Text), D) end, Dict, Texts),
    split_lang(Rest, Dict2, Context);
split_lang([Text|Rest], Dict, Context) ->
    Dict2 = add_lang(z_context:language(Context), Text, Dict),
    split_lang(Rest, Dict2, Context).

    add_lang(Lang, Text, Dict) ->
        case dict:find(Lang, Dict) of
            {ok, _} -> dict:append(Lang, z_html:strip(Text), Dict);
            error -> dict:store(Lang, [z_html:strip(Text)], Dict)
        end.


%% @doc Fetch the title of all things related to the resource
related(Id, Context) ->
    Edges = lists:filter(
                    fun({Predicate, _Edges}) ->
                        not z_convert:to_bool(m_rsc:p_no_acl(m_rsc:rid(Predicate, Context), is_object_noindex, Context))
                    end,
                    m_edge:get_edges(Id, Context)),
    Ids = lists:usort(
                lists:flatten(
                    [ [ proplists:get_value(object_id, E) || E <- Es ] || {_Pred, Es} <- Edges])),
    Ids1 = z_notifier:foldr(#pivot_related{id=Id}, Ids, Context),
    IdsTexts = z_notifier:foldr(#pivot_related_text_ids{id=Id}, Ids, Context),
    Texts = [ m_rsc:p_no_acl(R, title, Context) || R <- IdsTexts ],
    {Ids1, Texts}.


%% @doc Fetch the names of all categories in the category path
%% @spec category(int(), Context) -> { IdList, TextsList }
category(CatId, Context) ->
    Names = [ z_convert:to_list(Name) || Name <- m_category:is_a(CatId, Context) ],
    Ids   = [ CatId |  m_category:get_path(CatId, Context) ],
    {Ids, Names}.


fetch_texts({F, _} = FV, Acc, Context) ->
    case do_pivot_field(F) of
        false -> Acc;
        true -> fetch_texts_1(FV, Acc, Context)
    end.

    fetch_texts_1({title, Value}, {A,B}, _Context) ->
        {[Value|A], B};
    fetch_texts_1({subtitle, Value}, {A,B}, _Context) ->
        {[Value|A], B};
    fetch_texts_1({name_surname, Value}, {A,B}, _Context) ->
        {[Value|A], B};
    fetch_texts_1({name_first, Value}, {A,B}, _Context) ->
        {[Value|A], B};
    fetch_texts_1({F, Value}, {A,B}, _Context) when is_binary(Value) ->
        case is_lang_neutral(F, Value) of
            true ->
                {A, [{trans, [{none, Value}]}|B]};
            false ->
                case do_pivot_field(F) of
                    false -> {A,B};
                    true -> {A, [Value|B]}
                end
        end;
    fetch_texts_1({F, {{Y,M,D},{H,Min,S}} = Date}, {A,B} = Acc, Context)
        when is_integer(Y) andalso is_integer(M) andalso is_integer(D)
            andalso is_integer(H) andalso is_integer(Min) andalso is_integer(S) ->
        case do_pivot_field(F) of
            false ->
                Acc;
            true ->
                case catch erlydtl_dateformat:format(Date, "Y m d H i F l h", Context) of
                    {'EXIT', _} -> Acc;
                    Formatted -> {A, [Formatted|B]}
                end
        end;
    fetch_texts_1({_, {trans, _} = V}, {A,B}, _Context) ->
        {A, [V|B]};
    fetch_texts_1({blocks, Blocks}, AB, Context) ->
        lists:foldl(fun(Block, ABAcc) ->
                        fetch_texts_block(Block, ABAcc, Context)
                    end,
                    AB,
                    Blocks);
    fetch_texts_1({_, V}, {A,B} = Acc, _Context) ->
        case z_string:is_string(V) of
            true -> {A, [V|B]};
            false -> Acc
        end.

fetch_texts_block(Block, {A,B}, Context) ->
    Header = proplists:get_value(header, Block),
    Body = proplists:get_value(body, Block),
    RscTitle = m_rsc:p_no_acl(proplists:get_value(rsc_id, Block), title, Context),
    B1 = maybe_add_text(Header, B),
    B2 = maybe_add_text(Body, B1),
    B3 = maybe_add_text(RscTitle, B2),
    {A, B3}.

maybe_add_text(Text, A) ->
    case z_utils:is_empty(Text) of
        true -> A;
        false -> [Text|A]
    end.

% Suppress some fields that are only for supporting the pivoting
do_pivot_field(pivot_category_nr) -> false;
do_pivot_field(pivot_tsv) -> false;
do_pivot_field(pivot_rtsv) -> false;
do_pivot_field(pivot_first_name) -> false;
do_pivot_field(pivot_surname) -> false;
do_pivot_field(pivot_gender) -> false;
do_pivot_field(pivot_date_start) -> false;
do_pivot_field(pivot_date_end) -> false;
do_pivot_field(pivot_date_start_month_day) -> false;
do_pivot_field(pivot_date_end_month_day) -> false;
do_pivot_field(pivot_street) -> false;
do_pivot_field(pivot_city) -> false;
do_pivot_field(pivot_state) -> false;
do_pivot_field(pivot_postcode) -> false;
do_pivot_field(pivot_country) -> false;
do_pivot_field(pivot_geocode) -> false;
do_pivot_field(pivot_geocode_qhash) -> false;
do_pivot_field(uri) -> false;
do_pivot_field(publication_start) -> false;
do_pivot_field(publication_end) -> false;
do_pivot_field(created) -> false;
do_pivot_field(modified) -> false;
do_pivot_field(location_lat) -> false;
do_pivot_field(location_lng) -> false;
do_pivot_field(computed_location_lat) -> false;
do_pivot_field(computed_location_lng) -> false;
do_pivot_field(_) -> true.


%% @doc some fields are taken as-is without any language processing
is_lang_neutral(_, {trans, _}) -> false;
is_lang_neutral(address_street_1, _) -> true;
is_lang_neutral(address_street_2, _) -> true;
is_lang_neutral(address_city, _) -> true;
is_lang_neutral(address_postcode, _) -> true;
is_lang_neutral(address_state, _) -> true;
is_lang_neutral(address_country, _) -> true;
is_lang_neutral(mail_street_1, _) -> true;
is_lang_neutral(mail_street_2, _) -> true;
is_lang_neutral(mail_city, _) -> true;
is_lang_neutral(mail_postcode, _) -> true;
is_lang_neutral(mail_state, _) -> true;
is_lang_neutral(mail_country, _) -> true;
is_lang_neutral(email, _) -> true;
is_lang_neutral(phone, _) -> true;
is_lang_neutral(phone_alt, _) -> true;
is_lang_neutral(phone_emergency, _) -> true;
is_lang_neutral(_, _) -> false.


%% @doc Translate a language to a language string as used by
%% postgresql. This language list is the intersection of the default
%% catalogs of postgres with the languages supported by
%% mod_translation.
pg_lang(dk) -> "danish";
pg_lang(nl) -> "dutch";
pg_lang(en) -> "english";
pg_lang(fi) -> "finnish";
pg_lang(fr) -> "french";
pg_lang(de) -> "german";
pg_lang(hu) -> "hungarian";
pg_lang(it) -> "italian";
pg_lang(no) -> "norwegian";
pg_lang(ro) -> "romanian";
pg_lang(ru) -> "russian";
pg_lang(es) -> "spanish";
pg_lang(se) -> "swedish";
pg_lang(tr) -> "turkish";
pg_lang(<<"dk">>) -> "danish";
pg_lang(<<"nl">>) -> "dutch";
pg_lang(<<"en">>) -> "english";
pg_lang(<<"fi">>) -> "finnish";
pg_lang(<<"fr">>) -> "french";
pg_lang(<<"de">>) -> "german";
pg_lang(<<"hu">>) -> "hungarian";
pg_lang(<<"it">>) -> "italian";
pg_lang(<<"no">>) -> "norwegian";
pg_lang(<<"ro">>) -> "romanian";
pg_lang(<<"ru">>) -> "russian";
pg_lang(<<"es">>) -> "spanish";
pg_lang(<<"se">>) -> "swedish";
pg_lang(<<"tr">>) -> "turkish";
pg_lang(_) -> "english".

%% Map extra languages, these are from the i18n.language_stemmer configuration and not
%% per default installed in PostgreSQL
pg_lang_extra(Iso) ->
    case iso639:lc2lang(z_convert:to_list(Iso)) of
        <<"">> ->
            pg_lang(Iso);
        Lang ->
            lists:takewhile(fun
                                (C) when C >= $a, C =< $z -> true;
                                (_) -> false
                            end,
                            z_convert:to_list(z_string:to_lower(Lang)))
    end.

% Default stemmers in a Ubuntu psql install:
%
%  pg_catalog | danish_stem     | snowball stemmer for danish language
%  pg_catalog | dutch_stem      | snowball stemmer for dutch language
%  pg_catalog | english_stem    | snowball stemmer for english language
%  pg_catalog | finnish_stem    | snowball stemmer for finnish language
%  pg_catalog | french_stem     | snowball stemmer for french language
%  pg_catalog | german_stem     | snowball stemmer for german language
%  pg_catalog | hungarian_stem  | snowball stemmer for hungarian language
%  pg_catalog | italian_stem    | snowball stemmer for italian language
%  pg_catalog | norwegian_stem  | snowball stemmer for norwegian language
%  pg_catalog | portuguese_stem | snowball stemmer for portuguese language
%  pg_catalog | romanian_stem   | snowball stemmer for romanian language
%  pg_catalog | russian_stem    | snowball stemmer for russian language
%  pg_catalog | simple          | simple dictionary: just lower case and check for stopword
%  pg_catalog | spanish_stem    | snowball stemmer for spanish language
%  pg_catalog | swedish_stem    | snowball stemmer for swedish language
%  pg_catalog | turkish_stem    | snowball stemmer for turkish language

%% @doc Return the language used for stemming the full text index.
%%      We use a single stemming to prevent having seperate indexes per language.
-spec stemmer_language(#context{}) -> string().
stemmer_language(Context) ->
    StemmingLanguage = m_config:get_value(i18n, language_stemmer, Context),
    case z_utils:is_empty(StemmingLanguage) of
        true -> pg_lang(z_trans:default_language(Context));
        false -> pg_lang_extra(StemmingLanguage)
    end.


%% @spec define_custom_pivot(Module, columns(), Context) -> ok
%% @doc Let a module define a custom pivot
%% columns() -> [column()]
%% column()  -> {ColumName::atom(), ColSpec::string()} | {atom(), string(), options::list()}
define_custom_pivot(Module, Columns, Context) ->
    TableName = "pivot_" ++ z_convert:to_list(Module),
    case z_db:table_exists(TableName, Context) of
        true ->
            % Compare column names to see if table needs an update
            DbColumns = [ Name || #column_def{name=Name} <- z_db:columns(TableName, Context), not(Name == id)],
            SpecColumns = lists:map(
                fun(ColumnDef) ->
                    [Name|_] = tuple_to_list(ColumnDef),
                    Name
                end,
                Columns
            ),
            case lists:usort(SpecColumns) == lists:usort(DbColumns) of
                false ->
                    z_db:drop_table(TableName, Context),
                    define_custom_pivot(Module, Columns, Context);
                true ->
                    ok
            end;
        false ->
            ok = z_db:transaction(
                    fun(Ctx) ->
                        Fields = custom_columns(Columns),
                        Sql = "CREATE TABLE " ++ TableName ++ "(" ++
                              "id int NOT NULL," ++ Fields ++ " primary key(id))",

                        [] = z_db:q(lists:flatten(Sql), Ctx),

                        [] = z_db:q("ALTER TABLE " ++ TableName ++
                                    " ADD CONSTRAINT fk_" ++ TableName ++ "_id " ++
                                    " FOREIGN KEY (id) REFERENCES rsc(id) ON UPDATE CASCADE ON DELETE CASCADE", Ctx),

                        Indexable = lists:filter(fun({_,_}) -> true;
                                                    ({_,_,Opts}) -> not lists:member(noindex, Opts)
                                                 end,
                                                 Columns),
                        Idx = [
                                begin
                                    K = element(1,Col),
                                    "CREATE INDEX " ++ TableName ++ "_" ++ z_convert:to_list(K) ++ "_key ON "
                                    ++ TableName ++ "(" ++ z_convert:to_list(K) ++ ")"
                                end
                                || Col <- Indexable
                            ],
                        lists:foreach(
                            fun(Sql1) ->
                                [] = z_db:q(Sql1, Ctx)
                            end,
                            Idx),
                        ok
                    end,
                    Context),
            z_db:flush(Context),
            ok
    end.


custom_columns(Cols) ->
    custom_columns(Cols, []).

custom_columns([], Acc) ->
    lists:reverse(Acc);
custom_columns([{Name, Spec}|Rest], Acc) ->
    custom_columns(Rest, [ [z_convert:to_list(Name), " ", Spec, ","] |  Acc]);
custom_columns([{Name, Spec, _Opts}|Rest], Acc) ->
    custom_columns(Rest, [ [z_convert:to_list(Name), " ", Spec, ","] |  Acc]).


update_custom_pivot(Id, {Module, Columns}, Context) ->
    TableName = "pivot_" ++ z_convert:to_list(Module),
    Result = case z_db:select(TableName, Id, Context) of
        {ok, []} ->
            z_db:insert(TableName, [{id, Id}|Columns], Context);
        {ok, _Row}  ->
            z_db:update(TableName, Id, Columns, Context);
        {error, _} = Error ->
            Error
    end,
    case Result of
        {ok, _} -> ok;
        {error, Reason} ->
            lager:error("Error updating custom pivot ~p for ~p (~p): ~p",
                        [Module, Id, z_context:site(Context), Reason])
    end.


%% @doc Lookup a custom pivot; give back the Id based on a column. Will always return the first Id found.
%% @spec lookup_custom_pivot(Module, Column, Value, Context) -> Id | undefined
lookup_custom_pivot(Module, Column, Value, Context) ->
    TableName = "pivot_" ++ z_convert:to_list(Module),
    Column1 = z_convert:to_list(Column),
    Query = "SELECT id FROM " ++ TableName ++ " WHERE " ++ Column1 ++ " = $1",
    case z_db:q(Query, [Value], Context) of
        [] -> undefined;
        [{Id}|_] -> Id
    end.
