%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell, Maas-Maarten Zeeman
%% @doc Pivoting server for the rsc table. Takes care of full text indices. Polls the pivot queue for any changed resources.
%% @end

%% Copyright 2009-2024 Marc Worrell, Maas-Maarten Zeeman
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
    status/1,
    pivot/2,
    pivot_delay/1,
    pivot_resource_update/4,
    queue_all/1,
    queue_count/1,
    queue_count_backlog/1,
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
    list_tasks/1,
    count_tasks/1,
    delete_tasks/1,

    task_job_ping/3,
    task_job_done/2,

    pivot_job_ping/2,
    pivot_job_done/2,

    publish_task_event/5,

    stemmer_language/1,
    stemmer_language_config/1,
    cleanup_tsv_text/1,
    pg_lang/1,
    pg_lang_extra/1,
    % get_pivot_data/2,

    define_custom_pivot/3,
    lookup_custom_pivot/4,

    insert_queue/3
]).

-include("zotonic.hrl").
% -include_lib("epgsql/include/epgsql.hrl").

% Interval (in seconds) to check if there are any items to be pivoted.
-define(PIVOT_POLL_INTERVAL_FAST, 2).
-define(PIVOT_POLL_INTERVAL_SLOW, 20).

% Interval to check for stuck pivot or task jobs (seconds)
-define(JOB_CHECK_INTERVAL, 10*60).

% Timeouts for pivot and tasks, killed if didn't ping in this number of seconds.
-define(PIVOT_JOB_TIMEOUT, 20*60).      % 20 minutes for pivots
-define(TASK_JOB_TIMEOUT, 2*60*60).     % 2 hours for tasks (might be shorter in future)

% How many PIVOT_POLL_INTERVAL_SLOW we will skip on SQL errors (like timeouts)
-define(BACKOFF_POLL_ERROR, 4).

% Number of queued ids taken from the queue at one go
-define(POLL_BATCH, 50).

-record(state, {
    site :: atom(),
    is_initial_delay = true :: boolean(),
    is_pivot_delay = false :: boolean(),
    backoff_counter = 0 :: integer(),

    poll_timer :: timer:tref(),

    task_pid :: undefined | pid(),
    task_id :: undefined | integer(),
    task_progress :: undefined | 0..100,
    task_ping :: undefined | erlang:timestamp(),
    task :: undefined | map(),

    pivot_pid :: undefined | pid(),
    pivot_rsc_id :: undefined | m_rsc:resource_id(),
    pivot_queue = [] :: [ m_rsc:resource_id() ],
    pivot_queue_inflight = [] :: [ m_rsc:resource_id() ],
    pivot_queue_inflight_date :: undefined | calendar:datetime(),
    pivot_inflight_date :: undefined | calendar:datetime(),
    pivot_ping :: undefined | erlang:timestamp()
}).


-type task_key() :: undefined | binary() | string() | atom() | integer().

-export_type([
    task_key/0
]).



%% @doc Poll the pivot queue for the database in the context
-spec poll( z:context() ) -> ok.
poll(Context) ->
    gen_server:cast(Context#context.pivot_server, poll).


-spec status( z:context() ) -> {ok, map()} | {error, term()}.
status(Context) ->
    gen_server:call(Context#context.pivot_server, status).

%% @doc An immediate pivot request for a resource
-spec pivot(Id, Context) -> ok when
    Id :: m_rsc:resource_id(),
    Context :: z:context().
pivot(Id, Context) ->
    gen_server:cast(Context#context.pivot_server, {pivot, Id}).

%% @doc Delay the next pivot, useful when performing big updates
-spec pivot_delay(z:context()) -> ok.
pivot_delay(Context) ->
    gen_server:cast(Context#context.pivot_server, pivot_delay).


%% @doc Return a modified property list with fields that need immediate pivoting on an update.
-spec pivot_resource_update(Id, UpdateProps, RawProps, Context) -> UpdateProps1 when
    Id :: m_rsc:resource_id(),
    UpdateProps :: m_rsc:props(),
    RawProps :: m_rsc:props(),
    Context :: z:context(),
    UpdateProps1 :: m_rsc:props().
pivot_resource_update(Id, UpdateProps, RawProps, Context) ->
    z_pivot_rsc_job:pivot_resource_update(Id, UpdateProps, RawProps, Context).

%% @doc Rebuild the search index by queueing all resources for pivot.
-spec queue_all(Context) -> ok when
    Context :: z:context().
queue_all(Context) ->
    ?LOG_INFO(#{
        in => zotonic_mod_search,
        text => <<"Pivot: queueing all resources for repivot - start">>
    }),
    Max = z_db:q1("select max(id) from rsc", Context),
    z_proc:spawn_md(fun() ->
                    queue_all_1(Max+1, Context)
                 end),
    ok.

queue_all_1(ToId, Context) ->
    case z_db:q("
        insert into rsc_pivot_log (rsc_id)
        select id
        from rsc
        where id < $1
        order by id desc
        limit 10000
        returning rsc_id",
        [ ToId ],
        Context)
    of
        [] ->
            ?LOG_INFO(#{
                in => zotonic_mod_search,
                text => <<"Pivot: queueing all resources for repivot - queued">>
            }),
            done;
        Rs ->
            Ids = [ Id || {Id} <- Rs ],
            queue_all_1(lists:min(Ids), Context)
    end.


%% @doc Return the length of the pivot queue.
-spec queue_count(Context) -> QueueLength when
    Context :: z:context(),
    QueueLength :: non_neg_integer().
queue_count(Context) ->
    z_db:q1("SELECT COUNT(distinct rsc_id) FROM rsc_pivot_log", Context).

%% @doc Return the number of pivot queue items scheduled for direct pivot.
-spec queue_count_backlog(Context) -> BacklogLength when
    Context :: z:context(),
    BacklogLength :: non_neg_integer().
queue_count_backlog(Context) ->
    z_db:q1("
        select count(distinct rsc_id)
        from rsc_pivot_log
        where due < current_timestamp", Context).

%% @doc Insert a rsc_id in the pivot queue
-spec insert_queue(IdOrIds, Context) -> ok when
    IdOrIds :: m_rsc:resource_id() | list( m_rsc:resource_id() ),
    Context :: z:context().
insert_queue(IdorIds, Context) ->
    insert_queue(IdorIds, calendar:universal_time(), Context).

%% @doc Insert a rsc_id in the pivot queue for a certain date
-spec insert_queue(IdOrIds, DueDate, Context) -> ok when
    IdOrIds :: m_rsc:resource_id() | list( m_rsc:resource_id() ),
    DueDate :: calendar:datetime(),
    Context :: z:context().
insert_queue(Id, DueDate, Context) when is_integer(Id), is_tuple(DueDate) ->
    insert_queue([Id], DueDate, Context);
insert_queue(Ids, DueDate, Context) when is_list(Ids), is_tuple(DueDate) ->
    gen_server:cast(Context#context.pivot_server, {insert_queue, DueDate, Ids}).

%% @doc Insert a slow running pivot task. For example syncing category numbers after an category update.
-spec insert_task(Module, Function, Context) -> {ok, TaskId} | {error, term()}
    when Module :: atom(),
         Function :: atom(),
         Context :: z:context(),
         TaskId :: integer().
insert_task(Module, Function, Context) ->
    insert_task_after(undefined, Module, Function, undefined, [], Context).

%% @doc Insert a slow running pivot task. Use the UniqueKey to prevent double queued tasks for
%% the same module:function.
-spec insert_task(Module, Function, UniqueKey, Context) -> {ok, TaskId} | {error, term()}
    when Module :: atom(),
         Function :: atom(),
         UniqueKey :: task_key(),
         Context :: z:context(),
         TaskId :: integer().
insert_task(Module, Function, UniqueKey, Context) ->
    insert_task_after(undefined, Module, Function, UniqueKey, [], Context).

%% @doc Insert a slow running pivot task with unique key and arguments. The key is unique
%% for the module:function.
-spec insert_task(Module, Function, UniqueKey, Args, Context) -> {ok, TaskId} | {error, term()}
    when Module :: atom(),
         Function :: atom(),
         UniqueKey :: task_key(),
         Args :: list()
               | fun( ( OldDue :: undefined | calendar:datetime(),
                        OldArgs :: undefined | list(),
                        NewDue :: calendar:datetime(),
                        z:context()
                    ) -> {ok, {calendar:datetime(), list()}} | {error, term()} ),
         Context :: z:context(),
         TaskId :: integer().
insert_task(Module, Function, UniqueKey, Args, Context) ->
    insert_task_after(undefined, Module, Function, UniqueKey, Args, Context).

%% @doc Insert a slow running pivot task with unique key and arguments that should start after Seconds seconds.
%% Always delete any existing transaction, to prevent race conditions when the task is running
%% during this insert. The UniqueKey is used to have multiple entries per module/function. If only
%% a single module/function should be queued, then set the UniqueKey to <tt>&lt;&lt;&gt;&gt;</tt>.
-spec insert_task_after(SecondsOrDate, Module, Function, UniqueKey, Args, Context) -> {ok, TaskId} | {error, term()}
    when SecondsOrDate::undefined | integer() | calendar:datetime(),
         Module :: atom(),
         Function :: atom(),
         UniqueKey :: task_key(),
         Args :: list()
               | fun( ( OldDue :: undefined | calendar:datetime(),
                        OldArgs :: undefined | list(),
                        NewDue :: calendar:datetime(),
                        z:context()
                    ) -> {ok, {calendar:datetime(), list()}} | {error, term()} ),
         Context :: z:context(),
         TaskId :: integer().
insert_task_after(SecondsOrDate, Module, Function, UniqueKey, ArgsFun, Context) ->
    gen_server:call(
        Context#context.pivot_server,
        {insert_task_after, SecondsOrDate, Module, Function, UniqueKey, ArgsFun}).

-spec get_task( z:context() ) -> {ok, [ map() ]} | {error, term()}.
get_task(Context) ->
    z_db:qmap("
            select *
            from pivot_task_queue",
            Context).

-spec get_task( module(), z:context() ) -> {ok, [ map() ]} | {error, term()}.
get_task(Module, Context) ->
    z_db:qmap("
            select *
            from pivot_task_queue
            where module = $1",
            [Module],
            Context).

-spec get_task( module(), atom(), z:context() ) -> {ok, [ map() ]} | {error, term()}.
get_task(Module, Function, Context) ->
    z_db:qmap("
            select *
            from pivot_task_queue
            where module = $1 and function = $2",
            [Module, Function],
            Context).

-spec get_task( module(), atom(), task_key(), z:context() ) -> {ok, map()} | {error, term()}.
get_task(Module, Function, UniqueKey, Context) ->
    UniqueKeyBin = z_convert:to_binary(UniqueKey),
    z_db:qmap_row("
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


-spec delete_task( module(), atom(), z:context() ) -> non_neg_integer().
delete_task(Module, Function, Context) ->
    case z_db:q("delete from pivot_task_queue where module = $1 and function = $2",
           [Module, Function],
           Context)
    of
        0 ->
            0;
        N ->
            publish_task_event(delete, Module, Function, undefined, Context),
            N
    end.


-spec delete_task( module(), atom(), task_key(), z:context() ) -> non_neg_integer().
delete_task(Module, Function, UniqueKey, Context) ->
    UniqueKeyBin = z_convert:to_binary(UniqueKey),
    case z_db:q("delete from pivot_task_queue where module = $1 and function = $2 and key = $3",
           [Module, Function, UniqueKeyBin],
           Context)
    of
        0 ->
            0;
        N ->
            publish_task_event(delete, Module, Function, undefined, Context),
            N
    end.

-spec list_tasks( z:context() ) -> {ok, list( map() )} | {error, term()}.
list_tasks(Context) ->
    z_db:qmap("select * from pivot_task_queue", Context).

-spec count_tasks( z:context() ) -> {ok, list( map() )} | {error, term()}.
count_tasks(Context) ->
    z_db:qmap("
        select module, function, count(*), min(due) as due,
               sum(error_count) as error_count_total,
               max(error_count) as error_count_max
        from pivot_task_queue
        group by module, function
        order by due", Context).

-spec delete_tasks( z:context() ) -> non_neg_integer().
delete_tasks(Context) ->
    case z_db:q("delete from pivot_task_queue", Context) of
        0 ->
            0;
        N ->
            publish_task_event(delete, <<"*">>, <<"*">>, undefined, Context),
            N
    end.

-spec get_pivot_title( m_rsc:resource_id(), z:context() ) -> binary().
get_pivot_title(Id, Context) ->
    z_pivot_rsc_job:get_pivot_title(Id, Context).

-spec get_pivot_title( map() ) -> binary().
get_pivot_title(Props) ->
    z_pivot_rsc_job:get_pivot_title(Props).


%% @doc Ping from pivot process to keep alive and report progress
-spec pivot_job_ping( m_rsc:resource_id(), z:context() ) -> ok.
pivot_job_ping(Id, Context) ->
    gen_server:cast(Context#context.pivot_server, {pivot_ping, self(), Id}).

%% @doc Signal from pivot job that processing is done.
-spec pivot_job_done(IdsOrError, Context) -> ok when
    IdsOrError :: list( m_rsc:resource_id() ) | error,
    Context :: z:context().
pivot_job_done(IdsOrError, Context) ->
    gen_server:call(Context#context.pivot_server, {pivot_done, self(), IdsOrError}, infinity).

%% @doc Ping from task process to keep alive and report progress
-spec task_job_ping( TaskId :: integer(), Percentage :: 0..100, z:context() ) -> ok.
task_job_ping(TaskId, Percentage, Context) ->
    gen_server:cast(Context#context.pivot_server, {task_ping, self(), TaskId, Percentage}).

%% @doc Signal from task process that job is finished.
-spec task_job_done( TaskId :: integer(), z:context() ) -> ok.
task_job_done(TaskId, Context) ->
    gen_server:call(Context#context.pivot_server, {task_done, self(), TaskId}, infinity).

%% @doc Return the language used for stemming the full text index.
%%      We use a single stemming to prevent having seperate indexes per language.
-spec stemmer_language(z:context()) -> string().
stemmer_language(Context) ->
    z_pivot_rsc_job:stemmer_language(Context).

-spec stemmer_language_config(z:context()) -> atom().
stemmer_language_config(Context) ->
    z_pivot_rsc_job:stemmer_language_config(Context).

-spec cleanup_tsv_text(binary()) -> binary().
cleanup_tsv_text(Text) when is_binary(Text) ->
    z_pivot_rsc_job:cleanup_tsv_text(Text).

-spec pg_lang(atom()) -> string().
pg_lang(LangCode) ->
    z_pivot_rsc_job:pg_lang(LangCode).

-spec pg_lang_extra(atom()) -> string().
pg_lang_extra(LangCode) ->
    z_pivot_rsc_job:pg_lang_extra(LangCode).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link(SiteProps) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Site) ->
    Name = z_utils:name_for_site(?MODULE, Site),
    gen_server:start_link({local, Name}, ?MODULE, Site, []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Site) ->
    z_context:logger_md(Site),
    logger:set_process_metadata(#{
        site => Site,
        module => ?MODULE
    }),
    timer:send_interval(?JOB_CHECK_INTERVAL*1000, job_check),
    {ok, TRefPoll} = timer:send_after(?PIVOT_POLL_INTERVAL_SLOW*1000, poll),
    {ok, #state{
        site = Site,
        poll_timer = TRefPoll,
        is_initial_delay = true,
        is_pivot_delay = false
    }}.

handle_call({task_done, _TaskPid, TaskId}, _From, #state{ task_id = TaskId } = State) ->
    % Handle task_done messages from task queue jobs
    State1 = State#state {
        task_id = undefined,
        task_pid = undefined
    },
    self() ! poll, % Request another poll, there could be more due tasks on the queue.
    {reply, ok, State1};
handle_call({task_done, _TaskPid, TaskId}, _From, State) ->
    ?LOG_ERROR(#{
        text => <<"Pivot received 'task_done' from unknown task job">>,
        in => zotonic_core,
        result => error,
        reason => unknown_task_job,
        task_id => TaskId
    }),
    {reply, {error, unknown_task}, State};

handle_call({pivot_done, PivotPid, error}, _From, #state{ pivot_pid = PivotPid } = State) ->
    % Error during pivot - keep the pivot log queue as is.
    {reply, ok, State#state{
        pivot_pid = undefined,
        pivot_queue_inflight = [],
        pivot_queue_inflight_date = undefined,
        pivot_inflight_date = undefined
    }};
handle_call({pivot_done, PivotPid, Ids}, _From, #state{ pivot_pid = PivotPid, site = Site } = State) ->
    % Signal that ids are pivoted, delete all entries before the cut off date.
    Context = z_context:new(Site),
    {Prio, Normal} = lists:partition(
        fun(Id) -> lists:member(Id, State#state.pivot_queue_inflight) end,
        Ids),
    delete_queue(Prio, State#state.pivot_queue_inflight_date, Context),
    delete_queue(Normal, State#state.pivot_inflight_date, Context),
    {reply, ok, State#state{
        pivot_pid = undefined,
        pivot_queue_inflight = [],
        pivot_queue_inflight_date = undefined,
        pivot_inflight_date = undefined
    }};
handle_call({pivot_done, PivotPid, _IdsOrError}, _From, State) ->
    ?LOG_ERROR(#{
        text => <<"Pivot received 'pivot_done' from unknown pivot job">>,
        in => zotonic_core,
        result => error,
        reason => unknown_pivot_job,
        pid => PivotPid
    }),
    {reply, {error, unknown_pivot}, State};

handle_call({insert_task_after, SecondsOrDate, Module, Function, UniqueKey, ArgsFun}, _From, State) ->
    Context = z_context:new(State#state.site),
    Result = do_insert_task_after(SecondsOrDate, Module, Function, UniqueKey, ArgsFun, Context),
    State1 = if
        SecondsOrDate =:= undefined ->
            next_poll(State, 0);
        true ->
            State
    end,
    {reply, Result, State1};

handle_call(status, _From, State) ->
    Status = #{
        site => State#state.site,
        is_initial_delay => State#state.is_initial_delay,
        is_pivot_delay => State#state.is_pivot_delay,
        task_pid => State#state.task_pid,
        task_id => State#state.task_id,
        task_progress => State#state.task_progress,
        pivot_pid => State#state.pivot_pid,
        pivot_rsc_id => State#state.pivot_rsc_id
    },
    {reply, {ok, Status}, State};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


handle_cast(poll, #state{ is_initial_delay = true } = State) ->
    % Manual poll of the pivot queue, but starting up - wait
    {noreply, State};
handle_cast(poll, State) ->
    % Manual poll of the pivot queue
    try
        State1 = do_poll(State),
        {noreply, State1}
    catch
        Type:Err:Stack ->
            ?LOG_ERROR(#{
                text => <<"Pivot poll error">>,
                in => zotonic_core,
                result => Type,
                reason => Err,
                stack => Stack
            }),
            {noreply, State#state{ backoff_counter = ?BACKOFF_POLL_ERROR }}
    end;

handle_cast({insert_queue, DueDate, Ids}, State) when is_list(Ids) ->
    % Insert an id into the queue.
    do_insert_queue(Ids, DueDate, z_context:new(State#state.site)),
    z_utils:flush_message({'$gen_cast', {insert_queue, DueDate, Ids}}),
    {noreply, State};

handle_cast({pivot, Id}, #state{ is_initial_delay = true } = State) when is_integer(Id) ->
    % Immediate pivot of an resource-id - but we are still starting up
    Due = z_datetime:next_minute(calendar:universal_time()),
    do_insert_queue([ Id ], Due, z_context:new(State#state.site)),
    {noreply, State};
handle_cast({pivot, Id}, #state{ backoff_counter = Ct } = State) when Ct > 0 ->
    % Immediate pivot of an resource-id - but we are in a back off due to errors
    Due = z_datetime:next_minute(calendar:universal_time()),
    do_insert_queue([ Id ], Due, z_context:new(State#state.site)),
    {noreply, State};
handle_cast({pivot, Id}, State) when is_integer(Id) ->
    % Immediate pivot of an resource-id - queue and add as priority
    do_insert_queue([ Id ], calendar:universal_time(), z_context:new(State#state.site)),
    State1 = State#state{ pivot_queue = [ Id | State#state.pivot_queue ]},
    State2 = do_poll(State1),
    {noreply, State2};

handle_cast({pivot_ping, Pid, Id}, #state{ pivot_pid = Pid } = State) ->
    State1 = State#state{
        pivot_ping = os:timestamp(),
        pivot_rsc_id = Id
    },
    {noreply, State1};
handle_cast({pivot_ping, Pid, Id}, State) ->
    ?LOG_NOTICE(#{
        text => <<"Pivot ping from unknown process">>,
        in => zotonic_core,
        result => error,
        reason => unknown_process,
        pid => Pid,
        rsc_id => Id,
        expected_pid => State#state.pivot_pid
    }),
    {noreply, State};

handle_cast({task_ping, _Pid, TaskId, Percentage}, #state{ task_id = TaskId } = State) ->
    State1 = State#state{
        task_ping = os:timestamp(),
        task_progress = Percentage
    },
    {noreply, State1};
handle_cast({task_ping, Pid, TaskId, _Percentage}, State) ->
    ?LOG_NOTICE(#{
        text => <<"Task ping for wrong task id">>,
        in => zotonic_core,
        result => error,
        reason => wrong_task_id,
        pid => Pid,
        task_id => TaskId,
        expected_task_id => State#state.task_id
    }),
    {noreply, State};

handle_cast(pivot_delay, State) ->
    % Delay the next pivot, useful when performing big updates
    {noreply, State#state{is_pivot_delay=true}};

handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @doc Handling all non call/cast messages
handle_info(poll, #state{ is_pivot_delay = true } = State) ->
    State1 = State#state{ is_pivot_delay = false },
    {noreply, next_poll(State1, ?PIVOT_POLL_INTERVAL_SLOW)};
handle_info(poll, #state{backoff_counter = Ct} = State) when Ct > 0 ->
    State1 = State#state{ backoff_counter = Ct - 1 },
    {noreply, next_poll(State1, ?PIVOT_POLL_INTERVAL_SLOW)};
handle_info(poll, #state{ pivot_pid = Pid } = State) when is_pid(Pid) ->
    ?LOG_DEBUG(#{
        text => <<"Pivot job still running, delaying next poll">>,
        in => zotonic_core,
        pivot_pid => Pid,
        reason => busy
    }),
    {noreply, next_poll(State, ?PIVOT_POLL_INTERVAL_FAST)};
handle_info(poll, #state{ site = Site } = State) ->
    case z_sites_manager:get_site_status(Site) of
        {ok, running} ->
            ?LOG_DEBUG(#{
                text => <<"Pivot poll">>,
                in => zotonic_core
            }),
            try
                State1 = do_poll(State),
                IsPivoting = is_pid(State1#state.pivot_pid)
                        orelse is_pid(State1#state.task_pid),
                Interval = case IsPivoting of
                    true ->  ?PIVOT_POLL_INTERVAL_FAST;
                    false -> ?PIVOT_POLL_INTERVAL_SLOW
                end,
                State2 = State1#state{ is_initial_delay = false },
                {noreply, next_poll(State2, Interval)}
            catch
                Type:Err:Stack ->
                    ?LOG_ERROR(#{
                        text => <<"Pivot error">>,
                        in => zotonic_core,
                        result => Type,
                        reason => Err,
                        stack => Stack
                    }),
                    StateBackoff = State#state{ backoff_counter = ?BACKOFF_POLL_ERROR },
                    {noreply, next_poll(StateBackoff, ?PIVOT_POLL_INTERVAL_SLOW)}
            end;
        _ ->
            State1 = State#state{ is_initial_delay = true },
            {noreply, next_poll(State1, ?PIVOT_POLL_INTERVAL_SLOW)}
    end;

handle_info({'DOWN', _MRef, process, _Pid, _Reason}, #state{ pivot_pid = undefined, task_pid = undefined } = State) ->
    {noreply, State};

handle_info({'DOWN', _MRef, process, Pid, Reason}, #state{ pivot_pid = Pid } = State) ->
    LastRscId = State#state.pivot_rsc_id,
    ?LOG_ERROR(#{
        text => <<"Pivot received DOWN from pivot job">>,
        in => zotonic_core,
        rsc_id => LastRscId,
        result => 'DOWN',
        reason => Reason,
        pivot_pid => Pid
    }),
    Context = z_context:new(State#state.site),
    z_db:q("
        delete from rsc_pivot_log
        where rsc_id = $1
        ",
        [ LastRscId ],
        Context),
    {noreply, State#state{
        pivot_pid = undefined,
        pivot_rsc_id = undefined,
        backoff_counter = ?BACKOFF_POLL_ERROR
    }};

handle_info({'DOWN', _MRef, process, Pid, Reason}, #state{ task = Task, task_pid = Pid } = State) ->
    Context = z_context:new(State#state.site),
    z_pivot_rsc_task_job:maybe_schedule_retry(Task, 'DOWN', Reason, [], Context),
    {noreply, State#state{ task_id = undefined, task_pid = undefined }};

handle_info(job_check, State) ->
    check_pivot_job(State),
    check_task_job(State),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

publish_task_event(What, Module, Function, undefined, Context) ->
    z_mqtt:publish(task_topic(Context),
            #{
                what => What,
                module => Module,
                function => Function
            },
            z_acl:sudo(Context));
publish_task_event(What, Module, Function, Due, Context) ->
    z_mqtt:publish(task_topic(Context),
            #{
                what => What,
                module => Module,
                function => Function,
                due => Due
            },
            z_acl:sudo(Context)).

task_topic(Context) ->
    [ <<"$SYS">>, <<"site">>, z_convert:to_binary(z_context:site(Context)), <<"task-queue">> ].

next_poll(State = #state{ poll_timer = TRef }, Interval) ->
    timer:cancel(TRef),
    z_utils:flush_message(poll),
    {ok, TRefNew} = timer:send_after(Interval*1000, poll),
    State#state{ poll_timer = TRefNew }.


check_pivot_job(#state{ pivot_pid = undefined }) ->
    ok;
check_pivot_job(#state{ pivot_pid = Pid, pivot_ping = LastPing } = State) ->
    Now = z_datetime:timestamp(),
    Ping = z_datetime:datetime_to_timestamp(calendar:now_to_universal_time(LastPing)),
    Timeout = Ping + ?PIVOT_JOB_TIMEOUT,
    case Now > Timeout of
        true ->
            ?LOG_ERROR(#{
                text => <<"Pivot job timeout, killing pivot job">>,
                in => zotonic_core,
                reason => timeout,
                pivot_pid => Pid,
                rsc_id => State#state.pivot_rsc_id
            }),
            erlang:exit(Pid, timeout);
        false ->
            ok
    end.

check_task_job(#state{ task_pid = undefined }) ->
    ok;
check_task_job(#state{ task_pid = Pid, task_id = TaskId, task_ping = LastPing } = State) ->
    Now = z_datetime:timestamp(),
    Ping = z_datetime:datetime_to_timestamp(calendar:now_to_universal_time(LastPing)),
    Timeout = Ping + ?TASK_JOB_TIMEOUT,
    case Now > Timeout of
        true ->
            ?LOG_ERROR(#{
                text => <<"Task job timeout, killing task for later retry">>,
                in => zotonic_core,
                reason => timeout,
                task_id => TaskId,
                task_pid => Pid,
                progress => State#state.task_progress
            }),
            % Force exit - the monitor will increment the task error count
            % and then restart the task after a backoff.
            erlang:exit(Pid, timeout);
        false ->
            ok
    end.

do_insert_task_after(SecondsOrDate, Module, Function, undefined, ArgsFun, Context) ->
    UniqueKey = z_ids:id(),
    do_insert_task_after(SecondsOrDate, Module, Function, UniqueKey, ArgsFun, Context);
do_insert_task_after(SecondsOrDate, Module, Function, UniqueKey, ArgsFun, Context) when is_function(ArgsFun) ->
    Due = to_utc_date(SecondsOrDate),
    UniqueKeyBin = z_convert:to_binary(UniqueKey),
    case z_db:transaction(
        fun(Ctx) ->
            OldTask = z_db:q_row("
                select props, due
                from pivot_task_queue
                where module = $1
                  and function = $2
                  and key = $3
                limit 1
                for update",
                [ Module, Function, UniqueKeyBin ],
                Ctx),
            New = case OldTask of
                {OldProps, OldDue} ->
                    OldArgs = get_args(OldProps),
                    ArgsFun(OldDue, OldArgs, Due, Ctx);
                undefined ->
                    ArgsFun(undefined, undefined, Due, Ctx)
            end,
            case New of
                {ok, {NewDue, NewArgs}} ->
                    case OldTask of
                        undefined -> ok;
                        {_, _} ->
                            _ = z_db:q("
                                delete from pivot_task_queue
                                where module = $1
                                  and function = $2
                                  and key = $3",
                                [ Module, Function, UniqueKeyBin ],
                                Ctx)
                    end,
                    Fields = #{
                        <<"module">> => Module,
                        <<"function">> => Function,
                        <<"key">> => UniqueKeyBin,
                        <<"args">> => NewArgs,
                        <<"due">> => NewDue
                    },
                    z_db:insert(pivot_task_queue, Fields, Ctx);
                {error, _} = Error ->
                    Error
            end
        end,
        Context)
    of
        {ok, _} = Ok ->
            z_mqtt:publish(<<"model/sysconfig/event/task">>,
                           #{
                                what => <<"insert">>,
                                module => Module,
                                function => Function,
                                due => Due
                           },
                           Context),
            Ok;
        {error, _} = Error ->
            Error
    end;
do_insert_task_after(SecondsOrDate, Module, Function, UniqueKey, Args, Context) ->
    Due = to_utc_date(SecondsOrDate),
    UniqueKeyBin = z_convert:to_binary(UniqueKey),
    case z_db:transaction(
        fun(Ctx) ->
            _ = z_db:q("
                delete from pivot_task_queue
                where module = $1
                  and function = $2
                  and key = $3",
                [ Module, Function, UniqueKeyBin ],
                Ctx),
            Fields = #{
                <<"module">> => Module,
                <<"function">> => Function,
                <<"key">> => UniqueKeyBin,
                <<"args">> => Args,
                <<"due">> => Due
            },
            z_db:insert(pivot_task_queue, Fields, Ctx)
        end,
        Context)
    of
        {ok, _} = Ok ->
            z_mqtt:publish(<<"model/sysconfig/event/task">>,
                           #{
                                what => <<"insert">>,
                                module => Module,
                                function => Function,
                                due => Due
                           },
                           Context),
            Ok;
        {error, _} = Error ->
            Error
    end.

%% @doc Insert a list of ids into the pivot queue.
do_insert_queue(Ids, DueDate, Context) when is_list(Ids) ->
    z_db:q("insert into rsc_pivot_log as p (rsc_id, due, is_update)
            select r.id, $2, true from rsc r where r.id = any($1)",
            [ Ids, DueDate ], Context),
    ok.


%% @doc Poll a database for any queued updates.
-spec do_poll( #state{} ) -> #state{}.
do_poll(State) ->
    Context = z_acl:sudo( z_context:new(State#state.site) ),
    State1 = maybe_start_task(State, Context),
    maybe_start_pivot(State1, Context).

maybe_start_pivot(#state{ pivot_pid = Pid } = State, _Context) when is_pid(Pid) ->
    State;
maybe_start_pivot(#state{ pivot_queue = Queue } = State, Context) ->
    case do_poll_queue(Context) of
        {_, undefined} ->
            State;
        {Ids, DueDate} ->
            case lists:usort(Queue ++ Ids) of
                [] ->
                    State;
                PivotIds ->
                    case z_pivot_rsc_job:start_pivot(PivotIds, Context) of
                        {ok, Pid} ->
                            erlang:monitor(process, Pid),
                            State#state{
                                pivot_pid = Pid,
                                pivot_queue = [],
                                pivot_queue_inflight = Queue,
                                pivot_queue_inflight_date = calendar:universal_time(),
                                pivot_inflight_date = DueDate,
                                pivot_ping = os:timestamp(),
                                pivot_rsc_id = undefined
                            };
                        {error, _} ->
                            % overload - ignore
                            State
                    end
            end
    end.

do_poll_queue(Context) ->
    try
        fetch_queue(Context)
    catch
        exit:{timeout, _} ->
            {[], undefined};
        throw:{error, econnrefused} ->
            {[], undefined}
    end.

maybe_start_task(#state{ task_pid = undefined } = State, Context) ->
    case poll_task(Context) of
        {ok, #{ task_id := TaskId } = Task} ->
            case z_pivot_rsc_task_job:start_task(Task, Context) of
                {ok, TaskPid} ->
                    erlang:monitor(process, TaskPid),
                    State#state{
                        task_id = TaskId,
                        task_pid = TaskPid,
                        task_ping = os:timestamp(),
                        task = Task
                    };
                {error, _} ->
                    State
            end;
        {error, enoent} ->
            State;
        {error, nodb} ->
            State;
        {error, Reason} ->
            ?LOG_ERROR(#{
                text => "Pivot could not check task queue",
                in => zotonic_core,
                reason => Reason
            }),
            State
    end;
maybe_start_task(State, _Context) ->
    State.


%% @doc Fetch the next task uit de task queue, if any.
poll_task(Context) ->
    case z_db:qmap_row("
        select id, module, function, props, error_count
        from pivot_task_queue
        where due is null
           or due < current_timestamp
        order by due asc
        limit 1",
        [],
        [ {keys, atom} ],
        Context)
    of
        {ok, #{
            id := Id,
            module := Module,
            function := Function,
            props := Props,
            error_count := ErrCt
        }} ->
            Args = get_args(Props),
            {ok, #{
                task_id => Id,
                mfa => {z_convert:to_atom(Module), z_convert:to_atom(Function), Args},
                error_count => ErrCt
            }};
        {error, _} = Error ->
            Error
    end.

get_args(Props) when is_map(Props) ->
    maps:get(<<"args">>, Props, []);
get_args(Props) when is_list(Props) ->
    % deprecated task queue entries
    proplists:get_value(args, Props, []);
get_args(undefined) ->
    [].


%% @doc Fetch the next batch of ids from the queue. Remembers the serials, as a new
%% pivot request might come in while we are pivoting.
-spec fetch_queue(Context) -> {Ids, DueDate} when
    Context :: z:context(),
    Ids :: [ m_rsc:resource_id() ],
    DueDate :: calendar:datetime().
fetch_queue(Context) ->
    PivotDate = z_db:q1("select current_timestamp - '10 second'::interval", Context),
    Rows = z_db:q("
        select rsc_id
        from rsc_pivot_log
        where due < $2
        order by is_update, due
        limit $1",
        [ ?POLL_BATCH, PivotDate ],
        Context),
    if
        Rows =:= [] ->
            {[], PivotDate};
        true ->
            % Remove log entries that have a pivot date after the cutoff date.
            % They will be pivoted at a later date.
            ToPivot = lists:foldl(
                fun({Id}, Acc) ->
                    case z_db:q_row("
                        select max(due), max(due) >= $2
                        from rsc_pivot_log
                        where rsc_id = $1
                        ", [ Id, PivotDate ], Context)
                    of
                        {_MaxDue, false} ->
                            [ Id | Acc ];
                        {MaxDue, true} ->
                            z_db:q("
                                delete from rsc_pivot_log
                                where rsc_id = $1
                                  and due < $2
                                ", [ Id, MaxDue ],
                                Context),
                            Acc
                    end
                end,
                [],
                lists:usort(Rows)),
            if
                ToPivot =:= [] ->
                    fetch_queue(Context);
                true ->
                    {ToPivot, PivotDate}
            end
        end.


%% @doc Delete pivot log entries for all pivoted ids. Use the due date
%% so that optional newer entries are still queued.
delete_queue(Ids, DueDate, Context) ->
    z_db:q("
        delete from rsc_pivot_log
        where rsc_id = any($1)
          and due < $2",
        [ Ids, DueDate ],
        Context).


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
