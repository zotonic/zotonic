%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2022 Marc Worrell, Maas-Maarten Zeeman
%% @doc Pivoting server for the rsc table. Takes care of full text indices. Polls the pivot queue for any changed resources.

%% Copyright 2009-2022 Marc Worrell, Maas-Maarten Zeeman
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

    stemmer_language/1,
    stemmer_language_config/1,
    cleanup_tsv_text/1,
    pg_lang/1,
    pg_lang_extra/1,
    % get_pivot_data/2,

    define_custom_pivot/3,
    lookup_custom_pivot/4
]).

-include("zotonic.hrl").
% -include_lib("epgsql/include/epgsql.hrl").

% Interval (in seconds) to check if there are any items to be pivoted.
-define(PIVOT_POLL_INTERVAL_FAST, 2).
-define(PIVOT_POLL_INTERVAL_SLOW, 20).

% How many PIVOT_POLL_INTERVAL_SLOW we will skip on SQL errors (like timeouts)
-define(BACKOFF_POLL_ERROR, 4).

% Number of queued ids taken from the queue at one go
-define(POLL_BATCH, 50).

-record(state, {
    site :: atom(),
    is_initial_delay = true :: boolean(),
    is_pivot_delay = false :: boolean(),
    backoff_counter = 0 :: integer(),
    task_pid :: undefined | pid(),
    task_id :: undefined | integer(),
    pivot_pid :: undefined | pid()
}).


%% @doc Poll the pivot queue for the database in the context
-spec poll( z:context() ) -> ok.
poll(Context) ->
    gen_server:cast(Context#context.pivot_server, poll).


-spec status( z:context() ) -> {ok, map()} | {error, term()}.
status(Context) ->
    gen_server:call(Context#context.pivot_server, status).

%% @doc An immediate pivot request for a resource
-spec pivot(integer(), z:context()) -> ok.
pivot(Id, Context) ->
    gen_server:cast(Context#context.pivot_server, {pivot, Id}).

%% @doc Delay the next pivot, useful when performing big updates
-spec pivot_delay(z:context()) -> ok.
pivot_delay(Context) ->
    gen_server:cast(Context#context.pivot_server, pivot_delay).


%% @doc Return a modified property list with fields that need immediate pivoting on an update.
pivot_resource_update(Id, UpdateProps, RawProps, Context) ->
    z_pivot_rsc_job:pivot_resource_update(Id, UpdateProps, RawProps, Context).

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
            do_insert_queue(Ids, calendar:universal_time(), Context),
            queue_all(lists:last(Ids), Context)
    end.


%% @doc Return the length of the pivot queue.
-spec queue_count(z:context()) -> integer().
queue_count(Context) ->
    z_db:q1("SELECT COUNT(*) FROM rsc_pivot_queue", Context).

%% @doc Insert a rsc_id in the pivot queue
-spec insert_queue(m_rsc:resource_id() | list(m_rsc:resource_id()), z:context()) -> ok | {error, eexist}.
insert_queue(IdorIds, Context) ->
    insert_queue(IdorIds, calendar:universal_time(), Context).

%% @doc Insert a rsc_id in the pivot queue for a certain date
-spec insert_queue(m_rsc:resource_id() | list(m_rsc:resource_id()), calendar:datetime(), z:context()) -> ok | {error, eexist}.
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

%% @doc Insert a slow running pivot task. Use the UniqueKey to prevent double queued tasks.
-spec insert_task(Module, Function, UniqueKey, Context) -> {ok, TaskId} | {error, term()}
    when Module :: atom(),
         Function :: atom(),
         UniqueKey :: undefined | binary() | string() | atom(),
         Context :: z:context(),
         TaskId :: integer().
insert_task(Module, Function, UniqueKey, Context) ->
    insert_task_after(undefined, Module, Function, UniqueKey, [], Context).

%% @doc Insert a slow running pivot task with unique key and arguments.
-spec insert_task(Module, Function, UniqueKey, Args, Context) -> {ok, TaskId} | {error, term()}
    when Module :: atom(),
         Function :: atom(),
         UniqueKey :: undefined | binary() | string() | atom(),
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
         UniqueKey :: undefined | binary() | string() | atom(),
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

-spec get_task( module(), atom(), binary()|string(), z:context() ) -> {ok, map()} | {error, term()}.
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
    z_db:q("delete from pivot_task_queue where module = $1 and function = $2",
           [Module, Function],
           Context).

-spec delete_task( module(), atom(), term(), z:context() ) -> non_neg_integer().
delete_task(Module, Function, UniqueKey, Context) ->
    UniqueKeyBin = z_convert:to_binary(UniqueKey),
    z_db:q("delete from pivot_task_queue where module = $1 and function = $2 and key = $3",
           [Module, Function, UniqueKeyBin],
           Context).

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
    z_db:q("delete from pivot_task_queue", Context).

-spec get_pivot_title( m_rsc:resource_id(), z:context() ) -> binary().
get_pivot_title(Id, Context) ->
    z_pivot_rsc_job:get_pivot_title(Id, Context).

-spec get_pivot_title( map() ) -> binary().
get_pivot_title(Props) ->
    z_pivot_rsc_job:get_pivot_title(Props).


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
    logger:set_process_metadata(#{
        site => Site,
        module => ?MODULE
    }),
    timer:send_after(?PIVOT_POLL_INTERVAL_SLOW*1000, poll),
    {ok, #state{
        site=Site,
        is_initial_delay=true,
        is_pivot_delay=false,
        task_pid = undefined,
        task_id = undefined
    }}.


%% @doc Handle task_done messages from task queue jobs
handle_call({task_done, TaskId, _TaskPid}, _From, #state{ task_id = TaskId } = State) ->
    {reply, ok, State#state{ task_id = undefined, task_pid = undefined }};
handle_call({task_done, TaskId, _TaskPid}, _From, State) ->
    ?LOG_ERROR(#{
        text => <<"Pivot received 'task_done' from unknown task job">>,
        task_id => TaskId
    }),
    {reply, {error, unknown_task, State}};

handle_call({pivot_done, PivotPid}, _From, #state{ pivot_pid = PivotPid } = State) ->
    {reply, ok, State#state{ pivot_pid = undefined }};
handle_call({pivot_done, PivotPid}, _From, State) ->
    ?LOG_ERROR(#{
        text => <<"Pivot received 'pivot_done' from unknown pivot job">>,
        pid => PivotPid
    }),
    {reply, {error, unknown_pivot, State}};

handle_call({insert_task_after, SecondsOrDate, Module, Function, UniqueKey, ArgsFun}, _From, State) ->
    Context = z_context:new(State#state.site),
    Result = do_insert_task_after(SecondsOrDate, Module, Function, UniqueKey, ArgsFun, Context),
    {reply, Result, State};

handle_call(status, _From, State) ->
    Status = #{
        site => State#state.site,
        is_initial_delay => State#state.is_initial_delay,
        is_pivot_delay => State#state.is_pivot_delay,
        task_pid => State#state.task_pid,
        task_id => State#state.task_id
    },
    {reply, {ok, Status}, State};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @doc Poll the queue for the default host
handle_cast(poll, #state{is_initial_delay=true} = State) ->
    {noreply, State};
handle_cast(poll, State) ->
    try
        State1 = do_poll(State),
        {noreply, State1}
    catch
        Type:Err:Stack ->
            ?LOG_ERROR(#{
                text => <<"Pivot poll error">>,
                type => Type,
                error => Err,
                stack => Stack
            }),
            {noreply, State#state{ backoff_counter = ?BACKOFF_POLL_ERROR }}
    end;

%% @doc Insert an id into the queue.
handle_cast({insert_queue, DueDate, Ids}, State) when is_list(Ids) ->
    do_insert_queue(Ids, DueDate, z_context:new(State#state.site)),
    z_utils:flush_message({'$gen_cast', {insert_queue, DueDate, Ids}}),
    {noreply, State};

%% @doc Poll the queue for a particular database
handle_cast({pivot, Id}, #state{ is_initial_delay = true } = State) when is_integer(Id) ->
    Due = z_datetime:next_minute(calendar:universal_time()),
    do_insert_queue([ Id ], Due, z_context:new(State#state.site)),
    {noreply, State};
handle_cast({pivot, Id}, #state{ backoff_counter = Ct } = State) when Ct > 0 ->
    Due = z_datetime:next_minute(calendar:universal_time()),
    do_insert_queue([ Id ], Due, z_context:new(State#state.site)),
    {noreply, State};
handle_cast({pivot, Id}, State) when is_integer(Id) ->
    do_pivot(Id, z_acl:sudo(z_context:new(State#state.site))),
    {noreply, State};

%% @doc Delay the next pivot, useful when performing big updates
handle_cast(pivot_delay, State) ->
    {noreply, State#state{is_pivot_delay=true}};
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @doc Handling all non call/cast messages
handle_info(poll, #state{ is_pivot_delay = true } = State) ->
    timer:send_after(?PIVOT_POLL_INTERVAL_SLOW*1000, poll),
    {noreply, State#state{ is_pivot_delay = false}};
handle_info(poll, #state{backoff_counter = Ct} = State) when Ct > 0 ->
    timer:send_after(?PIVOT_POLL_INTERVAL_SLOW*1000, poll),
    {noreply, State#state{ backoff_counter = Ct - 1 }};
handle_info(poll, #state{ pivot_pid = Pid } = State) when is_pid(Pid) ->
    ?LOG_INFO(#{
        text => <<"Pivot poll when other pivot still running">>,
        pivot_pid => Pid,
        reason => busy
    }),
    timer:send_after(?PIVOT_POLL_INTERVAL_SLOW*1000, poll),
    {noreply, State};
handle_info(poll, #state{ site = Site } = State) ->
    case z_sites_manager:get_site_status(Site) of
        {ok, running} ->
            ?LOG_DEBUG(#{
                text => <<"Pivot poll">>
            }),
            try
                State1 = do_poll(State),
                IsPivoting = is_pid(State1#state.pivot_pid)
                        orelse is_pid(State1#state.task_pid),
                case IsPivoting of
                    true ->  timer:send_after(?PIVOT_POLL_INTERVAL_FAST*1000, poll);
                    false -> timer:send_after(?PIVOT_POLL_INTERVAL_SLOW*1000, poll)
                end,
                {noreply, State1#state{ is_initial_delay = false }}
            catch
                Type:Err:Stack ->
                    ?LOG_ERROR(#{
                        text => <<"Pivot error">>,
                        type => Type,
                        error => Err,
                        stack => Stack
                    }),
                    timer:send_after(?PIVOT_POLL_INTERVAL_SLOW*1000, poll),
                    {noreply, State#state{ backoff_counter = ?BACKOFF_POLL_ERROR }}
            end;
        _ ->
            timer:send_after(?PIVOT_POLL_INTERVAL_SLOW*1000, poll),
            {noreply, State#state{ is_initial_delay = true }}
    end;

handle_info({'DOWN', _MRef, process, _Pid, _Reason}, #state{ pivot_pid = undefined, task_pid = undefined } = State) ->
    {noreply, State};

handle_info({'DOWN', _MRef, process, Pid, Reason}, #state{ pivot_pid = Pid } = State) ->
    ?LOG_ERROR(#{
        text=> <<"Pivot received unexpected DOWN from pivot job">>,
        reason => Reason
    }),
    {noreply, State#state{ pivot_pid = undefined, backoff_counter = ?BACKOFF_POLL_ERROR }};

handle_info({'DOWN', _MRef, process, Pid, Reason}, #state{ task_pid = Pid } = State) ->
    ?LOG_ERROR(#{
        text => <<"Pivot received unexpected DOWN from task job">>,
        reason => Reason,
        task_id => State#state.task_id
    }),
    {noreply, State#state{ task_id = undefined, task_pid = undefined }};


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

do_insert_task_after(SecondsOrDate, Module, Function, undefined, ArgsFun, Context) ->
    UniqueKey = z_ids:id(),
    do_insert_task_after(SecondsOrDate, Module, Function, UniqueKey, ArgsFun, Context);
do_insert_task_after(SecondsOrDate, Module, Function, UniqueKey, ArgsFun, Context) when is_function(ArgsFun) ->
    Due = to_utc_date(SecondsOrDate),
    UniqueKeyBin = z_convert:to_binary(UniqueKey),
    z_db:transaction(
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
        Context);
do_insert_task_after(SecondsOrDate, Module, Function, UniqueKey, Args, Context) ->
    Due = to_utc_date(SecondsOrDate),
    UniqueKeyBin = z_convert:to_binary(UniqueKey),
    z_db:transaction(
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
        Context).


%% @doc Insert a list of ids into the pivot queue.
do_insert_queue(Ids, DueDate, Context) when is_list(Ids) ->
    F = fun(Ctx) ->
        z_db:q("lock table rsc_pivot_queue in share row exclusive mode", Ctx),
        lists:foreach(
            fun(Id) ->
                case z_db:q1("select id from rsc where id = $1", [Id], Ctx) of
                    Id ->
                        case z_db:q("
                            update rsc_pivot_queue
                            set serial = serial + 1,
                                due = $2
                            where rsc_id = $1",
                            [ Id, DueDate ],
                            Ctx)
                        of
                            1 -> ok;
                            0 ->
                                z_db:q("
                                    insert into rsc_pivot_queue (rsc_id, due, is_update)
                                    select id, $2, true from rsc where id = $1",
                                    [ Id, DueDate ], Ctx)
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
            ?LOG_ERROR(#{
                text => <<"Rollback during pivot queue insert">>,
                reason => Reason
            }),
            timer:apply_after(100, ?MODULE, insert_queue, [Ids, DueDate, Context]);
        {error, Reason} ->
            ?LOG_ERROR(#{
                text => <<"Error during pivot queue insert">>,
                reason => Reason
            })
    end.


%% @doc Poll a database for any queued updates.
-spec do_poll( #state{} ) -> #state{}.
do_poll(State) ->
    Context = z_acl:sudo( z_context:new(State#state.site) ),
    State1 = maybe_start_task(State, Context),
    maybe_start_pivot(State1, Context).

maybe_start_pivot(#state{ pivot_pid = Pid } = State, _Context) when is_pid(Pid) ->
    State;
maybe_start_pivot(State, Context) ->
    case do_poll_queue(Context) of
        [] ->
            State;
        Queue ->
            case z_pivot_rsc_job:start_pivot(self(), Queue, Context) of
                {ok, Pid} ->
                    erlang:monitor(process, Pid),
                    State#state{ pivot_pid = Pid };
                {error, _} ->
                    % overload - ignore
                    State
            end
    end.

do_poll_queue(Context) ->
    try
        fetch_queue(Context)
    catch
        exit:{timeout, _} ->
            [];
        throw:{error, econnrefused} ->
            []
    end.

%% @doc Pivot a specific id, delete its queue record if present
do_pivot(Id, Context) ->
    OptSerial = fetch_queue_id(Id, Context),
    z_pivot_rsc_job:pivot_job(self(), [ {Id, OptSerial} ], Context).


maybe_start_task(#state{ task_pid = undefined } = State, Context) ->
    case poll_task(Context) of
        {ok, #{ task_id := TaskId } = Task} ->
            case z_pivot_rsc_task_job:start_task(self(), Task, Context) of
                {ok, TaskPid} ->
                    erlang:monitor(process, TaskPid),
                    State#state{ task_id = TaskId, task_pid = TaskPid };
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
-spec fetch_queue( z:context() ) -> [ { Id::m_rsc:resource_id(), Serial::integer() }].
fetch_queue(Context) ->
    z_db:q("
        select rsc_id, serial
        from rsc_pivot_queue
        where due < current_timestamp - '10 second'::interval
        order by is_update, due
        limit $1", [?POLL_BATCH], Context).

%% @doc Fetch the serial of the id's queue record
-spec fetch_queue_id( m_rsc:resource_id(), z:context() ) -> Serial::integer() | undefined.
fetch_queue_id(Id, Context) ->
    z_db:q1("select serial from rsc_pivot_queue where rsc_id = $1", [Id], Context).



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
