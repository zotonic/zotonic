%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2020 Marc Worrell, Maas-Maarten Zeeman
%% @doc Pivoting server for the rsc table. Takes care of full text indices. Polls the pivot queue for any changed resources.

%% Copyright 2009-2020 Marc Worrell, Maas-Maarten Zeeman
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
-include_lib("epgsql/include/epgsql.hrl").

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


-record(state, {
    site :: atom(),
    is_initial_delay = true :: boolean(),
    is_pivot_delay = false :: boolean(),
    backoff_counter = 0 :: integer(),
    task_pid :: undefined | pid(),
    task_id :: undefined | integer()
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
    Props = lists:foldl(
        fun(Key, Acc) ->
            case maps:is_key(Key, UpdateProps) of
                false ->
                    Acc#{ Key => maps:get(Key, RawProps, undefined) };
                true ->
                    Acc
            end
        end,
        UpdateProps,
        [ <<"date_start">>, <<"date_end">>, <<"title">> ]),
    {DateStart, DateEnd} = pivot_date(Props),
    PivotTitle = truncate(get_pivot_title(Props), 100),
    Props1 = Props#{
        <<"pivot_date_start">> => DateStart,
        <<"pivot_date_end">> => DateEnd,
        <<"pivot_date_start_month_day">> => month_day(DateStart),
        <<"pivot_date_end_month_day">> => month_day(DateEnd),
        <<"pivot_title">> => PivotTitle
    },
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
insert_task(Module, Function, Context) ->
    insert_task_after(undefined, Module, Function, undefined, [], Context).

%% @doc Insert a slow running pivot task. Use the UniqueKey to prevent double queued tasks.
insert_task(Module, Function, UniqueKey, Context) ->
    insert_task_after(undefined, Module, Function, UniqueKey, [], Context).

%% @doc Insert a slow running pivot task with unique key and arguments.
insert_task(Module, Function, UniqueKey, Args, Context) ->
    insert_task_after(undefined, Module, Function, UniqueKey, Args, Context).

%% @doc Insert a slow running pivot task with unique key and arguments that should start after Seconds seconds.
%%      Always delete any existing transaction, to prevent race conditions when the task is running
%%      during this insert.
insert_task_after(SecondsOrDate, Module, Function, undefined, ArgsFun, Context) ->
    UniqueKey = z_ids:id(),
    insert_task_after(SecondsOrDate, Module, Function, UniqueKey, ArgsFun, Context);
insert_task_after(SecondsOrDate, Module, Function, UniqueKey, ArgsFun, Context) when is_function(ArgsFun) ->
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
                    OldArgs = maps:get(<<"args">>, OldProps),
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
insert_task_after(SecondsOrDate, Module, Function, UniqueKey, Args, Context) ->
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

-spec delete_tasks( z:context() ) -> non_neg_integer().
delete_tasks(Context) ->
    z_db:q("delete from pivot_task_queue", Context).


%%====================================================================
%% API
%%====================================================================
%% @spec start_link(SiteProps) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(SiteProps) ->
    {site, Site} = proplists:lookup(site, SiteProps),
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
    lager:md([
        {site, Site},
        {module, ?MODULE}
      ]),
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
    lager:error("Pivot received unexpected 'task_done' from task job for task ~p",
                [ TaskId ]),
    {reply, {error, unknown_task, State}};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @doc Poll the queue for the default host
handle_cast(poll, #state{is_initial_delay=true} = State) ->
    {noreply, State};
handle_cast(poll, State) ->
    try
        {_IsPivoting, State1} = do_poll(State),
        {noreply, State1}
    catch
        Type:Err:Stack ->
            lager:error("Poll error ~p:~p, backing off pivoting. Stack: ~p", [ Type, Err, Stack ]),
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
    do_pivot(Id, z_context:new(State#state.site)),
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
handle_info(poll, State) ->
    try
        {IsPivoting, State1} = do_poll(State),
        case IsPivoting of
            true ->  timer:send_after(?PIVOT_POLL_INTERVAL_FAST*1000, poll);
            false -> timer:send_after(?PIVOT_POLL_INTERVAL_SLOW*1000, poll)
        end,
        {noreply, State1#state{ is_initial_delay = false }}
    catch
        Type:Err:Stack ->
            lager:error("Pivot error ~p:~p, backing off pivoting. Stack: ~p", [ Type, Err, Stack ]),
            timer:send_after(?PIVOT_POLL_INTERVAL_SLOW*1000, poll),
            {noreply, State#state{ backoff_counter = ?BACKOFF_POLL_ERROR }}
    end;

handle_info({'DOWN', _MRef, process, _Pid, _Reason}, #state{ task_pid = undefined } = State) ->
    {noreply, State};
handle_info({'DOWN', _MRef, process, Pid, Reason}, #state{ task_pid = Pid } = State) ->
    lager:error("Pivot received unexpected DOWN with reason '~p' from task job for task ~p",
                [ Reason, State#state.task_id ]),
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
            lager:error("pivot: rollback during pivot queue insert: ~p", [Reason]),
            timer:apply_after(100, ?MODULE, insert_queue, [Ids, DueDate, Context]);
        {error, Reason} ->
            lager:error("pivot: error during pivot queue insert: ~p", [Reason])
    end.


%% @doc Poll a database for any queued updates.
do_poll(State) ->
    Context = z_acl:sudo( z_context:new(State#state.site) ),
    State1 = maybe_start_task(State, Context),
    try
        {do_poll_queue(Context) orelse is_pid(State1#state.task_pid), State1}
    catch
        exit:{timeout, _} ->
            {false, State1};
        throw:{error, econnrefused} ->
            {false, State1}
    end.

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
        {error, _} = Error ->
            lager:error("Pivot could not check task queue: ~p", [ Error ]),
            State
    end;
maybe_start_task(State, _Context) ->
    State.


%% @doc Fetch the next task uit de task queue, if any.
poll_task(Context) ->
    case z_db:qmap_row("
        select id, module, function, props
        from pivot_task_queue
        where due is null
           or due < current_timestamp
        order by due asc
        limit 1",
        [],
        [ {keys, atom} ],
        Context)
    of
        {ok, #{ id := Id, module := Module, function := Function, props := Props }} ->
            Args = get_args(Props),
            ErrCt = get_error_ct(Props),
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

get_error_ct(Props) when is_map(Props) ->
    maps:get(<<"error_ct">>, Props, 0);
get_error_ct(Props) when is_list(Props) ->
    % deprecated task queue entries
    proplists:get_value(error_ct, Props, 0);
get_error_ct(_) ->
    0.


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


-spec pivot_resource(integer(), #context{}) -> ok | {error, eexist}.
pivot_resource(Id, Context0) ->
    Lang = stemmer_language_config(Context0),
    Context = z_context:set_language(Lang,
                 z_context:set_tz(<<"UTC">>,
                    z_acl:sudo(Context0))),
    case m_rsc:exists(Id, Context) of
        true ->
            RscProps = get_pivot_rsc(Id, Context),
            Vars = #{
                id => Id,
                props => RscProps,
                z_language => Lang
            },
            case z_template_compiler_runtime:map_template({cat, <<"pivot/pivot.tpl">>}, Vars, Context) of
                {ok, Template} ->
                    TextA = render_block(a, Template, Vars, Context),
                    TextB = render_block(b, Template, Vars, Context),
                    TextC = render_block(c, Template, Vars, Context),
                    TextD = render_block(d, Template, Vars, Context),
                    TsvIds = render_block(related_ids, Template, Vars, Context),
                    Title = render_block(title, Template, Vars, Context),
                    Street = render_block(address_street, Template, Vars, Context),
                    City = render_block(address_city, Template, Vars, Context),
                    Postcode = render_block(address_postcode, Template, Vars, Context),
                    State = render_block(address_state, Template, Vars, Context),
                    Country = render_block(address_country, Template, Vars, Context),
                    NameFirst = render_block(name_first, Template, Vars, Context),
                    NameSurname = render_block(name_surname, Template, Vars, Context),
                    Gender = render_block(gender, Template, Vars, Context),
                    DateStart = to_datetime(render_block(date_start, Template, Vars, Context)),
                    DateEnd = to_datetime(render_block(date_end, Template, Vars, Context)),
                    DateStartMonthDay = to_integer(render_block(date_start_month_day, Template, Vars, Context)),
                    DateEndMonthDay = to_integer(render_block(date_end_month_day, Template, Vars, Context)),
                    LocationLat = to_float(render_block(location_lat, Template, Vars, Context)),
                    LocationLng = to_float(render_block(location_lng, Template, Vars, Context)),

                    % Make psql tsv texts from the A..D blocks
                    StemmerLanguage = stemmer_language(Context),
                    {SqlA, ArgsA} = to_tsv(TextA, $A, [], StemmerLanguage),
                    {SqlB, ArgsB} = to_tsv(TextB, $B, ArgsA, StemmerLanguage),
                    {SqlC, ArgsC} = to_tsv(TextC, $C, ArgsB, StemmerLanguage),
                    {SqlD, ArgsD} = to_tsv(TextD, $D, ArgsC, StemmerLanguage),

                    % Make the text and object-ids vectors for the pivot
                    TsvSql = [SqlA, " || ", SqlB, " || ", SqlC, " || ", SqlD],
                    Tsv  = z_db:q1(iolist_to_binary(["select ", TsvSql]), ArgsD, Context),
                    Rtsv = z_db:q1("select to_tsvector($1)", [TsvIds], Context),

                    KVs = #{
                        <<"pivot_tsv">> => Tsv,
                        <<"pivot_rtsv">> =>  Rtsv,
                        <<"pivot_street">> => truncate(Street, 120),
                        <<"pivot_city">> => truncate(City, 100),
                        <<"pivot_postcode">> => truncate(Postcode, 30),
                        <<"pivot_state">> => truncate(State, 50),
                        <<"pivot_country">> => truncate(Country, 80),
                        <<"pivot_first_name">> => truncate(NameFirst, 100),
                        <<"pivot_surname">> => truncate(NameSurname, 100),
                        <<"pivot_gender">> => truncate(Gender, 1),
                        <<"pivot_date_start">> => DateStart,
                        <<"pivot_date_end">> => DateEnd,
                        <<"pivot_date_start_month_day">> => DateStartMonthDay,
                        <<"pivot_date_end_month_day">> => DateEndMonthDay,
                        <<"pivot_title">> => truncate(Title, 100),
                        <<"pivot_location_lat">> => LocationLat,
                        <<"pivot_location_lng">> => LocationLng
                    },
                    KVs1 = z_notifier:foldr(#pivot_fields{id=Id, raw_props=RscProps}, KVs, Context),
                    update_changed(Id, KVs1, RscProps, Context),
                    pivot_resource_custom(Id, Context),

                    case to_datetime(render_block(date_repivot, Template, Vars, Context)) of
                        undefined -> ok;
                        DateRepivot -> insert_queue(Id, DateRepivot, Context)
                    end,
                    ok;
                {error, enoent} ->
                    lager:error("[~p] Missing 'pivot/pivot.tpl' template", [z_context:site(Context)]),
                    ok
            end;
        false ->
            {error, eexist}
    end.

render_block(Block, Template, Vars, Context) ->
    {Output, _RenderState} = z_template:render_block_to_iolist(Block, Template, Vars, Context),
    iolist_to_binary(Output).

%% @doc Check which pivot fields are changed, update only those
update_changed(Id, KVs, RscProps, Context) ->
    KVsChanged = maps:filter(
        fun
            (K, V) when is_list(V) ->
                maps:get(K, RscProps, undefined) =/= iolist_to_binary(V);
            (K, undefined) ->
                maps:get(K, RscProps, undefined) =/= undefined;
            (K, V) when is_atom(V) ->
                maps:get(K, RscProps, undefined) =/= z_convert:to_binary(V);
            (K, V) ->
                maps:get(K, RscProps, undefined) =/= V
        end,
        KVs),
    case maps:size(KVsChanged) of
        0 ->
            ok;
        _ ->
            % Make Sql update statement for the changed fields
            {Sql, Args} = maps:fold(
                    fun(K, V, {Sq,As}) ->
                        {[  Sq,
                            case As of [] -> []; _ -> $, end,
                            z_convert:to_list(K), " = $", integer_to_list(length(As)+1)
                        ],
                        [V|As]}
                    end,
                    {"update rsc set ",[]},
                    KVsChanged),

            z_db:q1(iolist_to_binary([Sql, " where id = $", integer_to_list(length(Args)+1)]),
                    lists:reverse([Id|Args]),
                    Context)
    end.


pivot_resource_custom(Id, Context) ->
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



to_datetime(Text) ->
    case z_string:trim(Text) of
        <<>> -> undefined;
        Text1 -> check_datetime(z_datetime:to_datetime(Text1))
    end.

check_datetime({{Y,M,D},{H,I,S}} = Date)
    when is_integer(Y), is_integer(M), is_integer(D),
         is_integer(H), is_integer(I), is_integer(S) ->
    Date;
% check_datetime({Y,M,D} = Date)
%     when is_integer(Y), is_integer(M), is_integer(D) ->
%     {Date, {0,0,0}};
check_datetime(_) ->
    undefined.


%% Make the setweight(to_tsvector()) parts of the update statement
to_tsv(Text, Level, Args, StemmingLanguage) when is_binary(Text) ->
    case cleanup_tsv_text(z_html:unescape(z_html:strip(Text))) of
        <<>> ->
            {"tsvector('')", Args};
        TsvText ->
            N = length(Args) + 1,
            Truncated = z_string:truncate(TsvText, ?MAX_TSV_LEN, <<>>),
            Args1 = Args ++ [Truncated],
            {["setweight(to_tsvector('pg_catalog.",StemmingLanguage,"', $",integer_to_list(N),"), '",Level,"')"], Args1}
    end.

-spec to_float(binary()) -> float() | undefined.
to_float(Text) ->
    case z_string:trim(Text) of
        <<>> -> undefined;
        Text1 -> z_convert:to_float(Text1)
    end.

-spec to_integer(binary()) -> integer() | undefined.
to_integer(Text) ->
    case z_string:trim(Text) of
        <<>> -> undefined;
        Text1 -> z_convert:to_integer(Text1)
    end.

-spec cleanup_tsv_text(binary()) -> binary().
cleanup_tsv_text(Text) when is_binary(Text) ->
    Text1 = z_string:sanitize_utf8(Text),
    Text2 = iolist_to_binary(re:replace(Text1, <<"[ ",13,10,9,"/-]+">>, <<" ">>, [global])),
    z_string:trim(Text2).

-spec truncate(binary(), integer()) -> binary().
truncate(S, Len) ->
    iolist_to_binary(
        z_string:trim(
            z_string:to_lower(
                truncate_1(S, Len, Len)))).

truncate_1(_S, 0, _Bytes) ->
    <<>>;
truncate_1(S, Utf8Len, Bytes) ->
    case z_string:truncate(S, Utf8Len, <<>>) of
        T when size(T) > Bytes -> truncate_1(T, Utf8Len-1, Bytes);
        L -> L
    end.


%% @doc Fetch the date range from the record
pivot_date(R) ->
    DateStart = z_datetime:undefined_if_invalid_date(maps:get(<<"date_start">>, R, undefined)),
    DateEnd   = z_datetime:undefined_if_invalid_date(maps:get(<<"date_end">>, R, undefined)),
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
    z_string:to_lower(get_pivot_title(#{ <<"title">> => m_rsc:p(Id, <<"title">>, Context) })).

get_pivot_title(Props) ->
    case maps:get(<<"title">>, Props, <<>>) of
        #trans{ tr = [] } ->
            <<>>;
        #trans{ tr = [{_, Text}|_] } ->
            z_string:to_lower(Text);
        T ->
            z_string:to_lower(T)
    end.


%% @doc Return the raw resource data for the pivoter
get_pivot_rsc(Id, Context) ->
    case z_db:qmap_props_row(
        "select * from rsc where id = $1",
        [ Id ],
        [ {keys, binary} ],
        Context)
    of
        {ok, FullRecord} ->
            z_notifier:foldl(#pivot_rsc_data{ id = Id }, FullRecord, Context);
        {error, _} ->
            undefined
    end.


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
pg_lang_extra(LangCode) ->
    case z_language:english_name(z_convert:to_atom(LangCode)) of
        undefined ->
            pg_lang(LangCode);
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
        true -> pg_lang(z_language:default_language(Context));
        false -> pg_lang_extra(StemmingLanguage)
    end.

-spec stemmer_language_config(#context{}) -> atom().
stemmer_language_config(Context) ->
    StemmingLanguage = m_config:get_value(i18n, language_stemmer, Context),
    case z_utils:is_empty(StemmingLanguage) of
        true ->
            z_language:default_language(Context);
        false ->
            case z_language:to_language_atom(StemmingLanguage) of
                {ok, LangAtom} -> LangAtom;
                {error, not_a_language} -> z_language:default_language(Context)
            end
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
        {ok, _Row}  ->
            z_db:update(TableName, Id, Columns, Context);
        {error, enoent} ->
            z_db:insert(TableName, [ {id, Id} | Columns ], Context)
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
