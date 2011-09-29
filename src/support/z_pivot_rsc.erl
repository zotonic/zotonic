%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-17
%% @doc Pivoting server for the rsc table. Takes care of full text indices. Polls the pivot queue for any changed resources.

%% Copyright 2009 Marc Worrell
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
    pivot_resource_update/2,
    queue_all/1,

    get_pivot_title/1,
    get_pivot_title/2,

    insert_task/3,
    insert_task/4,
    insert_task/5,
    
    insert_task_after/6,
    
    pivot_resource/2,
    pg_lang/1,
    get_pivot_data/2,

    define_custom_pivot/3,
    lookup_custom_pivot/4
]).

-include("zotonic.hrl").

% Interval (in seconds) to check if there are any items to be pivoted.
-define(PIVOT_POLL_INTERVAL, 10).

% Number of queued ids taken from the queue at one go
-define(POLL_BATCH, 100).

%% Minimum day, inserted for date start search ranges
-define(EPOCH_START, {{-4000,1,1},{0,0,0}}).


-record(state, {context, timer}).


%% @doc Poll the pivot queue for the database in the context
%% @spec poll(Context) -> void()
poll(Context) ->
    gen_server:cast(Context#context.pivot_server, poll).


%% @doc An immediate pivot request for a resource
%% @spec pivot(Id, Context) -> void()
pivot(Id, Context) ->
    gen_server:cast(Context#context.pivot_server, {pivot, Id}).


%% @doc Return a modified property list with fields that need immediate pivoting on an update.
pivot_resource_update(UpdateProps, RawProps) ->
    Props = lists:foldl(fun(Key, All) ->
                                case proplists:is_defined(Key, UpdateProps) of
                                    false ->
                                        [{Key, proplists:get_value(Key, RawProps)}|All];
                                    true ->
                                        All
                                end
                        end, UpdateProps, [date_start, date_end]),

    {DateStart, DateEnd} = pivot_date(Props),
    PivotTitle = truncate(get_pivot_title(Props), 100),
    [
        {pivot_date_start, DateStart},
        {pivot_date_end, DateEnd},
        {pivot_date_start_month_day, month_day(DateStart)},
        {pivot_date_end_month_day, month_day(DateEnd)},
        {pivot_title, PivotTitle}
        | Props
    ].

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
            Ids ->
                F = fun(Ctx) ->
                        [ insert_queue(Id, Ctx) || {Id} <- Ids ]
                    end,
                z_db:transaction(F, Context),
                {LastId} = lists:last(Ids),
                queue_all(LastId, Context)
        end.
                
%% @doc Insert a rsc_id in the pivot queue
insert_queue(Id, Context) ->
    case z_db:q("update rsc_pivot_queue 
                 set serial = serial + 1
                 where rsc_id = $1", [Id], Context) of
        1 -> ok;
        0 -> z_db:q("insert into rsc_pivot_queue (rsc_id, due, is_update) values ($1, now(), true)", [Id], Context)
    end.
    

%% @doc Insert a slow running pivot task. For example syncing category numbers after an category update.
insert_task(Module, Function, Context) ->
    insert_task(Module, Function, z_ids:id(), [], Context).

%% @doc Insert a slow running pivot task. Use the UniqueKey to prevent double queued tasks.
insert_task(Module, Function, UniqueKey, Context) ->
    insert_task(Module, Function, UniqueKey, [], Context).
    
%% @doc Insert a slow running pivot task with unique key and arguments.
insert_task(Module, Function, UniqueKey, Args, Context) ->
    insert_task_after(undefined, Module, Function, UniqueKey, Args, Context).

%% @doc Insert a slow running pivot task with unique key and arguments that should start after Seconds seconds.
insert_task_after(Seconds, Module, Function, UniqueKey, Args, Context) ->
    z_db:transaction(fun(Ctx) -> insert_transaction(Seconds, Module, Function, UniqueKey, Args, Ctx) end, Context).

    insert_transaction(Seconds, Module, Function, UniqueKey, Args, Context) ->
        Due = case Seconds of 
                undefined -> undefined; 
                _ -> calendar:gregorian_seconds_to_datetime(
                        calendar:datetime_to_gregorian_seconds(calendar:local_time()) + Seconds)
              end,
        Fields = [
            {module, Module},
            {function, Function},
            {key, UniqueKey},
            {args, Args},
            {due, Due}
        ],
        case z_db:q1("select id 
                      from pivot_task_queue 
                      where module = $1 and function = $2 and key = $3", 
                    [Module, Function, UniqueKey], Context) of
            undefined -> 
                z_db:insert(pivot_task_queue, Fields, Context);
            Id when is_integer(Id) -> 
                case Due of
                    undefined -> 
                        nop;
                    _ ->
                        z_db:q("update pivot_task_queue 
                                set due = $1 
                                where id = $2", 
                               [Due, Id], Context)
                end,
                {ok, Id}
        end.


count_queue(Context) ->
    z_db:q1("select count(*) from rsc_pivot_queue", Context).

count_task(Context) ->
    z_db:q1("select count(*) from pivot_task_queue", Context).


%%====================================================================
%% API
%%====================================================================
%% @spec start_link(SiteProps) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(SiteProps) ->
    {host, Host} = proplists:lookup(host, SiteProps),
    Name = z_utils:name_for_host(?MODULE, Host),
    gen_server:start_link({local, Name}, ?MODULE, SiteProps, []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(SiteProps) ->
    Context = z_context:new(proplists:get_value(host, SiteProps)),
    Timer = timer:apply_interval(?PIVOT_POLL_INTERVAL * 1000, ?MODULE, poll, [Context]),
    {ok, #state{timer=Timer, context=Context}}.


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
handle_cast(poll, State) ->
    flush(),
	Context1 = z_trans_server:set_context_table(State#state.context),
    do_poll(Context1),
    {noreply, State#state{context=Context1}};


%% @doc Poll the queue for a particular database
handle_cast({pivot, Id}, State) ->
	Context1 = z_trans_server:set_context_table(State#state.context),
    do_pivot(Id, Context1),
    {noreply, State#state{context=Context1}};


%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.



%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    timer:cancel(State#state.timer),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

%% @doc Poll a database for any queued updates.
do_poll(Context) ->
    % Perform some queued tasks
    case poll_task(Context) of
        {TaskId, Module, Function, _Key, Args} -> 
            try
                ?zInfo(io_lib:format("Executing task: ~p:~p( ~p )", [Module, Function, Args]), Context),
                case erlang:apply(Module, Function, z_convert:to_list(Args) ++ [Context]) of
                    {delay, Seconds} ->
                        Due = calendar:gregorian_seconds_to_datetime(
                                calendar:datetime_to_gregorian_seconds(calendar:local_time()) + Seconds
                              ),
                        z_db:q("update pivot_task_queue set due = $1 where id = $2", [Due, TaskId], Context);
                    _OK ->
                        z_db:q("delete from pivot_task_queue where id = $1", [TaskId], Context)
                end
            catch
                error:undef -> 
                    ?zWarning(io_lib:format("Undefined task, aborting: ~p:~p(~p)~n", [Module, Function, Args]), Context),
                    z_db:q("delete from pivot_task_queue where id = $1", [TaskId], Context);
                Error:Reason -> 
                    ?zWarning(io_lib:format("Task failed(~p:~p): ~p:~p(~p)~n", [Error, Reason, Module, Function, Args]), Context)
            end,
            case count_task(Context) of
                0 -> nop;
                N1 -> ?zInfo(io_lib:format("Task queue size: ~p", [N1]), Context)
            end;
        empty ->
            nop
    end,
    % Pivot some resources
    case fetch_queue(Context) of
        [] -> ok;
        Qs ->
            F = fun(Ctx) ->
                        [ {Id, catch pivot_resource(Id, Ctx)} || {Id,_Serial} <- Qs]
                end,
            case z_db:transaction(F, Context) of
                {rollback, PivotError} -> ?ERROR("Pivot error: ~p: ~p~n", [PivotError, Qs]);
                L when is_list(L) -> 
                    lists:map(fun({_Id, ok}) -> ok; 
                                 ({Id,Error}) -> log_error(Id, Error, Context) end, 
                              L),
                    delete_queue(Qs, Context)
            end,
            case count_queue(Context) of
                0 -> nop;
                N2 -> ?zInfo(io_lib:format("Pivot queue size: ~p", [N2]), Context)
            end
    end.

    log_error(Id, Error, Context) ->
        ?zWarning(io_lib:format("Pivot error ~p: ~p", [Id, Error]), Context).

    %% @doc Fetch the next task uit de task queue, if any.
    poll_task(Context) ->
        case z_db:q_row("select id, module, function, key, props 
                         from pivot_task_queue 
                         where due is null
                            or due < now()
                         order by id asc 
                         limit 1", Context) 
        of
            {Id,Module,Function,Key,Props} ->
                Args = case Props of
                    [{args,Args0}] -> Args0;
                    _ -> []
                end,
                %% @todo We delete the task right now, this needs to be changed to a deletion after the task has been running.
                {Id, z_convert:to_atom(Module), z_convert:to_atom(Function), Key, Args};
            undefined ->
                empty
        end.
    

%% @doc Pivot a specific id, delete its queue record if present
do_pivot(Id, Context) ->
    Serial = fetch_queue_id(Id, Context),
    pivot_resource(Id, Context),
    delete_queue(Id, Serial, Context).


%% @doc Fetch the next batch of ids from the queue. Remembers the serials, as a new
%% pivot request might come in while we are pivoting.
%% @spec fetch_queue(Context) -> [{Id,Serial}]
fetch_queue(Context) ->
    z_db:q("select rsc_id, serial from rsc_pivot_queue where due < now() - '10 second'::interval order by is_update, due limit $1", [?POLL_BATCH], Context).

%% @doc Fetch the serial of id's queue record
fetch_queue_id(Id, Context) ->
    z_db:q1("select serial from rsc_pivot_queue where rsc_id = $1", [Id], Context).

%% @doc Delete the previously queued ids iff the queue entry has not been updated in the meanwhile
delete_queue(Qs, Context) ->
    F = fun(Ctx) ->
        [ z_db:q("delete from rsc_pivot_queue where rsc_id = $1 and serial = $2", [Id,Serial], Ctx) || {Id,Serial} <- Qs ]
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
    R = get_pivot_rsc(Id, Context),
    {ObjIds, CatIds, [TA,TB,TC,TD]} = get_pivot_data(Id, R, Context),

    {SqlA, ArgsA} = to_tsv(TA, $A, []),
    {SqlB, ArgsB} = to_tsv(TB, $B, ArgsA),
    {SqlC, ArgsC} = to_tsv(TC, $C, ArgsB),
    {SqlD, ArgsD} = to_tsv(TD, $D, ArgsC),

    TsvSql = [SqlA, " || ", SqlB, " || ", SqlC, " || ", SqlD],

    TsvObj = [ [" zpo",integer_to_list(OId)] || OId <- ObjIds ],
    TsvCat = [ [" zpc",integer_to_list(CId)] || CId <- CatIds ],
    TsvIds = list_to_binary([TsvObj,TsvCat]),

    PropsPrePivoted = z_pivot_rsc:pivot_resource_update([], m_rsc:get_raw(Id, Context)),

    N = length(ArgsD),
    Sql = list_to_binary([
            "update rsc set pivot_tsv = ",TsvSql,
            ", pivot_rtsv     = to_tsvector($",integer_to_list(N+1),")",
            ", pivot_street   = $",integer_to_list(N+2),
            ", pivot_city     = $",integer_to_list(N+3),
            ", pivot_postcode = $",integer_to_list(N+4),
            ", pivot_state    = $",integer_to_list(N+5),
            ", pivot_country  = $",integer_to_list(N+6),
            ", pivot_first_name=$",integer_to_list(N+7),
            ", pivot_surname  = $",integer_to_list(N+8),
            ", pivot_gender   = $",integer_to_list(N+9),
            ", pivot_date_start = $",integer_to_list(N+10),
            ", pivot_date_end   = $",integer_to_list(N+11),
            ", pivot_date_start_month_day   = $",integer_to_list(N+12),
            ", pivot_date_end_month_day   = $",integer_to_list(N+13),
            " where id = $",integer_to_list(N+14)
        ]),

    SqlArgs = ArgsD ++ [
        TsvIds,
        truncate(proplists:get_value(address_street_1, R), 120),
        truncate(proplists:get_value(address_city, R), 100),
        truncate(proplists:get_value(address_postcode, R), 30),
        truncate(proplists:get_value(address_state, R), 50),
        truncate(proplists:get_value(address_country, R), 80),
        truncate(proplists:get_value(name_first, R), 100),
        truncate(proplists:get_value(name_surname, R), 100),
        truncate(proplists:get_value(gender, R), 1),
        proplists:get_value(pivot_date_start, PropsPrePivoted),
        proplists:get_value(pivot_date_end, PropsPrePivoted),
        proplists:get_value(pivot_date_start_month_day, PropsPrePivoted),
        proplists:get_value(pivot_date_end_month_day, PropsPrePivoted),
        Id
    ],
    z_db:q(Sql, SqlArgs, Context),
    Results = z_notifier:map({custom_pivot, Id}, Context),
    [ok = update_custom_pivot(Id, Res, Context) || Res <- Results],

    IsA = m_rsc:is_a(Id, Context),
    z_notifier:notify({rsc_pivot_done, Id, IsA}, Context),
    ok.


    %% Make the setweight(to_tsvector()) parts of the update statement
    to_tsv([], _Level, Args) ->
        {"tsvector('')", Args};
    to_tsv(List, Level, Args) -> 
        {Sql1, Args1} = lists:foldl(
            fun ({Lang,Text}, {Sql, As}) -> 
                N   = length(As) + 1,
                As1 = As ++ [Text],
                {[["setweight(to_tsvector('pg_catalog.",pg_lang(Lang),"', $",integer_to_list(N),"), '",Level,"')"] | Sql], As1}
            end,
            {[], Args},
            List),
    
        {z_utils:combine(" || ", Sql1), Args1}.
            

    %      new.tsv := 
    %        setweight(to_tsvector('pg_catalog.dutch', coalesce(new.title_nl,'')), 'A') || 
    %        setweight(to_tsvector('pg_catalog.dutch', coalesce(new.desc_nl,'')),  'D') ||
    %        setweight(to_tsvector('pg_catalog.english', coalesce(new.title_en,'')), 'A') || 
    %        setweight(to_tsvector('pg_catalog.english', coalesce(new.desc_en,'')),  'D'); 


truncate(undefined, _Len) -> undefined;
truncate(S, Len) -> z_string:to_lower(truncate(S, Len, Len)).
    
    truncate(_S, 0, _Bytes) ->
        "";
    truncate(S, Utf8Len, Bytes) ->
        case z_string:truncate(S, Utf8Len, "") of
            T when length(T) > Bytes -> truncate(T, Utf8Len-1, Bytes);
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
    z_string:to_lower(get_pivot_title([{title, m_rsc:p(Id, title, Context)}])).

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
    z_notifier:foldl(pivot_rsc_data, m_rsc:get(Id, Context), Context).


%% get_pivot_data {objids, catids, [ta,tb,tc,td]}
get_pivot_data(Id, Context) ->
    get_pivot_data(Id, get_pivot_rsc(Id, Context), Context).
    
get_pivot_data(Id, Rsc, Context) ->
    R = z_notifier:foldr({pivot_get, Id}, Rsc, Context),
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
    Ids = lists:usort(m_edge:objects(Id, Context)),
    Ids1 = z_notifier:foldr({pivot_related, Id}, Ids, Context),
    IdsTexts = z_notifier:foldr({pivot_related_text_ids, Id}, Ids, Context),
    Texts = [ m_rsc:p(R, title, Context) || R <- IdsTexts ],
    {Ids1, Texts}.
    

%% @doc Fetch the names of all categories in the category path
%% @spec category(int(), Context) -> { IdList, TextsList }
category(CatId, Context) ->
    Names = [ z_convert:to_list(Name) || Name <- m_category:is_a(CatId, Context) ],
    Ids   = [ CatId |  m_category:get_path(CatId, Context) ],
    {Ids, Names}.


fetch_texts({title, Value}, {A,B}, _Context) ->
    {[Value|A], B};
fetch_texts({subtitle, Value}, {A,B}, _Context) ->
    {[Value|A], B};
fetch_texts({name_surname, Value}, {A,B}, _Context) ->
    {[Value|A], B};
fetch_texts({name_first, Value}, {A,B}, _Context) ->
    {[Value|A], B};
fetch_texts({F, Value}, {A,B}, _Context) when is_binary(Value) ->
    case is_lang_neutral(F, Value) of
        true ->
            {A, [{trans, [{none, Value}]}|B]};
        false ->
            case do_pivot_field(F) of
                false -> {A,B};
                true -> {A, [Value|B]}
            end
    end;
fetch_texts({F, {{Y,M,D},{H,Min,S}} = Date}, {A,B} = Acc, Context)
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
fetch_texts({_, {trans, _} = V}, {A,B}, _Context) ->
    {A, [V|B]};
fetch_texts({F, V}, {A,B} = Acc, _Context) ->
    case do_pivot_field(F) of
        false -> Acc;
        true ->
            case z_string:is_string(V) of
                true -> {A, [V|B]};
                false -> Acc
            end
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
do_pivot_field(uri) -> false; 
do_pivot_field(publication_start) -> false; 
do_pivot_field(publication_end) -> false; 
do_pivot_field(created) -> false; 
do_pivot_field(modified) -> false; 
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


%% @doc Translate a language to a language string as used by postgresql
%% @todo Add more languages
% pg_lang(en) -> "english";
% pg_lang(nl) -> "dutch";
% pg_lang(de) -> "german";
% pg_lang(fr) -> "french";
pg_lang(_) -> "english".


%% @spec define_custom_pivot(Module, columns(), Context) -> ok
%% @doc Let a module define a custom pivot
%% columns() -> [column()]
%% column()  -> {ColumName::atom(), ColSpec::string()}
define_custom_pivot(Module, Columns, Context) ->
    TableName = "pivot_" ++ z_convert:to_list(Module),
    case z_db:table_exists(TableName, Context) of
        true ->
            ok;
        false ->
            Fields = custom_columns(Columns),
            Sql = "CREATE TABLE " ++ TableName ++ "(" ++
                "id int NOT NULL," ++ Fields ++ ")",
            z_db:q(lists:flatten(Sql), Context),
            z_db:q("ALTER TABLE " ++ TableName ++ " ADD CONSTRAINT fk_" ++ TableName ++ "_id FOREIGN KEY (id) REFERENCES rsc(id) ON UPDATE CASCADE ON DELETE CASCADE", Context),
            
            Idx = ["CREATE INDEX " ++ z_convert:to_list(K) ++ "_key ON " ++ TableName ++ "(" ++ z_convert:to_list(K) ++ ")" || {K,_} <- Columns],
            [z_db:q(Sql1, Context) || Sql1 <- Idx]
    end,
    ok.


custom_columns(Cols) ->
    custom_columns(Cols, []).
custom_columns([{Name, Spec}], Acc) ->
    [ z_convert:to_list(Name), " ", Spec |  Acc];
custom_columns([{Name, Spec}|Rest], Acc) ->
    custom_columns(Rest, [ ", ", z_convert:to_list(Name), " ", Spec |  Acc]).



update_custom_pivot(_Id, none, _Context) ->
    ok;
update_custom_pivot(Id, {Module, Columns}, Context) ->
    TableName = "pivot_" ++ z_convert:to_list(Module),
    case z_db:select(TableName, Id, Context) of
        {ok, []} ->
            {ok, _} = z_db:insert(TableName, [{id, Id}|Columns], Context);
        {ok, _}  ->
            {ok, _} = z_db:update(TableName, Id, Columns, Context)
    end,
    ok.


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


%% @doc Flush all 'poll' messages in the message queue.  This is needed when waking up after sleep.
flush() ->
    receive
        {'$gen_cast', poll} -> flush()
    after 
        0 -> ok
    end.
