%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Public reporting, configuration and event recording APIs for Site Analytics.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(m_analytics).
-behaviour(zotonic_model).
-moduledoc(#{
    zotonic_keywords => ["reference", "model", "backend_developer", "logging_and_monitoring", "authorization_and_access_control", "privacy_and_consent"]
}).
-moduledoc("
Read Site Analytics reports, manage recordings and submit trusted server-side events.

## Model paths

| GET path | Result | Permission |
| --- | --- | --- |
| `recordings` | Recording maps in saved order. | `use mod_analytics` |
| `overview` | Map with `panels` and `period` for non-paused recordings. | `use mod_analytics` |
| `dashboard` | Recordings, selected recording and its report. | `use mod_analytics` |
| `configuration` | Recordings, event schemas, dimensions, status and storage usage. | Site administrator |
| `status` | Current ingestion worker status. | Site administrator |
| `registry` | List of declared event schemas. | Site administrator |
| `info/<id>` | Recording details and stored payload/bucket usage. | Site administrator |

For example, `{% for recording in m.analytics.recordings %}{{ recording.name|escape }}{% endfor %}`
lists the configured recordings. Model GET callbacks return `{ok, {Value, Rest}}`.
Access failures return `{error, eacces}`; unsupported paths return `{error, unknown_path}`.
Site administrator checks use `z_acl:is_admin/1`, including `use mod_admin_config`.

POST `create` accepts a definition map; `state/<id>` accepts a map with binary key
`state` (`active` or `paused`); `order` accepts the full list of recording IDs.
These operations enforce administrator access. Deletion is available through `purge/2`
and the configuration page's confirmation flow, not a public model POST path.

## Erlang reporting and configuration

`overview(Period, Context)` returns all non-paused recording panels; `dashboard(Id, Period, Context)`
returns one selected report with the recording list. Both enforce `use mod_analytics`.
Period keys are `hour`, `day` (six hours), `24hours`, `week`, `month`, `year` and `years`.
Unknown periods use six hours. Reports contain aggregate counts; popular-content counts
are lower bounds with approximation metadata. `configuration/1` enforces administrator access.

`create(Definition, State, Context)` accepts binary keys `name`, optional `description`,
`event`, `dimension`, `groups` and `filters`. Filters map dimension names to lists of
allowed values; an empty dimension records totals only. Initial state can be `active`,
`paused` or `draft`. Success returns `{ok, Id}`; invalid definitions, duplicate recordings
and exhausted storage budgets return errors. `set_state/3`, `reorder/2` and `purge/2`
also enforce administrator access. Purging permanently deletes a recording and its history.

## Trusted event API

After declaring an event in an `observe_analytics_events/3` fold observer, submit it with:

```erlang
m_analytics:record_event(<<\"mod_newsletter:sent\">>,
    #{<<\"kind\">> => <<\"digest\">>}, Context).
```

`record_event/3` does not implicitly associate the context's request. For request-associated
measurement use `record_event(Name, Properties, z_context:get_reqdata(Context), Context)`.
The request must match the context. Request exclusions apply, and schema-declared request
dimensions are added by the server; callers cannot override these derived properties.
These are trusted Erlang APIs, not public model POST routes or administrator-only actions.
The calling module is responsible for its domain permissions and for avoiding personal data.
`registry/1` is an internal Erlang schema lookup without the model GET permission check.
The `store_*` functions are trusted persistence APIs used by schema installation and
background workers; they do not enforce user ACLs and are not exposed as model routes.

Recording returns `ok` or `{error, Reason}`. Excluded requests return `ok` without recording;
an accepted event is not a durable-write acknowledgement. Invalid requests, unknown events,
invalid properties and full ingestion queues can return errors. Analytics failure should
not fail the domain operation. `mark_page(Id, Context)` returns an updated context marking
a custom controller's page; normal public-page eligibility checks still apply.
").
-export([m_get/3,m_post/3,record_event/3,record_event/4,mark_page/2,
    create/3,set_state/3,purge/2,reorder/2,overview/2,configuration/1,dashboard/3,registry/1]).
%% Trusted internal persistence API. Public model paths use the ACL-checked APIs above.
-export([store_install/1,store_collectors/1,store_create/3,store_state/3,
    store_purge/2,store_reorder/2,store_flush/5,store_rows/5,store_usage/1,store_info/2]).
-include_lib("zotonic_core/include/zotonic.hrl").

m_get([Path|_]=Parts,_Msg,Context)
        when Path =:= <<"configuration">>; Path =:= <<"status">>; Path =:= <<"registry">>; Path =:= <<"info">> ->
    case z_acl:is_admin(Context) of
        false -> {error,eacces};
        true -> get(Parts,Context)
    end;
m_get(Path,_Msg,Context) ->
    case z_acl:is_allowed(use,mod_analytics,Context) of
        false -> {error,eacces};
        true -> get(Path,Context)
    end.
get([<<"info">>,Id0|Rest],Context) ->
    case to_id(Id0) of
        Id when is_integer(Id),Id>0 ->
            case store_info(Id,Context) of
                {ok,Info} -> {ok,{Info,Rest}};
                Error -> Error
            end;
        _ -> {error,enoent}
    end;
get([<<"recordings">>|Rest],Context) -> {ok,{store_collectors(Context),Rest}};
get([<<"status">>|Rest],Context) -> {ok,{z_analytics_worker:status(Context),Rest}};
get([<<"registry">>|Rest],Context) -> {ok,{maps:values(registry(Context)),Rest}};
get([<<"dashboard">>|Rest],Context) -> {ok,{dashboard(undefined,<<"day">>,Context),Rest}};
get([<<"overview">>|Rest],Context) -> {ok,{overview(<<"day">>,Context),Rest}};
get([<<"configuration">>|Rest],Context) ->
    case configuration(Context) of {error,_}=Error -> Error; Data -> {ok,{Data,Rest}} end;
get(_,_) -> {error,unknown_path}.
m_post([<<"create">>],#{payload:=P},Context) -> create(P,<<"active">>,Context);
m_post([<<"state">>,Id],#{payload:=#{<<"state">>:=State}},Context) -> set_state(to_id(Id),State,Context);
m_post([<<"order">>],#{payload:=Ids},Context) -> reorder(Ids,Context);
m_post(_,_,_) -> {error,unknown_path}.

registry(Context) ->
    case z_analytics_worker:snapshot(Context) of undefined -> z_analytics:registry(Context); #{registry:=R}->R end.

-spec reorder(Ids, Context) -> ok | {error, term()} | {rollback, term()}
    when Ids :: [integer()], Context :: z:context().
reorder(Ids,Context) ->
    case z_acl:is_admin(Context) of
        true -> store_reorder(Ids,Context);
        false -> {error,eacces}
    end.

-spec configuration(Context) -> map() | {error,eacces} when Context :: z:context().
configuration(Context) ->
    case z_acl:is_admin(Context) of
        false -> {error,eacces};
        true ->
            Reg=registry(Context),
            #{recordings=>store_collectors(Context),registry=>[E#{dimension_keys=>lists:sort(maps:keys(maps:get(dimensions,E))),
                choices=>maps:map(fun(_,V) -> case V of
                    L when is_list(L) -> iolist_to_binary(lists:join(<<", ">>,L));
                    http_status -> <<"100–599">>;
                    _ -> <<>>
                end end,maps:get(dimensions,E))} || E <- maps:values(Reg)],
              request_types=>z_analytics_request:dispatch_types(Context),
              dimensions=>z_analytics:dimension_keys(Reg),status=>z_analytics_worker:status(Context),
              usage=>store_usage(Context)}
    end.

%% Skip paused recordings before loading reports; retain the saved display order.
-spec overview(Period, Context) -> map() | {error,eacces}
    when Period :: binary(), Context :: z:context().
overview(Period,Context) ->
    case z_acl:is_allowed(use,mod_analytics,Context) of
        false -> {error,eacces};
        true ->
            Reg=registry(Context),
            Collectors=store_collectors(Context),
            Panels=[#{collector=>Col,available=>z_analytics:available(Col,Reg),
                      report=>report(Col,Period,Context)} || Col <- Collectors,
                      maps:get(state,Col) =/= <<"paused">>],
            #{panels=>Panels,period=>Period}
    end.

-spec record_event(Name, Properties, Context) -> Result when
    Name :: binary(), Properties :: map(), Context :: z:context(), Result :: ok | {error,term()}.
record_event(Name,Properties,Context) -> record_event(Name,Properties,undefined,Context).
-spec record_event(Name, Properties, Req, Context) -> Result when
    Name :: binary(), Properties :: map(), Req :: map() | undefined,
    Context :: z:context(), Result :: ok | {error,term()}.
record_event(Name,Properties,undefined,Context) ->
    %% ingest/4 handles an unavailable worker without a full registry snapshot.
    z_analytics_worker:ingest(Name,Properties,erlang:system_time(second),Context);
record_event(Name,Properties,Req,Context) when is_map(Req),is_map(Properties) ->
    %% Request metadata is best-effort: malformed or incomplete contexts must not
    %% turn a successful domain operation into an analytics exception.
    try record_request(Name,Properties,Req,Context)
    catch _:_ -> {error,invalid_request} end;
record_event(_,_,_,_) -> {error,invalid_request}.

record_request(Name,Properties,Req,Context) ->
    case Req =:= z_context:get_reqdata(Context) of
        false -> {error,invalid_request};
        true ->
            case z_analytics_request:allowed(Context) of
                false -> ok;
                true ->
                    D=z_analytics_request:dimensions(Context),
                    %% Only schema-declared request dimensions are included.
                    case event_schema(Name,Context) of
                        error -> {error,unknown_event};
                        {ok,Schema} ->
                            Enriched=maps:with(maps:keys(maps:get(dimensions,Schema)),D),
                            case lists:any(fun(K)->maps:is_key(K,Properties) end,maps:keys(Enriched)) of
                                true -> {error,reserved_property};
                                false -> z_analytics_worker:ingest(Name,maps:merge(Properties,Enriched),erlang:system_time(second),Context)
                            end
                    end
            end
    end.

%% Use an event-local lookup on the request path. Preserve registry fallback
%% during worker startup or shutdown, when an event row is not available yet.
event_schema(Name,Context) ->
    case z_analytics_worker:schema(Name,Context) of
        {ok,_}=Found -> Found;
        error -> maps:find(Name,registry(Context))
    end.

%% Returns a new Context; handled-time eligibility still checks public visibility.
-spec mark_page(Id, Context) -> z:context() when Id :: integer(), Context :: z:context().
mark_page(Id,Context) when is_integer(Id) ->
    z_context:set(analytics,page,z_context:set(analytics_page,Id,Context)).
-spec create(Definition, State, Context) -> {ok, pos_integer()} | {error, term()} | {rollback, term()}
    when Definition :: map(), State :: binary(), Context :: z:context().
create(P,State,Context) ->
    case z_acl:is_admin(Context) of
        false -> {error,eacces};
        true ->
            case z_analytics:definition(P,registry(Context)) of
                {ok,D} -> R=store_create(D,State,Context),z_analytics_worker:reload(Context),R;
                Error -> Error
            end
    end.
-spec set_state(Id, State, Context) -> ok | {error, term()} | {rollback, term()}
    when Id :: integer() | binary(), State :: binary(), Context :: z:context().
set_state(Id,State,Context) ->
    case z_acl:is_admin(Context) of
        false -> {error,eacces};
        true -> R=store_state(to_id(Id),State,Context),z_analytics_worker:reload(Context),R
    end.
-spec purge(Id, Context) -> ok | {error, term()} | {rollback, term()}
    when Id :: integer() | binary(), Context :: z:context().
purge(Id,Context) ->
    case {z_acl:is_admin(Context),to_id(Id)} of
        {true,N} when is_integer(N),N>0 -> R=store_purge(N,Context),z_analytics_worker:reload(Context),R;
        _ -> {error,eacces}
    end.

-spec dashboard(Id, Period, Context) -> map() | {error, eacces}
    when Id :: integer() | binary() | undefined, Period :: binary(), Context :: z:context().
dashboard(Id0,Period,Context) ->
    case z_acl:is_allowed(use,mod_analytics,Context) of
        false -> {error,eacces};
        true ->
            Reg=registry(Context),
            Cs=[Col#{available=>z_analytics:available(Col,Reg)} || Col <- store_collectors(Context)],
            Id=to_id(Id0),
            Selected=case [X||X<-Cs,maps:get(id,X)=:=Id] of [X]->X; _->case Cs of [X|_]->X; []->undefined end end,
            #{recordings=>Cs,selected=>Selected,report=>report(Selected,Period,Context),period=>Period}
    end.
report(undefined,_,_) -> #{};
report(Col,Period,Context) ->
    {R,N}=period(Period), Now=erlang:system_time(second), End=z_analytics:bucket(Now,R)+R,
    Start=End-R*N, Rows=store_rows(maps:get(id,Col),R,Start,End,Context),
    Popular=z_analytics_frequency:enabled(Col),
    Counts0=lists:foldl(fun(X,A) ->
        B=maps:get(counts,X),
        case Popular of
            true -> z_analytics_frequency:merge(A,B,maps:get(groups,Col));
            false -> z_analytics:merge(A,B)
        end
    end,#{},Rows),
    {Groups,Total,Error}=case Popular of
        true ->
            Ranked=[G#{label=>label(maps:get(key,G),Col,Context)}
                || G <- lists:sublist(z_analytics_frequency:ranked(Counts0),20)],
            {Ranked,z_analytics_frequency:total(Counts0),z_analytics_frequency:error(Counts0)};
        false ->
            Counts=z_analytics:reduce(Counts0,maps:get(groups,Col)),
            Sorted=lists:sort(fun(A,B)->maps:get(count,A)>maps:get(count,B) end,
                [#{label=>label(K,Col,Context),count=>V}||{K,V}<-maps:to_list(Counts)]),
            {Sorted,z_analytics_frequency:total(Counts),0}
    end,
    %% Popular content needs only ranked counts. Skip the time index, gap
    %% expansion, graph aggregation and timestamp formatting for this report.
    Chart=case Popular of
        true -> [#{label=>maps:get(label,G),value=>maps:get(count,G)} || G <- Groups];
        false -> trend_chart(Rows,Start,End,R,N,Context)
    end,
    #{chart=>Chart,groups=>lists:sublist(Groups,100),popular=>Popular,error=>Error,
      total=>Total,resolution=>R,from=>Start,until=>Now,
      incomplete=>length(Rows)<N orelse lists:any(fun(X)->not maps:get(complete,X) end,Rows)}.

trend_chart(Rows,Start,End,R,N,Context) ->
    Index=maps:from_list([{maps:get(start,X),X}||X<-Rows]),
    Step=max(1,(N+239) div 240),
    Points=[point(T,min(End,T+R*Step),R,Index)||T<-lists:seq(Start,End-1,R*Step)],
    [#{label=>time_label(maps:get(start,P),R,Context),
        value=>case maps:get(complete,P) orelse maps:get(total,P)>0 of
            true -> maps:get(total,P); false -> null end}||P<-Points].


time_label(T,R,Context) ->
    Date=z_datetime:timestamp_to_datetime(T),
    Format=case R of 60 -> <<"H:i">>; 900 -> <<"D H:i">>; 3600 -> <<"M j H:i">>; _ -> <<"Y-m-d">> end,
    z_datetime:format(Date,Format,Context).
point(A,B,R,Index) ->
    Rows=[maps:get(T,Index,#{counts=>#{},complete=>false})||T<-lists:seq(A,B-1,R)],
    #{start=>A,total=>lists:sum([z_analytics_frequency:total(maps:get(counts,X))||X<-Rows]),
      complete=>lists:all(fun(X)->maps:get(complete,X) end,Rows)}.
label(<<"$total">>,_,_) -> <<"Total">>;
label(<<"$other">>,_,_) -> <<"Other (overflow)">>;
label(<<"v:",Id/binary>>,#{dimension:= <<"page">>},Context) ->
    case to_id(Id) of
        N when is_integer(N), N>0 ->
            case m_rsc:is_visible(N,Context) of
                true -> z_trans:lookup_fallback(m_rsc:p(N,title,Context),Context);
                false -> <<"Unavailable content">>
            end;
        _ -> <<"Unknown content">>
    end;
label(<<"v:",V/binary>>,_,_) -> V;
label(_,_,_) -> <<"Unknown">>.
period(<<"hour">>)->{60,60};
period(<<"24hours">>)->{900,96};
period(<<"week">>)->{900,672};
period(<<"month">>)->{3600,720};
period(<<"year">>)->{86400,365};
period(<<"years">>)->{86400,1826};
period(_)->{60,360}.
to_id(I) when is_integer(I) -> I;
to_id(B) when is_binary(B),byte_size(B)<12 -> try binary_to_integer(B) catch _:_ -> undefined end;
to_id(_) -> undefined.


%% Persistence: bounded site-local storage and retry-safe aggregate batches.

store_install(Context) ->
    Statements = [
        "
            create table if not exists analytics_budget (
                id int primary key check(id=1),
                marker int not null default 0
            )",
        "insert into analytics_budget(id) values(1) on conflict do nothing",
        "
            create table if not exists analytics_collector (
                id serial primary key,
                name varchar(255) not null,
                definition bytea not null,
                state varchar(16) not null check(state in ('active','paused','draft')),
                started bigint not null,
                sort_order int not null default 0,
                created timestamptz not null default now()
            )",
        "
            create table if not exists analytics_bucket (
                collector_id int not null references analytics_collector(id) on delete cascade,
                resolution int not null check(resolution in (60,900,3600,86400)),
                slot int not null check(slot>=0),
                start bigint not null,
                counts bytea not null,
                coverage bigint not null default 0,
                complete boolean not null default false,
                quality boolean not null default false,
                primary key(collector_id,resolution,slot)
            )",
        "
            create index if not exists analytics_bucket_time
            on analytics_bucket(collector_id,resolution,start)",
        "
            create table if not exists analytics_batch (
                epoch bytea not null,
                seq bigint not null,
                created timestamptz not null default now(),
                primary key(epoch,seq)
            )",
        "create index if not exists analytics_batch_created on analytics_batch(created)"
    ],
    lists:foreach(fun(Sql) -> z_db:q(Sql,Context) end,Statements),
    case z_db:column_exists(analytics_collector, sort_order, Context) of
        true -> ok;
        false -> z_db:q("
            alter table analytics_collector
            add column if not exists sort_order int not null default 0", Context)
    end,
    ok.

store_collectors(Context) ->
    {ok, Rows} = z_db:qmap("select * from analytics_collector order by sort_order, id",Context),
    [ (z_analytics:decode(maps:get(<<"definition">>,R)))#{
        id => maps:get(<<"id">>,R), state => maps:get(<<"state">>,R),
        name => maps:get(<<"name">>,R), started => maps:get(<<"started">>,R)} || R <- Rows ].

store_create(D, State, Context) when State =:= <<"draft">>; State =:= <<"active">>; State =:= <<"paused">> ->
    z_db:transaction(fun(Tx) ->
        lock(Tx),
        Cs = store_collectors(Tx),
        Budget = config(row_budget, 1000000, Tx),
        Reserved = lists:sum([3578*maps:get(groups,X) || X <- Cs]),
        Equivalent = [X || X <- Cs, maps:without([id,name,description,state,started],X) =:= maps:without([name,description],D)],
        case {Equivalent, length(Cs) < 32 andalso Reserved+3578*maps:get(groups,D) =< Budget} of
            {[X|_], _} -> {error, {exists, maps:get(id,X)}};
            {[], true} ->
                Id = z_db:q1("
                    insert into analytics_collector(name, definition, state, started, sort_order)
                    values (
                        $1, $2, $3, $4,
                        (select coalesce(max(sort_order), 0) + 1 from analytics_collector)
                    )
                    returning id",
                    [maps:get(name,D), z_analytics:encode(D),State,erlang:system_time(second)],Tx),
                {ok,Id};
            _ -> {error, budget_exceeded}
        end
    end,Context);
store_create(_,_,_) -> {error, invalid_state}.

%% Validate a complete permutation under the same lock used for create/purge.
%% Stale pages must not silently lose a newly created recording from the order.
-spec store_reorder(Ids, Context) -> ok | {error, term()} | {rollback, term()}
    when Ids :: [integer()], Context :: z:context().
store_reorder(Ids, Context) when is_list(Ids), length(Ids) =< 32 ->
    z_db:transaction(fun(Tx) ->
        lock(Tx),
        Current = [maps:get(id, X) || X <- store_collectors(Tx)],
        case lists:sort(Ids) =:= lists:sort(Current) of
            true ->
                lists:foreach(fun({Id, Position}) ->
                    z_db:q("update analytics_collector set sort_order=$2 where id=$1", [Id,Position], Tx)
                end, lists:zip(Ids, lists:seq(1,length(Ids)))),
                ok;
            false -> {error, stale_order}
        end
    end, Context);
store_reorder(_, _) -> {error, invalid_order}.

store_state(Id, State, Context) when is_integer(Id), State =:= <<"active">>; is_integer(Id), State =:= <<"paused">> ->
    z_db:transaction(fun(Tx) ->
        lock(Tx),
        case z_db:q("update analytics_collector set state=$2 where id=$1",[Id,State],Tx) of
            1 -> ok; _ -> {error,enoent}
        end
    end,Context);
store_state(_,_,_) -> {error,invalid_state}.
store_purge(Id,Context) when is_integer(Id) ->
    z_db:transaction(fun(Tx) -> lock(Tx), z_db:q("delete from analytics_collector where id=$1",[Id],Tx), ok end,Context).
lock(Context) -> z_db:q("select id from analytics_budget where id=1 for update",Context).
config(K,Default,Context) ->
    case m_config:get_value(mod_analytics,K,Context) of
        undefined -> Default;
        V -> try max(1,z_convert:to_integer(V)) catch _:_ -> Default end
    end.

%% Batch = #{deltas => #{{Collector,Minute} => #{Group => Count}}, from, until, lost}.
%% Receipt and all snapshots/rollups commit together. Retry the same epoch/sequence.
store_flush(Epoch, Seq, Batch, Cs, Context) ->
    Deadline = erlang:monotonic_time(millisecond)+10000,
    z_db:transaction(fun(Tx0) ->
        Tx = z_context:set(analytics_flush_deadline,Deadline,Tx0),
        flush_q("set local statement_timeout = '10s'",Tx),
        flush_q("set local lock_timeout = '250ms'",Tx),
        flush_q("select id from analytics_budget where id=1 for update",Tx),
        flush_q("delete from analytics_batch where created < now() - interval '24 hours'",Tx),
        case flush_q1("select 1 from analytics_batch where epoch=$1 and seq=$2",[Epoch,Seq],Tx) of
            1 -> ok;
            undefined ->
                case flush_q1("select count(*) from analytics_batch",Tx) < 90000 of
                    true -> ok; false -> error(receipt_budget_exceeded)
                end,
                flush_q("insert into analytics_batch(epoch,seq) values($1,$2)",[Epoch,Seq],Tx),
                Now = maps:get(until,Batch),
                %% Partition once instead of scanning all deltas for every recording.
                ByCollector = maps:fold(fun({Id,T},Counts,A) ->
                    maps:update_with(Id,fun(D)->D#{T=>Counts} end,#{T=>Counts},A)
                end,#{},maps:get(deltas,Batch)),
                Existing = maps:from_list([{Id,true} || {Id} <-
                    flush_q("select id from analytics_collector",Tx)]),
                lists:foreach(fun(#{id:=Id}=Col) ->
                    case maps:is_key(Id,Existing) of
                        true -> flush_collector(Col,Batch#{deltas=>maps:get(Id,ByCollector,#{})},Now,Tx);
                        false -> ok
                    end
                end,Cs),
                ok
        end
    end,[{noretry_on_deadlock,true}],Context).

flush_collector(#{id := Id} = Col, Batch, Now, Context) ->
    From = max(maps:get(from,Batch),maps:get(started,Col)),
    Deltas = maps:get(deltas,Batch),
    Times = lists:usort(maps:keys(Deltas)
        ++ case maps:get(state,Col) of
            <<"active">> -> minute_times(From,Now);
            _ -> []
        end),
    lists:foreach(fun(T) ->
        case T >= z_analytics:bucket(Now,60)-359*60 andalso T =< Now of
            false -> ok;
            true ->
                Old = get_bucket(Id,60,T,Context),
                Counts0 = maps:get(counts,Old,#{}),
                Delta = maps:get(T,Deltas,#{}),
                Counts = case z_analytics_frequency:enabled(Col) of
                    true ->
                        z_analytics_frequency:merge(Counts0,Delta,maps:get(groups,Col));
                    false ->
                        lists:foldl(
                            fun({K,N},A)->z_analytics:add(K,N,A,maps:get(groups,Col)) end,
                            Counts0,
                            lists:sort(maps:to_list(Delta)))
                end,
                Cov = maps:get(coverage,Old,0)
                    bor case maps:get(state,Col) of
                            <<"active">> -> z_analytics:coverage(T,From,Now);
                            _ -> 0
                        end,
                Q = maps:get(quality,Old,false) orelse maps:get(lost,Batch,false),
                put_bucket(Id,60,T,Counts,Cov,z_analytics:complete(Cov) andalso not Q,Q,Context)
        end
    end,Times),
    rollup(Col,Times,60,900,Context),
    rollup(Col,lists:usort([z_analytics:bucket(T,900)||T<-Times]),900,3600,Context),
    rollup(Col,lists:usort([z_analytics:bucket(T,3600)||T<-Times]),3600,86400,Context),
    lists:foreach(fun({R,N}) ->
        %% Expiry thresholds change only at a tier boundary, not each flush.
        case z_analytics:bucket(maps:get(from,Batch),R) < z_analytics:bucket(Now,R) of
            true -> flush_q("
                delete
                from analytics_bucket
                where collector_id=$1
                  and resolution=$2
                  and start < $3",
                [Id,R,z_analytics:bucket(Now,R)-(N-1)*R],Context);
            false -> ok
        end
    end,z_analytics:tiers()).
minute_times(From,Until) when Until > From, Until-From =< 120 ->
    lists:seq(z_analytics:bucket(From,60),z_analytics:bucket(Until-1,60),60);
minute_times(_,_) ->
    [].

rollup(#{id := Id, groups := G}=Col, Times, Child, Parent, Context) ->
    lists:foreach(
        fun(T) ->
            {ok,Rows} = flush_qmap("
                select counts,complete,quality
                from analytics_bucket
                where collector_id=$1
                  and resolution=$2
                  and start >= $3
                  and start < $4
                order by start",
                [Id,Child,T,T+Parent],Context),
            Counts = lists:foldl(
                fun(R,A) ->
                    B=z_analytics:decode(maps:get(<<"counts">>,R)),
                    case z_analytics_frequency:enabled(Col) of
                        true -> z_analytics_frequency:merge(A,B,G);
                        false -> z_analytics:merge(A,B)
                    end
                end,
                #{},
                Rows),
            Reduced=case z_analytics_frequency:enabled(Col) of
                true -> Counts;
                false -> z_analytics:reduce(Counts,G)
            end,
            Complete = length(Rows) =:= Parent div Child
                       andalso lists:all(fun(R)->maps:get(<<"complete">>,R) end,Rows),
            Quality = lists:any(fun(R)->maps:get(<<"quality">>,R) end,Rows),
            put_bucket(Id,Parent,T,Reduced,0,Complete,Quality,Context)
        end,
        lists:usort([z_analytics:bucket(T,Parent) || T <- Times])).

get_bucket(Id,R,T,Context) ->
    case flush_qmap_row("
        select counts,coverage,quality
        from analytics_bucket
        where collector_id=$1
          and resolution=$2
          and slot=$3
          and start=$4",
            [Id,R,z_analytics:slot(T,R),T],Context) of
        {ok,Row} ->
            #{
                counts=>z_analytics:decode(maps:get(<<"counts">>,Row)),
                coverage=>maps:get(<<"coverage">>,Row),quality=>maps:get(<<"quality">>,Row)
            };
        {error,enoent} ->
            #{}
    end.

put_bucket(Id,R,T,Counts,Cov,Complete,Quality,Context) ->
    flush_q("
        insert into analytics_bucket(
            collector_id, resolution, slot, start, counts, coverage, complete, quality
        )
        values ($1, $2, $3, $4, $5, $6, $7, $8)
        on conflict (collector_id, resolution, slot) do update
        set start = excluded.start,
            counts = excluded.counts,
            coverage = excluded.coverage,
            complete = excluded.complete,
            quality = excluded.quality
        where analytics_bucket.start <= excluded.start
          and (
              analytics_bucket.start,
              analytics_bucket.counts,
              analytics_bucket.coverage,
              analytics_bucket.complete,
              analytics_bucket.quality
          ) is distinct from (
              excluded.start,
              excluded.counts,
              excluded.coverage,
              excluded.complete,
              excluded.quality
          )",
        [Id,R,z_analytics:slot(T,R),T,z_analytics:encode(Counts),Cov,Complete,Quality],
        Context).

%% Share one monotonic budget across the flush, instead of allowing every query
%% another ten seconds. Driver timeouts cancel SQL; a failure rolls back receipt
%% and counts together. Connection checkout/transaction cleanup are external.
flush_timeout(Context) ->
    Remaining = z_context:get(analytics_flush_deadline,Context)-erlang:monotonic_time(millisecond),
    case Remaining > 0 of
        true -> Remaining;
        false -> error(analytics_flush_timeout)
    end.

flush_q(Sql,Context) -> flush_q(Sql,[],Context).
flush_q(Sql,Args,Context) -> z_db:q(Sql,Args,Context,flush_timeout(Context)).

flush_q1(Sql,Context) -> flush_q1(Sql,[],Context).
flush_q1(Sql,Args,Context) -> z_db:q1(Sql,Args,Context,flush_timeout(Context)).

flush_qmap(Sql,Args,Context) -> z_db:qmap(Sql,Args,[{timeout,flush_timeout(Context)}],Context).
flush_qmap_row(Sql,Args,Context) -> z_db:qmap_row(Sql,Args,[{timeout,flush_timeout(Context)}],Context).

store_rows(Id,R,From,Until,Context) ->
    {ok,Rows} = z_db:qmap("
        select start,counts,complete,quality
        from analytics_bucket
        where collector_id=$1
          and resolution=$2
          and start >= $3
          and start < $4
        order by start
        limit 1826",
        [Id,R,From,Until],Context),
    [
        #{
            start=>maps:get(<<"start">>,X),
            counts=>z_analytics:decode(maps:get(<<"counts">>,X)),
            complete=>maps:get(<<"complete">>,X),
            quality=>maps:get(<<"quality">>,X)
        } || X <- Rows
    ].

store_usage(Context) ->
    Cs = store_collectors(Context),
    #{
        reserved => lists:sum([3578*maps:get(groups,X)||X<-Cs]),
        budget => config(row_budget,1000000,Context),
        buckets => z_db:q1("select count(*) from analytics_bucket",Context),
        recordings => length(Cs)
    }.


%% Read on demand for one recording, using the collector/resolution index.
%% Payload sizes are measured by PostgreSQL; shared indexes and table overhead
%% cannot be attributed accurately to an individual recording.
-spec store_info(integer(), z:context()) -> {ok,map()} | {error,enoent}.
store_info(Id,Context) ->
    case z_db:qmap_row("
        select id, name, definition, state, started, created,
               pg_column_size(definition) as definition_bytes
        from analytics_collector
        where id=$1",[Id],Context) of
        {ok,Row} ->
            {ok,Rows}=z_db:qmap("
                select resolution,
                       count(*) as buckets,
                       coalesce(sum(pg_column_size(counts)), 0)::bigint as bytes,
                       min(start) as oldest,
                       max(start) as newest
                from analytics_bucket
                where collector_id = $1
                group by resolution
                order by resolution",[Id],Context),
            Index = maps:from_list([{maps:get(<<"resolution">>,X),X} || X<-Rows]),
            Tiers = [
                begin
                    X=maps:get(R,Index,#{}),
                    #{
                        resolution=>R,
                        slots=>N,
                        buckets=>maps:get(<<"buckets">>,X,0),
                        bytes=>maps:get(<<"bytes">>,X,0),
                        oldest=>info_date(maps:get(<<"oldest">>,X,undefined)),
                        newest=>info_date(maps:get(<<"newest">>,X,undefined))
                    }
                end || {R,N}<-z_analytics:tiers()
            ],
            D=z_analytics:decode(maps:get(<<"definition">>,Row)),
            Bytes=maps:get(<<"definition_bytes">>,Row)+lists:sum([maps:get(bytes,X)||X<-Tiers]),
            {ok, #{
                collector => D#{
                    id=>Id,
                    name=>maps:get(<<"name">>,Row),
                    state=>maps:get(<<"state">>,Row)
                },
                created => maps:get(<<"created">>,Row),
                started => info_date(maps:get(<<"started">>,Row)),
                payload_bytes => Bytes,
                reserved => 3578*maps:get(groups,D),
                tiers => Tiers,
                buckets => lists:sum([maps:get(buckets,X)||X<-Tiers])
            }};
        {error,enoent} ->
            {error,enoent}
    end.

info_date(T) when is_integer(T) -> z_datetime:timestamp_to_datetime(T);
info_date(_) -> undefined.
