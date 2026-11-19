%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Bounded per-site event ingestion and aggregate batching.
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

-module(z_analytics_worker).
-behaviour(gen_server).
-moduledoc("Bounded per-site aggregate ingestion and immutable, retry-safe flush batches.").
-export([start_link/1, ingest/4, snapshot/1, interested/2, schema/2, status/1, reload/1, init/1,
    handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-include_lib("zotonic_core/include/zotonic.hrl").

-define(MAX_ENTRIES, 10000).
-define(DRAIN_BUDGET_MS, 5).

name(S) -> z_utils:name_for_site(analytics_worker,S).
table(S) -> z_utils:name_for_site(analytics_config,S).
buffer(S) -> z_utils:name_for_site(analytics_buffer,S).
start_link(Site) -> gen_server:start_link({local,name(Site)},?MODULE,Site,[]).
snapshot(Context) ->
    try ets:lookup_element(table(z_context:site(Context)),config,2)
    catch error:badarg -> undefined end.
status(Context) ->
    try gen_server:call(name(z_context:site(Context)),status,1000)
    catch exit:_ -> #{running=>false} end.
reload(Context) ->
    case whereis(name(z_context:site(Context))) of undefined -> ok; P -> P ! reload, ok end.

%% Event-local ETS rows avoid copying every module schema on each request.
-spec interested(binary(), z:context()) -> boolean().
interested(Event,Context) ->
    try ets:lookup_element(table(z_context:site(Context)),{event,Event},4)
    catch error:badarg -> false end.

-spec schema(binary(), z:context()) -> {ok,map()} | error.
schema(Event,Context) ->
    try {ok,ets:lookup_element(table(z_context:site(Context)),{event,Event},3)}
    catch error:badarg -> error end.

ingest(Event,Props,T,Context) ->
    try ets:lookup(table(z_context:site(Context)),{event,Event}) of
        [] -> {error,unknown_event};
        [{_,Gen,Schema,Active}] ->
            case z_analytics:properties(Props,Schema) of
                {ok,P} when Active ->
                    try ringbuffer:write(buffer(z_context:site(Context)),
                        {Gen,Event,maps:get(version,Schema),T,P})
                    catch _:_ -> ok end;
                {ok,_} -> ok;
                Error -> Error
            end
    catch error:badarg -> ok end.

init(Site) ->
    Context = z_context:new(Site),
    _ = ringbuffer:delete(buffer(Site)),
    {ok,_} = ringbuffer:new(buffer(Site),4096),
    ets:new(table(Site),[named_table,protected,set,{read_concurrency,true}]),
    Now = erlang:system_time(second),
    S0 = #{site=>Site, epoch=>crypto:strong_rand_bytes(16), seq=>0,
        pending=>undefined, deltas=>#{}, entries=>0, admissions=>#{}, from=>Now, lost=>false, dropped=>0,
        accepted=>0, last_flush=>undefined, generation=>0, failures=>0},
    S = refresh(S0),
    ok = z_mqtt:subscribe([<<"$SYS">>,<<"site">>,atom_to_binary(Site,utf8),<<"log">>,<<"access">>],
        {mod_analytics,access_log,[]},self(),z_acl:sudo(Context)),
    erlang:send_after(100,self(),drain),
    erlang:send_after(10000,self(),flush),
    {ok,S}.

handle_call(status,_,S) ->
    {reply,(maps:with([accepted,dropped,last_flush,failures],S))#{running=>true,
        pending=>maps:get(pending,S)=/=undefined, buffered=>map_size(maps:get(deltas,S)),
        entries=>maps:get(entries,S), entry_limit=>?MAX_ENTRIES},S};
handle_call(_,_,S) -> {reply,{error,unknown_call},S}.
handle_cast(_,S) -> {noreply,S}.
%% Upgrade a running worker before using the event index introduced here.
handle_info(drain,S) when not is_map_key(event_index,S) ->
    handle_info(drain,refresh(S));
handle_info(drain,S) ->
    %% Yield after each bounded chunk, but do not impose a 2,000 events/s cap.
    %% A timer is used instead of a recursive drain so calls/flushes stay fair.
    Now = erlang:system_time(second),
    {S1,Delay} = drain(200,prune_admissions(Now,S),Now,
        erlang:monotonic_time(millisecond)+?DRAIN_BUDGET_MS),
    erlang:send_after(Delay,self(),drain),
    {noreply,S1};
handle_info(flush,#{pending:=undefined,deltas:=D,event_index:=Index}=S)
        when map_size(D) =:= 0, map_size(Index) =:= 0 ->
    %% Paused/empty sites need no receipts or coverage writes.
    erlang:send_after(10000,self(),flush),
    {noreply,S#{from=>erlang:system_time(second)}};
handle_info(flush,#{pending:=undefined}=S) ->
    Now = erlang:system_time(second),
    Batch = #{from=>maps:get(from,S),until=>Now,deltas=>maps:get(deltas,S),lost=>maps:get(lost,S)},
    Seq = maps:get(seq,S)+1,
    Pending = {Seq,Batch,maps:get(collectors,S)},
    send_batch(Pending,S),
    erlang:send_after(10000,self(),flush),
    {noreply,S#{pending=>Pending,seq=>Seq,deltas=>#{},entries=>0,from=>Now,lost=>false}};
handle_info(flush,S) ->
    erlang:send_after(10000,self(),flush),
    {noreply,S};
handle_info({flushed,Seq,ok},#{pending:={Seq,_,_}}=S) ->
    {noreply,S#{pending=>undefined,last_flush=>erlang:system_time(second),failures=>0}};
handle_info({flushed,Seq,_Error},#{pending:={Seq,Batch,_}}=S) ->
    S1 = S#{failures=>maps:get(failures,S)+1},
    case erlang:system_time(second)-maps:get(until,Batch) > 120 of
        true -> {noreply,drop(S1#{pending=>undefined})};
        false -> erlang:send_after(5000,self(),retry), {noreply,S1}
    end;
handle_info(retry,#{pending:=undefined}=S) -> {noreply,S};
handle_info(retry,#{pending:={_,Batch,_}=P}=S) ->
    %% An overloaded node can deliver the retry timer long after it was due.
    case erlang:system_time(second)-maps:get(until,Batch) > 120 of
        true -> {noreply,drop(S#{pending=>undefined})};
        false -> send_batch(P,S), {noreply,S}
    end;
handle_info(reload,S) ->
    coalesce_reload(64),
    {noreply,refresh(S)};
handle_info(_,S) -> {noreply,S}.
terminate(_,#{site:=Site}) ->
    z_mqtt:unsubscribe([<<"$SYS">>,<<"site">>,atom_to_binary(Site,utf8),<<"log">>,<<"access">>],self(),z_acl:sudo(z_context:new(Site))),
    ets:delete(table(Site)),
    ringbuffer:delete(buffer(Site)),
    ok.

refresh(#{site:=Site}=S) ->
    Context = z_context:new(Site),
    Reg = z_analytics:registry(Context),
    Cs = [case z_analytics:available(Col,Reg) of
              true -> Col;
              false -> Col#{state=><<"paused">>}
          end || Col <- m_analytics:store_collectors(Context)],
    Gen = maps:get(generation,S)+1,
    EventIndex = lists:foldl(fun
        (#{state:= <<"active">>,event:=Event,schema_version:=V}=Col,A) ->
            maps:update_with({Event,V},fun(L)->[Col|L] end,[Col],A);
        (_,A) -> A
    end,#{},Cs),
    %% Insert all current rows together. Old-generation work is discarded on drain.
    EventRows = [{{event,E},Gen,Schema,maps:is_key({E,maps:get(version,Schema)},EventIndex)}
        || {E,Schema} <- maps:to_list(Reg)],
    ets:insert(table(Site),[{config,#{registry=>Reg,generation=>Gen}}|EventRows]),
    [ets:delete(table(Site),{event,E}) || {{event,E},_,_,_} <- ets:tab2list(table(Site)),
        not maps:is_key(E,Reg)],
    Now=erlang:system_time(second),
    Oldest=z_analytics:bucket(Now-120,60),
    Ids=maps:from_list([{maps:get(id,Col),true} || Col <- Cs]),
    Recent=maps:filter(fun({Id,T},_) -> T >= Oldest andalso maps:is_key(Id,Ids) end,
        maps:get(admissions,S)),
    Admissions=lists:foldl(fun(Col,A) ->
        Rows=m_analytics:store_rows(maps:get(id,Col),60,z_analytics:bucket(Now-120,60),Now+60,Context),
        lists:foldl(fun(#{start:=T,counts:=Counts},Acc) ->
            K={maps:get(id,Col),T},
            Existing=maps:get(K,Recent,#{}),
            Acc#{K=>maps:merge(maps:map(fun(_,_) -> 0 end,Counts),Existing)}
        end,A,Rows)
    end,Recent,[Col || Col <- Cs, maps:get(state,Col) =:= <<"active">>,
        not z_analytics_frequency:enabled(Col)]),
    S#{registry=>Reg,collectors=>Cs,event_index=>EventIndex,generation=>Gen,
        admissions=>Admissions,admission_minute=>z_analytics:bucket(Now,60),from=>Now,lost=>true}.
%% Module activation and configuration bursts need only one fresh snapshot.
%% Bound the coalescing loop too, so a producer cannot keep us here indefinitely.
coalesce_reload(0) -> ok;
coalesce_reload(N) -> receive reload -> coalesce_reload(N-1) after 0 -> ok end.

send_batch({Seq,Batch,Cs},S) ->
    gen_server:cast(z_utils:name_for_site(analytics_writer,maps:get(site,S)),
        {flush,self(),maps:get(epoch,S),Seq,Batch,Cs}).
prune_admissions(Now,S) ->
    Minute = z_analytics:bucket(Now,60),
    case maps:get(admission_minute,S,undefined) of
        Minute -> S;
        _ -> S#{admission_minute=>Minute,admissions=>maps:filter(
            fun({_,T},_) -> T >= Minute-120 end,maps:get(admissions,S))}
    end.

drain(0,S,_,_) -> {S,0};
drain(N,S,Now,Deadline) ->
    case erlang:monotonic_time(millisecond) >= Deadline of
        true -> {S,0};
        false -> drain_next(N,S,Now,Deadline)
    end.

drain_next(N,#{site:=Site}=S,Now,Deadline) ->
    case ringbuffer:read(buffer(Site)) of
        {error,empty} -> {S,100};
        {ok,{Skipped,E}} ->
            S1 = case Skipped of 0 -> S; _ -> S#{lost=>true,dropped=>maps:get(dropped,S)+Skipped} end,
            drain(N-1,consume(E,Now,S1),Now,Deadline)
    end.
consume({Gen,Event,V,T,Props},Now,#{generation:=Gen}=S) ->
    case T >= Now-120 andalso T =< Now+1 of
        false -> drop(S);
        true ->
            Cs = [X || X <- maps:get({Event,V},maps:get(event_index,S),[]),
                T >= maps:get(started,X), z_analytics:matches(X,Props)],
            lists:foldl(fun(Col,Acc) -> add(Col,T,Props,Acc) end,S,Cs)
    end;
consume(_,_,S) -> drop(S).
add(#{dimension:= <<"page">>}=Col,T,Props,#{deltas:=D,entries:=Entries}=S) ->
    K={maps:get(id,Col),z_analytics:bucket(T,60)},
    G=maps:get(groups,Col),
    %% Reserve the full capacity on first use, so eviction never needs to drop
    %% a new contender at the global hot-buffer limit. Metadata is constant.
    case maps:is_key(K,D) orelse Entries+G =< ?MAX_ENTRIES of
        false -> drop(S);
        true ->
            Counts=z_analytics_frequency:normalize(maps:get(K,D,#{})),
            Next=z_analytics_frequency:add(z_analytics:group(Col,Props),1,Counts,G),
            Extra=case maps:is_key(K,D) of true -> 0; false -> G end,
            S#{deltas=>D#{K=>Next},entries=>Entries+Extra,accepted=>maps:get(accepted,S)+1}
    end;
add(Col,T,Props,#{deltas:=D,admissions:=Admissions}=S) ->
    K = {maps:get(id,Col),z_analytics:bucket(T,60)},
    Counts = maps:get(K,D,#{}), Key = z_analytics:group(Col,Props),
    Known=maps:get(K,Admissions,#{}),
    %% Admission tracks membership, not counts. Reuse the map for known keys.
    Next=case maps:is_key(Key,Known) of
        true -> Known;
        false -> z_analytics:add(Key,0,Known,maps:get(groups,Col))
    end,
    Chosen=case maps:is_key(Key,Next) of true -> Key; false -> <<"$other">> end,
    IsNew=not maps:is_key(Chosen,Counts),
    case maps:get(entries,S) >= ?MAX_ENTRIES andalso IsNew of
        true -> drop(S);
        false ->
            C1=maps:update_with(Chosen,fun(N)->N+1 end,1,Counts),
            S#{deltas=>D#{K=>C1},admissions=>Admissions#{K=>Next},
                entries=>maps:get(entries,S)+case IsNew of true->1;false->0 end,
                accepted=>maps:get(accepted,S)+1}
    end.

drop(S) -> S#{lost=>true,dropped=>maps:get(dropped,S)+1}.
