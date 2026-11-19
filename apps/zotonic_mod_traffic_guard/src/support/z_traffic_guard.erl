%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Bounded per-site crawl budgets and pure token-bucket admission.
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

-module(z_traffic_guard).
-behaviour(gen_server).
-export([start_link/1, check/3, settings/1, mode/1, reset/1, step/3, decision/3,throttled/1,matches/3,index/1,reload_rules/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-include_lib("zotonic_core/include/zotonic.hrl").

start_link(Args) ->
    Context = proplists:get_value(context, Args),
    gen_server:start_link({local, name(Context)}, ?MODULE, [Context], []).
reset(Context) ->
    %% Synchronous invalidation: a completed admin save cannot leave a stale list.
    try gen_server:call(name(Context), reset, infinity)
    catch exit:_ -> ok end.
reload_rules(Context) ->
    try gen_server:call(name(Context),reload_rules,infinity)
    catch exit:_ -> ok end.
throttled(Context) ->
    try gen_server:call(name(Context),throttled,1000)
    catch exit:_ -> {error,unavailable} end.
name(Context) -> z_utils:name_for_site(traffic_guard, z_context:site(Context)).

-spec mode(Value) -> off | observe | enforce when Value :: term().
mode(off) -> off;
mode(<<"off">>) -> off;
mode("off") -> off;
mode(enforce) -> enforce;
mode(<<"enforce">>) -> enforce;
mode("enforce") -> enforce;
mode(_) -> observe.

-spec settings(Context) -> map() when Context :: z:context().
settings(Context) ->
    try ets:lookup_element(name(Context), config, 2)
    catch error:badarg ->
        try gen_server:call(name(Context), settings, 1000)
        catch exit:_ -> read_settings(Context) end
    end.

read_settings(Context) ->
    #{ip_lists => maps:map(fun(_, Entries) -> index(Entries) end, m_traffic_guard:stored(Context)),
      mode => mode(m_config:get_value(mod_traffic_guard, mode, Context)),
      period => integer_config(period, 60, 1, 3600, Context) * 1000,
      request_limit => integer_config(request_limit, 120, 1, 100000, Context),
      request_burst => integer_config(request_burst, 30, 1, 1000, Context),
      max_clients => integer_config(max_clients, 5000, 1, 50000, Context)}.

integer_config(Key, Default, Min, Max, Context) ->
    try z_convert:to_integer(m_config:get_value(mod_traffic_guard, Key, Default, Context)) of
        N when is_integer(N), N >= Min, N =< Max -> N;
        _ -> Default
    catch _:_ -> Default end.

%% No cookie or user-agent component: rotating them must not reset a budget.
%% The mailbox check sheds work before the limiter becomes a bottleneck.
-spec check(Client, Settings, Context) -> map() when
    Client :: term(), Settings :: map(), Context :: z:context().
check(_, #{mode := off}, _) -> decision(off, none, 0);
check(Client, Settings, Context) ->
    IP=m_traffic_guard:normalize(Client),
    case z_traffic_guard_rules:lookup(IP,Context) of
        ip_allow -> decision(maps:get(mode,Settings),ip_allow,0);
        ip_block -> decision(maps:get(mode,Settings),ip_block,60);
        _ -> check_budget(IP,Settings,Context)
    end.

check_budget(Client, Settings, Context) ->
    try
        Pid = whereis(name(Context)),
        {message_queue_len, N} = process_info(Pid, message_queue_len),
        true = N < 100,
        gen_server:call(Pid, {check, Client, Settings,
            erlang:monotonic_time(millisecond) + 1000}, 1000)
    catch _:_ -> decision(maps:get(mode, Settings), unavailable, 0) end.

init([Context]) ->
    ets:new(name(Context), [named_table, protected, set, {read_concurrency,true}]),
    erlang:send_after(30000, self(), prune),
    {ok, #{clients => #{}, settings => undefined, context => Context, managed => false}}.

handle_call(reload_rules, _, State) ->
    C=maps:get(context,State),
    Settings=read_settings(C),
    ok=z_traffic_guard_rules:replace(maps:get(ip_lists,Settings),C),
    ets:insert(name(C),{config,maps:remove(ip_lists,Settings)}),
    %% A rule edit must not forgive other clients' debt or erase their evidence.
    {reply,ok,State#{settings=>Settings,managed=>true}};
handle_call(throttled, _, State) ->
    Now=erlang:monotonic_time(millisecond),
    Wall=erlang:system_time(second),
    S=current_settings(maps:get(settings,State),State),
    Lists=case S of #{ip_lists:=L}->L; _->#{allow=>#{},block=>#{}} end,
    Rows=[#{ip=>list_to_binary(inet:ntoa(IP)),reason=>maps:get(reason,E),
        count=>maps:get(rejections,E),retry_after=>(Until-Now+999) div 1000,
        action=>maps:get(action,E)} || {IP,#{throttled_until:=Until}=E} <- maps:to_list(maps:get(clients,State)),
        Until>Now, is_tuple(IP), (tuple_size(IP)=:=4 orelse tuple_size(IP)=:=8),
        not matches(IP,maps:get(allow,Lists),Wall),
        not matches(IP,maps:get(block,Lists),Wall)],
    {reply,{ok,lists:sublist(lists:sort(fun(A,B)->maps:get(count,A)>maps:get(count,B) end,Rows),200)},State};
handle_call(reset, _, State) ->
    ok=z_traffic_guard_rules:invalidate(maps:get(context,State)),
    ets:delete(name(maps:get(context,State)), config),
    {reply, ok, State#{clients => #{}, settings => undefined}};
handle_call(settings, _, State) ->
    Settings = current_settings(undefined, State#{managed => true}),
    {reply, maps:remove(ip_lists,Settings), State#{managed => true, settings => Settings}};
handle_call({check, Client, Settings, Deadline}, _, State) ->
    case erlang:monotonic_time(millisecond) >= Deadline of
        true -> {reply, decision(maps:get(mode, Settings), unavailable, 0), State};
        false ->
            Current = current_settings(Settings,State),
            CachedState = case maps:get(managed,State,false) of
                true -> State#{settings => Current};
                false -> State
            end,
            admit(Client, Current, CachedState)
    end;
handle_call(_, _, State) -> {reply, {error, badarg}, State}.

%% Once used by HTTP, configuration is authoritative in this worker. A request
%% queued before an update cannot switch back to old limits and reset budgets.
current_settings(_, #{managed := true, settings := Cached}) when is_map(Cached) -> Cached;
current_settings(Provided, State) ->
    case maps:get(managed,State,false) of
        false ->
            %% Trusted direct callers can supply rules without site configuration.
            case Provided of
                #{ip_lists := Lists} -> Provided#{ip_lists => maps:map(fun(_, E) -> index(E) end, Lists)};
                _ -> Provided
            end;
        true ->
            Context = maps:get(context,State),
            Settings = read_settings(Context),
            ok=z_traffic_guard_rules:replace(maps:get(ip_lists,Settings),Context),
            ets:insert(name(Context),{config,maps:remove(ip_lists,Settings)}),
            Settings
    end.

admit(_, #{mode := off}, State) -> {reply, decision(off,none,0), State};
admit(Client, Settings, State) ->
    Lists = maps:get(ip_lists,Settings,#{allow => #{}, block => #{}}),
    IP = m_traffic_guard:normalize(Client),
    Now = erlang:system_time(second),
    case matches(IP,maps:get(allow,Lists),Now) of
        true -> {reply,decision(maps:get(mode,Settings),ip_allow,0),State};
        false ->
            case matches(IP,maps:get(block,Lists),Now) of
                true -> {reply,decision(maps:get(mode,Settings),ip_block,60),State};
                false -> admit_budget(IP,Settings,State)
            end
    end.

admit_budget(Client, Settings, State) ->
    %% Changing settings resets transient budgets, including mode transitions.
    Clients = case maps:get(settings, State) =:= Settings of
        true -> maps:get(clients, State);
        false -> #{}
    end,
    Existing = maps:get(Client, Clients, undefined),
    case Existing =:= undefined andalso map_size(Clients) >= maps:get(max_clients, Settings) of
        true -> {reply, decision(maps:get(mode, Settings), state_capacity, 0), State};
        false ->
            Now=erlang:monotonic_time(millisecond),
            {Result, Entry} = step(Now, Settings, Existing),
            Tracked=track(Result,Entry,Existing,Now),
            {reply, Result, State#{clients => Clients#{Client => Tracked}, settings => Settings}}
    end.

%% Match deadlines on every request, even while the table copy is cached.
matches(IP,Entries,Now) ->
    case maps:find(IP,Entries) of
        {ok,0} -> true;
        {ok,Until} -> Until > Now;
        error -> false
    end.

%% Duplicate site/global rules match while any entry remains valid. A permanent
%% entry wins; otherwise retain the latest expiry, independent of input order.
-spec index(Entries) -> map() when Entries :: list() | map().
index(Entries) when is_map(Entries) -> Entries;
index(Entries) ->
    lists:foldl(fun
        ({IP,Until},Acc) -> index_entry(IP,Until,Acc);
        (IP,Acc) -> index_entry(IP,0,Acc)
    end,#{},Entries).

index_entry(IP,Until,Acc) ->
    Address = m_traffic_guard:normalize(IP),
    Expiry = case maps:find(Address,Acc) of
        {ok,0} -> 0;
        _ when Until =:= 0 -> 0;
        {ok,Previous} -> max(Previous,Until);
        error -> Until
    end,
    Acc#{Address => Expiry}.

track(#{guard_action:=<<"allow">>},Entry,_,_) -> Entry;
track(Result,Entry,Previous,Now) ->
    Count=case Previous of #{rejections:=N}->N; _->0 end,
    Entry#{throttled_until=>Now+maps:get(retry_after,Result)*1000,
        reason=>maps:get(guard_reason,Result),action=>maps:get(guard_action,Result),
        rejections=>Count+1}.

handle_cast(reset, State) ->
    ok=z_traffic_guard_rules:invalidate(maps:get(context,State)),
    ets:delete(name(maps:get(context,State)), config),
    {noreply, State#{clients => #{}, settings => undefined}};
handle_cast(_, State) -> {noreply, State}.
handle_info(prune, #{clients := Clients} = State) ->
    Now = erlang:monotonic_time(millisecond),
    Kept = maps:filter(fun(_, #{expires := T}) -> T > Now end, Clients),
    erlang:send_after(30000, self(), prune),
    {noreply, State#{clients => Kept}};
handle_info(_, State) -> {noreply, State}.

%% Pure request-rate admission. Observe uses the same shadow state as enforce;
%% rejected requests do not consume credits or prolong depletion.
-spec step(Now, Settings, Previous) -> {map(), map()} when
    Now :: integer(), Settings :: map(), Previous :: undefined | map().
step(Now, Settings, undefined) ->
    step(Now, Settings, #{time => Now,
        requests => maps:get(request_burst, Settings) * maps:get(period, Settings)});
step(Now, S, Previous) ->
    Period = maps:get(period, S),
    Rate = maps:get(request_limit, S),
    Capacity = maps:get(request_burst, S) * Period,
    Elapsed = max(0, Now - maps:get(time, Previous)),
    Credits = min(Capacity, maps:get(requests, Previous) + Elapsed * Rate),
    Wait = wait_time(Credits, Period, Rate),
    {Reason, Remaining} = case Wait of
        0 -> {none, Credits - Period};
        _ -> {request_rate, Credits}
    end,
    %% Never expire debt before the bucket could fully recover.
    TTL = max(Period, (Capacity + Rate - 1) div Rate),
    {decision(maps:get(mode, S), Reason, (Wait + 999) div 1000),
     #{time => Now, expires => Now + TTL, requests => Remaining}}.

wait_time(Tokens, Cost, _) when Tokens >= Cost -> 0;
wait_time(Tokens, Cost, Rate) -> (Cost - Tokens + Rate - 1) div Rate.

-spec decision(Mode, Reason, Retry) -> map() when
    Mode :: off | observe | enforce, Reason :: atom(), Retry :: non_neg_integer().
decision(_, Reason, _) when Reason =:= unavailable; Reason =:= state_capacity ->
    #{crawl_behavior => <<"unknown">>, guard_action => <<"allow">>,
      guard_reason => atom_to_binary(Reason,utf8), retry_after => 0};
decision(_, ip_allow, _) ->
    #{crawl_behavior => <<"normal">>, guard_action => <<"allow">>,
      guard_reason => <<"ip_allow">>, retry_after => 0};
decision(Mode, ip_block, Retry) ->
    (decision(Mode,request_rate,Retry))#{crawl_behavior => <<"unknown">>, guard_reason => <<"ip_block">>};
decision(_, none, _) ->
    #{crawl_behavior => <<"normal">>, guard_action => <<"allow">>,
      guard_reason => <<"none">>, retry_after => 0};
decision(Mode, Reason, Retry) ->
    #{crawl_behavior => <<"excessive">>,
      guard_action => case Mode of enforce -> <<"throttle">>; _ -> <<"would_throttle">> end,
      guard_reason => atom_to_binary(Reason,utf8), retry_after => Retry}.
