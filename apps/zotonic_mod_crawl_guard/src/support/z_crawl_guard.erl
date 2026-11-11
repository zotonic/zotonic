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

-module(z_crawl_guard).
-behaviour(gen_server).
-export([start_link/1, check/4, settings/1, mode/1, reset/1, step/4, decision/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-include_lib("zotonic_core/include/zotonic.hrl").

start_link(Args) ->
    Context = proplists:get_value(context, Args),
    gen_server:start_link({local, name(Context)}, ?MODULE, [Context], []).
reset(Context) -> gen_server:cast(name(Context), reset).
name(Context) -> z_utils:name_for_site(crawl_guard, z_context:site(Context)).

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
    #{mode => mode(m_config:get_value(mod_crawl_guard, mode, Context)),
      period => integer_config(period, 60, 1, 3600, Context) * 1000,
      request_limit => integer_config(request_limit, 120, 1, 100000, Context),
      request_burst => integer_config(request_burst, 30, 1, 1000, Context),
      distinct_limit => integer_config(distinct_limit, 60, 1, 100000, Context),
      distinct_burst => integer_config(distinct_burst, 20, 1, 64, Context),
      max_clients => integer_config(max_clients, 5000, 1, 50000, Context)}.

integer_config(Key, Default, Min, Max, Context) ->
    try z_convert:to_integer(m_config:get_value(mod_crawl_guard, Key, Default, Context)) of
        N when is_integer(N), N >= Min, N =< Max -> N;
        _ -> Default
    catch _:_ -> Default end.

%% No cookie or user-agent component: rotating them must not reset a budget.
%% The mailbox check sheds work before the limiter becomes a bottleneck.
-spec check(Client, Target, Settings, Context) -> map() when
    Client :: term(), Target :: binary(), Settings :: map(), Context :: z:context().
check(_, _, #{mode := off}, _) -> decision(off, none, 0);
check(Client, Target, Settings, Context) ->
    try
        Pid = whereis(name(Context)),
        {message_queue_len, N} = process_info(Pid, message_queue_len),
        true = N < 100,
        gen_server:call(Pid, {check, Client, Target, Settings,
            erlang:monotonic_time(millisecond) + 1000}, 1000)
    catch _:_ -> decision(maps:get(mode, Settings), unavailable, 0) end.

init([Context]) ->
    ets:new(name(Context), [named_table, protected, set, {read_concurrency,true}]),
    erlang:send_after(30000, self(), prune),
    {ok, #{clients => #{}, settings => undefined, context => Context, managed => false}}.

handle_call(settings, _, State) ->
    Settings = current_settings(undefined, State#{managed => true}),
    {reply, Settings, State#{managed => true}};
handle_call({check, Client, Target, Settings, Deadline}, _, State) ->
    case erlang:monotonic_time(millisecond) >= Deadline of
        true -> {reply, decision(maps:get(mode, Settings), unavailable, 0), State};
        false -> admit(Client, Target, current_settings(Settings,State), State)
    end;
handle_call(_, _, State) -> {reply, {error, badarg}, State}.

%% Once used by HTTP, configuration is authoritative in this worker. A request
%% queued before an update cannot switch back to old limits and reset budgets.
current_settings(_, #{managed := true, settings := Cached}) when is_map(Cached) -> Cached;
current_settings(Provided, State) ->
    case maps:get(managed,State,false) of
        false -> Provided;
        true ->
            Context = maps:get(context,State), Table = name(Context),
            case ets:lookup(Table,config) of
                [{config,Settings}] -> Settings;
                [] ->
                    Settings = read_settings(Context),
                    ets:insert(Table,{config,Settings}),
                    Settings
            end
    end.

admit(_, _, #{mode := off}, State) -> {reply, decision(off,none,0), State};
admit(Client, Target, Settings, State) ->
    %% Changing settings resets transient budgets, including mode transitions.
    Clients = case maps:get(settings, State) =:= Settings of
        true -> maps:get(clients, State);
        false -> #{}
    end,
    Existing = maps:get(Client, Clients, undefined),
    case Existing =:= undefined andalso map_size(Clients) >= maps:get(max_clients, Settings) of
        true -> {reply, decision(maps:get(mode, Settings), state_capacity, 0), State};
        false ->
            {Result, Entry} = step(Target, erlang:monotonic_time(millisecond), Settings, Existing),
            {reply, Result, State#{clients => Clients#{Client => Entry}, settings => Settings}}
    end.

handle_cast(reset, State) ->
    ets:delete(name(maps:get(context,State)), config),
    {noreply, State#{clients => #{}, settings => undefined}};
handle_cast(_, State) -> {noreply, State}.
handle_info(prune, #{clients := Clients} = State) ->
    Now = erlang:monotonic_time(millisecond),
    Kept = maps:filter(fun(_, #{expires := T}) -> T > Now end, Clients),
    erlang:send_after(30000, self(), prune),
    {noreply, State#{clients => Kept}};
handle_info(_, State) -> {noreply, State}.

%% Pure token-bucket admission. Observe uses the same shadow admission state as
%% enforce; rejected requests do not prolong depletion. Targets are short hashes.
-spec step(Target, Now, Settings, Previous) -> {map(), map()} when
    Target :: binary(), Now :: integer(), Settings :: map(),
    Previous :: undefined | map().
step(Target, Now, Settings, undefined) ->
    step(Target, Now, Settings, #{time => Now, targets => #{},
        requests => maps:get(request_burst, Settings) * maps:get(period, Settings),
        distinct => maps:get(distinct_burst, Settings) * maps:get(period, Settings)});
step(Target, Now, S, Previous) ->
    Period = maps:get(period, S),
    Elapsed = max(0, Now - maps:get(time, Previous)),
    R = min(maps:get(request_burst,S)*Period,
        maps:get(requests,Previous) + Elapsed*maps:get(request_limit,S)),
    D = min(maps:get(distinct_burst,S)*Period,
        maps:get(distinct,Previous) + Elapsed*maps:get(distinct_limit,S)),
    {Targets, NextExpiry} = live_targets(Previous, Now),
    New = not maps:is_key(Target, Targets),
    RWait = wait_time(R, Period, maps:get(request_limit,S)),
    DWait = case New of true -> wait_time(D,Period,maps:get(distinct_limit,S)); false -> 0 end,
    {Reason, Wait} = case {RWait,DWait} of
        {0,0} -> {none,0};
        {RW,DW} when RW >= DW -> {request_rate,RW};
        {_,DW} -> {distinct_targets,DW}
    end,
    Accepted = Wait =:= 0,
    {Targets1, NextExpiry1} = case Accepted andalso New andalso map_size(Targets) < 64 of
        true -> {Targets#{Target => Now + Period}, min_expiry(NextExpiry, Now + Period)};
        false -> {Targets, NextExpiry}
    end,
    %% Never expire debt before it would have recovered naturally.
    TTL = max(Period, max(
        (maps:get(request_burst,S)*Period + maps:get(request_limit,S)-1) div maps:get(request_limit,S),
        (maps:get(distinct_burst,S)*Period + maps:get(distinct_limit,S)-1) div maps:get(distinct_limit,S))),
    {decision(maps:get(mode,S), Reason, (Wait+999) div 1000),
     #{time => Now, expires => Now + TTL, targets => Targets1, target_expiry => NextExpiry1,
       requests => R - case Accepted of true -> Period; false -> 0 end,
       distinct => D - case Accepted andalso New of true -> Period; false -> 0 end}}.

%% Repeated visits do not need to scan/copy all 64 remembered targets.
%% Only prune when the earliest expiration has actually arrived.
live_targets(#{targets := Targets, target_expiry := Expiry}, Now)
        when is_integer(Expiry), Expiry > Now -> {Targets, Expiry};
live_targets(#{targets := Targets}, Now) ->
    Kept = maps:filter(fun(_, T) -> T > Now end, Targets),
    Expiry = maps:fold(fun(_, T, E) -> min_expiry(E,T) end, undefined, Kept),
    {Kept, Expiry}.

min_expiry(undefined, T) -> T;
min_expiry(E, T) -> min(E,T).

wait_time(Tokens, Cost, _) when Tokens >= Cost -> 0;
wait_time(Tokens, Cost, Rate) -> (Cost - Tokens + Rate - 1) div Rate.

-spec decision(Mode, Reason, Retry) -> map() when
    Mode :: off | observe | enforce, Reason :: atom(), Retry :: non_neg_integer().
decision(_, Reason, _) when Reason =:= unavailable; Reason =:= state_capacity ->
    #{crawl_behavior => <<"unknown">>, guard_action => <<"allow">>,
      guard_reason => atom_to_binary(Reason,utf8), retry_after => 0};
decision(_, none, _) ->
    #{crawl_behavior => <<"normal">>, guard_action => <<"allow">>,
      guard_reason => <<"none">>, retry_after => 0};
decision(Mode, Reason, Retry) ->
    #{crawl_behavior => <<"excessive">>,
      guard_action => case Mode of enforce -> <<"throttle">>; _ -> <<"would_throttle">> end,
      guard_reason => atom_to_binary(Reason,utf8), retry_after => Retry}.
