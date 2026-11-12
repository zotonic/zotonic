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

-module(traffic_guard_bench).
-export([run/0, settings/1, ip_rules/0, ip_rules/1]).
-include_lib("zotonic_core/include/zotonic.hrl").

run() ->
    S = #{mode=>observe, period=>60000, request_limit=>100000, request_burst=>1000, max_clients=>5000},
    {_,Entry} = z_traffic_guard:step(0,S,undefined),
    {PureUs,_} = timer:tc(fun()-> pure(50000,S,Entry) end),
    C = #context{site=traffic_guard_benchmark},
    {ok,RulesPid}=z_traffic_guard_rules:start_link([{context,C}]),
    ok=z_traffic_guard_rules:replace(#{allow=>#{},block=>#{}},C),
    {ok,Pid} = z_traffic_guard:start_link([{context,C}]),
    try
        {SequentialUs,_} = timer:tc(fun() ->
            [z_traffic_guard:check(I rem 100,S,C) || I <- lists:seq(1,20000)]
        end),
        Parent = self(),
        {ConcurrentUs,Results} = timer:tc(fun() ->
            Refs = [begin Ref=make_ref(),spawn(fun()->
                Samples = [begin
                    {Us,R}=timer:tc(fun()->z_traffic_guard:check({Client,I rem 10},S,C) end),
                    {Us,maps:get(guard_reason,R)}
                end || I<-lists:seq(1,1000)], Parent!{Ref,Samples}
            end),Ref end || Client<-lists:seq(1,32)],
            lists:append([receive {Ref,R}->R end || Ref<-Refs])
        end),
        Latencies=lists:sort([Us || {Us,_} <- Results]),
        {memory,Memory}=process_info(Pid,memory),
        #{pure_request_ns=>PureUs*1000 div 50000,
          sequential_requests_per_second=>20000000000 div max(1,SequentialUs),
          concurrent_requests_per_second=>32000000000 div max(1,ConcurrentUs),
          concurrent_p95_us=>lists:nth(length(Latencies)*95 div 100,Latencies),
          unavailable=>length([ok || {_,<<"unavailable">>}<-Results]),
          worker_memory_bytes=>Memory}
    after gen_server:stop(Pid),gen_server:stop(RulesPid) end.

pure(0,_,E)->E;
pure(N,S,E)->{_,Next}=z_traffic_guard:step(0,S,E),pure(N-1,S,Next).

settings(C) ->
    {Us,_}=timer:tc(fun()->[z_traffic_guard:settings(C)||_<-lists:seq(1,10000)] end),
    #{settings_ns_per_call=>Us*1000 div 10000}.


%% Compare identical managed-worker workloads across implementations. For a
%% baseline, compile the previous worker under a different module name and pass
%% that module to ip_rules/1. No running site or database is used.
ip_rules() -> ip_rules(z_traffic_guard).
ip_rules(Module) ->
    {module,Module}=code:ensure_loaded(Module),
    [ip_rules(Module,N) || N <- [0,10,256,1024]].

ip_rules(Module,N) ->
    Entries=[{{10,I div 65536,(I div 256) rem 256,I rem 256},0} || I<-lists:seq(1,N)],
    Indexed=erlang:function_exported(Module,index,1),
    Rules=case Indexed of true -> Module:index(Entries); false -> Entries end,
    BlockEntries=[{{11,A,B,D},Until} || {{10,A,B,D},Until}<-Entries],
    Blocks=case Indexed of true -> Module:index(BlockEntries); false -> BlockEntries end,
    S=#{mode=>observe,period=>60000,request_limit=>100000,request_burst=>1000,max_clients=>5000,
        ip_lists=>#{allow=>Rules,block=>Blocks}},
    Public=case Indexed of true -> maps:remove(ip_lists,S); false -> S end,
    C=#context{site=traffic_guard_ip_benchmark},
    {ok,RulesPid}=z_traffic_guard_rules:start_link([{context,C}]),
    ok=z_traffic_guard_rules:replace(#{allow=>Module:index(Entries),block=>Module:index(BlockEntries)},C),
    {ok,P}=Module:start_link([{context,C}]),
    try
        sys:replace_state(P,fun(State) ->
            ets:insert(z_utils:name_for_site(traffic_guard,z_context:site(C)),{config,Public}),
            State#{settings=>S,managed=>true}
        end),
        Miss={192,0,2,1}, First={10,0,0,1}, Now=erlang:system_time(second),
        %% Warm the worker before measuring; return no large result lists.
        repeat(1000,fun()->Module:check(Miss,Public,C) end),
        {Scan,_}=timer:tc(fun()->repeat(100000,fun()->Module:matches(Miss,Rules,Now) end) end),
        {Lookup,_}=timer:tc(fun()->repeat(20000,fun()->Module:settings(C) end) end),
        {Check,_}=timer:tc(fun()->repeat(20000,fun()->
            Module:check(Miss,Module:settings(C),C) end) end),
        {Allow,_}=timer:tc(fun()->repeat(20000,fun()->
            Module:check(First,Module:settings(C),C) end) end),
        {Block,_}=timer:tc(fun()->repeat(20000,fun()->
            Module:check({11,0,0,1},Module:settings(C),C) end) end),
        #{entries_per_list=>N,block_check_us=>Block/20000,miss_lookup_ns=>Scan/100,settings_ns=>Lookup/20,
          settings_and_check_us=>Check/20000,first_allow_check_us=>Allow/20000}
    after gen_server:stop(P),gen_server:stop(RulesPid) end.

repeat(0,_)->ok;
repeat(N,F)->F(),repeat(N-1,F).
