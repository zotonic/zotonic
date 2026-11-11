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

-module(crawl_guard_bench).
-export([run/0, settings/1]).
-include_lib("zotonic_core/include/zotonic.hrl").

run() ->
    S = #{mode=>observe, period=>60000, request_limit=>100000, request_burst=>1000,
        distinct_limit=>100000, distinct_burst=>64, max_clients=>5000},
    Hashes = [crypto:hash(sha256,integer_to_binary(I)) || I <- lists:seq(1,64)],
    Entry = lists:foldl(fun(H,E) -> {_,Next}=z_crawl_guard:step(H,0,S,E), Next end,undefined,Hashes),
    H = hd(Hashes),
    {PureUs,_} = timer:tc(fun()-> pure(50000,H,S,Entry) end),
    C = #context{site=crawl_guard_benchmark},
    {ok,Pid} = z_crawl_guard:start_link([{context,C}]),
    try
        {SequentialUs,_} = timer:tc(fun() ->
            [z_crawl_guard:check(I rem 100,H,S,C) || I <- lists:seq(1,20000)]
        end),
        Parent = self(),
        {ConcurrentUs,Results} = timer:tc(fun() ->
            Refs = [begin Ref=make_ref(),spawn(fun()->
                Samples = [begin
                    {Us,R}=timer:tc(fun()->z_crawl_guard:check({Client,I rem 10},H,S,C) end),
                    {Us,maps:get(guard_reason,R)}
                end || I<-lists:seq(1,1000)], Parent!{Ref,Samples}
            end),Ref end || Client<-lists:seq(1,32)],
            lists:append([receive {Ref,R}->R end || Ref<-Refs])
        end),
        Latencies=lists:sort([Us || {Us,_} <- Results]),
        {memory,Memory}=process_info(Pid,memory),
        #{pure_64_targets_ns=>PureUs*1000 div 50000,
          sequential_requests_per_second=>20000000000 div max(1,SequentialUs),
          concurrent_requests_per_second=>32000000000 div max(1,ConcurrentUs),
          concurrent_p95_us=>lists:nth(length(Latencies)*95 div 100,Latencies),
          unavailable=>length([ok || {_,<<"unavailable">>}<-Results]),
          worker_memory_bytes=>Memory}
    after gen_server:stop(Pid) end.

pure(0,_,_,E)->E;
pure(N,H,S,E)->{_,Next}=z_crawl_guard:step(H,0,S,E),pure(N-1,H,S,Next).

settings(C) ->
    {Us,_}=timer:tc(fun()->[z_crawl_guard:settings(C)||_<-lists:seq(1,10000)] end),
    #{settings_ns_per_call=>Us*1000 div 10000}.
