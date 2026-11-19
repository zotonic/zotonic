%% Run in an isolated VM with +S 2:2, never on the running site node.
%% Concurrent producers and CPU contention exercise actual gen_server scheduling.
-module(z_analytics_load_bench).
-export([run/0]).
run() ->
    {ok,_}=application:ensure_all_started(ringbuffer),
    Site=analytics_load_bench,
    Name=z_utils:name_for_site(analytics_buffer,Site),
    {ok,_}=ringbuffer:new(Name,4096),
    T=erlang:system_time(second),
    Cs=[#{id=>I,state=><<"active">>,event=><<"bench">>,schema_version=>1,
        started=>T-1,filters=>#{},dimension=><<"key">>,groups=>100} || I<-lists:seq(1,32)],
    S=#{site=>Site,generation=>1,collectors=>Cs,event_index=>#{{<<"bench">>,1}=>Cs},
        admissions=>#{},deltas=>#{},entries=>0,accepted=>0,dropped=>0,lost=>false,
        pending=>undefined,failures=>0,last_flush=>undefined},
    Worker=proc_lib:spawn(fun()->gen_server:enter_loop(z_analytics_worker,[],S) end),
    Burner=spawn(fun()->busy(1) end),
    try
        Worker ! drain,
        Producers=[spawn_monitor(fun()->
            lists:foreach(fun(_)->
                lists:foreach(fun(I)->ringbuffer:write(Name,
                    {1,<<"bench">>,1,T,#{<<"key">>=>integer_to_binary(I rem 100)}})
                end,lists:seq(1,100)),
                timer:sleep(10)
            end,lists:seq(1,100))
        end) || _<-lists:seq(1,8)],
        Samples=[sample(Worker,Name) || _<-lists:seq(1,300)],
        [receive {'DOWN',Ref,process,Pid,normal}->ok after 10000->error(producer_timeout) end
            || {Pid,Ref}<-Producers],
        Final=await_accounted(Worker,100),
        Latencies=lists:sort([Us || {Us,_,_,_}<-Samples]),
        io:format("~p~n",[#{produced=>80000,
            consumed=>maps:get(accepted,Final) div 32,dropped=>maps:get(dropped,Final),
            counters=>maps:get(entries,Final),
            status_p95_us=>lists:nth(285,Latencies),status_max_us=>lists:last(Latencies),
            peak_worker_bytes=>lists:max([M || {_,M,_,_}<-Samples]),
            peak_mailbox=>lists:max([Q || {_,_,Q,_}<-Samples]),
            peak_ring_rows=>lists:max([R || {_,_,_,R}<-Samples])}])
    after exit(Worker,kill),exit(Burner,kill),ringbuffer:delete(Name) end.

sample(Worker,Name) ->
    {Us,_}=timer:tc(fun()->gen_server:call(Worker,status,5000) end),
    [{memory,M},{message_queue_len,Q}]=process_info(Worker,[memory,message_queue_len]),
    R=ets:info(Name,size),
    timer:sleep(5),
    {Us,M,Q,R}.
await_accounted(Worker,N) when N>0 ->
    S=gen_server:call(Worker,status,5000),
    case maps:get(accepted,S) div 32+maps:get(dropped,S) of
        80000 -> S;
        _ -> timer:sleep(50),await_accounted(Worker,N-1)
    end;
await_accounted(_,0) -> error(events_not_accounted).
busy(N) -> busy(erlang:phash2({N,lists:seq(1,1000)})).
