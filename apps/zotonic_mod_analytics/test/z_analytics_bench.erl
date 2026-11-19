%% Manual microbenchmark: 20,000 events, 32 matching recordings, 100 groups.
%% Measures ring operations and aggregation only, not HTTP/MQTT/SQL throughput.
-module(z_analytics_bench).
-export([run/0,popular/0]).
run() -> run(<<"key">>,100).
popular() -> run(<<"page">>,10000).
run(Dimension,Distinct) ->
    {ok,_}=application:ensure_all_started(ringbuffer),
    Site=analytics_bench,
    Name=z_utils:name_for_site(analytics_buffer,Site),
    {ok,_}=ringbuffer:new(Name,4096),
    T=erlang:system_time(second),
    Cs=[#{id=>I,state=><<"active">>,event=><<"bench">>,schema_version=>1,
        started=>T-1,filters=>#{},dimension=>Dimension,groups=>100} || I<-lists:seq(1,32)],
    S=#{site=>Site,generation=>1,collectors=>Cs,event_index=>#{{<<"bench">>,1}=>Cs},
        admissions=>#{},admission_minute=>0,deltas=>#{},entries=>0,accepted=>0,dropped=>0,lost=>false},
    {Micros,Final}=timer:tc(fun()-> lists:foldl(fun(B,Acc)->
        [ringbuffer:write(Name,{1,<<"bench">>,1,T,#{Dimension=>integer_to_binary((B*200+I) rem Distinct)}}) || I<-lists:seq(1,200)],
        drain_until(maps:get(accepted,Acc)+6400,Acc)
    end,S,lists:seq(1,100)) end),
    io:format("~p~n",[#{microseconds=>Micros,events=>20000,updates=>maps:get(accepted,Final),
        entries=>maps:get(entries,Final),dropped=>maps:get(dropped,Final)}]),
    ringbuffer:delete(Name).


drain_until(Target,S) ->
    case maps:get(accepted,S) of
        Target -> S;
        _ -> {noreply,Next}=z_analytics_worker:handle_info(drain,S),drain_until(Target,Next)
    end.
