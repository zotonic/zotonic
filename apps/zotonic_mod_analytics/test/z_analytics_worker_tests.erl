-module(z_analytics_worker_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

%% Exercise the real bounded ring and drain path without a database or live site.
burst_test() ->
    with_buffer(fun(Name,S,T) ->
        [ringbuffer:write(Name,event(T,I)) || I <- lists:seq(1,10000)],
        ?assert(ets:info(Name,size)=<4100),
        {noreply,S1}=z_analytics_worker:handle_info(drain,S),
        ?assert(maps:get(accepted,S1)=<200),
        %% A full chunk schedules another turn promptly, rather than waiting 100 ms.
        receive drain -> ok after 1000 -> error(drain_not_rescheduled) end,
        S2=drain_until_accounted(S1,10000,10000),
        ?assertEqual(4096,maps:get(accepted,S2)),
        ?assertEqual(5904,maps:get(dropped,S2)),
        ?assert(maps:get(entries,S2)=<3),
        ?assertEqual(4096,lists:sum([lists:sum(maps:values(V)) || V <- maps:values(maps:get(deltas,S2))])),
        ?assert(maps:get(lost,S2))
    end).

inactive_and_invalid_events_test() ->
    with_buffer(fun(Name,S,T) ->
        C=#context{site=analytics_worker_test},
        Table=z_utils:name_for_site(analytics_config,analytics_worker_test),
        ets:new(Table,[named_table,set]),
        Schema=#{version=>1,dimensions=>#{<<"key">>=>text}},
        try
            ets:insert(Table,{{event,<<"bench">>},1,Schema,false}),
            ?assertNot(z_analytics_worker:interested(<<"bench">>,C)),
            ok=z_analytics_worker:ingest(<<"bench">>,#{<<"key">>=><<"x">>},T,C),
            ?assertEqual({error,empty},ringbuffer:read(Name)),
            ets:insert(Table,{{event,<<"bench">>},1,Schema,true}),
            ?assert(z_analytics_worker:interested(<<"bench">>,C)),
            ?assertEqual({error,invalid_properties},z_analytics_worker:ingest(<<"bench">>,#{<<"secret">>=><<"x">>},T,C)),
            ok=z_analytics_worker:ingest(<<"bench">>,#{<<"key">>=><<"x">>},T,C),
            {noreply,Final}=z_analytics_worker:handle_info(drain,S),
            ?assertEqual(1,maps:get(accepted,Final))
        after ets:delete(Table) end
    end).

stale_and_unrelated_events_test() ->
    with_buffer(fun(Name,S,T) ->
        ringbuffer:write(Name,{0,<<"bench">>,1,T,#{}}),
        ringbuffer:write(Name,event(T-121,1)),
        ringbuffer:write(Name,{1,<<"unrelated">>,1,T,#{}}),
        {noreply,Final}=z_analytics_worker:handle_info(drain,S),
        ?assertEqual(0,maps:get(accepted,Final)),
        ?assertEqual(2,maps:get(dropped,Final))
    end).

with_buffer(F) ->
    {ok,_}=application:ensure_all_started(ringbuffer),
    Site=analytics_worker_test,
    Name=z_utils:name_for_site(analytics_buffer,Site),
    {ok,_}=ringbuffer:new(Name,4096),
    T=erlang:system_time(second),
    Col=#{id=>1,state=><<"active">>,event=><<"bench">>,schema_version=>1,
        started=>T-200,filters=>#{},dimension=><<"key">>,groups=>3},
    S=#{site=>Site,generation=>1,collectors=>[Col],event_index=>#{{<<"bench">>,1}=>[Col]},
        admissions=>#{},deltas=>#{},entries=>0,accepted=>0,dropped=>0,lost=>false},
    try F(Name,S,T) after ringbuffer:delete(Name),flush_timers() end.

event(T,I) -> {1,<<"bench">>,1,T,#{<<"key">>=>integer_to_binary(I rem 10)}}.
flush_timers() -> receive drain -> flush_timers() after 0 -> ok end.


drain_until_accounted(S,Total,Turns) when Turns > 0 ->
    case maps:get(accepted,S)+maps:get(dropped,S) of
        Total -> S;
        _ -> {noreply,Next}=z_analytics_worker:handle_info(drain,S),
             drain_until_accounted(Next,Total,Turns-1)
    end;
drain_until_accounted(_,_,0) -> error(drain_did_not_finish).

idle_and_single_flight_test() ->
    with_buffer(fun(_,S,T) ->
        S0=S#{pending=>undefined,seq=>0,epoch=><<"test">>,from=>T-5},
        {noreply,Idle}=z_analytics_worker:handle_info(flush,S0#{event_index=>#{}}),
        ?assertEqual(undefined,maps:get(pending,Idle)),
        ?assertEqual(0,maps:get(seq,Idle)),
        {noreply,Busy}=z_analytics_worker:handle_info(flush,S0),
        ?assertMatch({1,_,_},maps:get(pending,Busy)),
        {noreply,StillBusy}=z_analytics_worker:handle_info(flush,Busy),
        ?assertEqual(maps:get(pending,Busy),maps:get(pending,StillBusy)),
        ?assertEqual(1,maps:get(seq,StillBusy))
    end).


expired_retry_test() ->
    with_buffer(fun(_,S,T) ->
        {noreply,Final}=z_analytics_worker:handle_info(retry,
            S#{pending=>{1,#{until=>T-121},[]}}),
        ?assertEqual(undefined,maps:get(pending,Final)),
        ?assertEqual(1,maps:get(dropped,Final)),
        ?assert(maps:get(lost,Final))
    end).

%% More distinct pages than capacity, followed by a newly popular page. Exercise
%% the real ring/worker path, including capacity reservation and batch merging.
popular_content_test() ->
    with_buffer(fun(Name,S,T) ->
        [Col0]=maps:get(collectors,S), Col=Col0#{dimension=><<"page">>,groups=>100},
        S0=S#{collectors=>[Col],event_index=>#{{<<"bench">>,1}=>[Col]}},
        S1=lists:foldl(fun(B,A) ->
            [ringbuffer:write(Name,{1,<<"bench">>,1,T,#{<<"page">>=>integer_to_binary(I)}})
                || I<-lists:seq(B*200+1,B*200+200)],
            drain_until_accounted(A,(B+1)*200,1000)
        end,S0,lists:seq(0,49)),
        [ringbuffer:write(Name,{1,<<"bench">>,1,T,#{<<"page">>=><<"winner">>}}) || _<-lists:seq(1,2000)],
        S2=drain_until_accounted(S1,12000,1000),
        [Counts]=maps:values(maps:get(deltas,S2)),
        ?assertEqual(100,maps:get(entries,S2)),
        ?assertEqual(0,maps:get(dropped,S2)),
        ?assertEqual(12000,z_analytics_frequency:total(Counts)),
        [#{key:= <<"v:winner">>}|_]=z_analytics_frequency:ranked(Counts)
    end).

%% Notification hooks keep only outcomes; delayed final success is not a send.
smtp_notifications_test() ->
    with_buffer(fun(_Name,S,T) ->
        C=#context{site=analytics_worker_test},
        Table=z_utils:name_for_site(analytics_config,analytics_worker_test),
        ets:new(Table,[named_table,set]),
        Schema=#{version=>1,dimensions=>#{<<"outcome">>=>[<<"sent">>,<<"retry">>,<<"failed">>,<<"bounced">>]}},
        [Col0]=maps:get(collectors,S),
        Col=Col0#{event=><<"smtp">>,dimension=><<"outcome">>,groups=>5},
        S0=S#{collectors=>[Col],event_index=>#{{<<"smtp">>,1}=>[Col]}},
        try
            ets:insert(Table,{{event,<<"smtp">>},1,Schema,true}),
            mod_analytics:observe_email_sent(#email_sent{is_final=false,recipient= <<"private@example.test">>},C),
            mod_analytics:observe_email_sent(#email_sent{is_final=true},C),
            mod_analytics:observe_email_failed(#email_failed{is_final=false,reason=retry,status= <<"private response">>},C),
            mod_analytics:observe_email_failed(#email_failed{is_final=true,reason=error},C),
            mod_analytics:observe_email_failed(#email_failed{is_final=true,reason=bounce},C),
            mod_analytics:observe_email_bounced(#email_bounced{message_nr= <<"private-id">>},C),
            Final=drain_until_accounted(S0,5,1000),
            Counts=lists:foldl(fun z_analytics:merge/2,#{},maps:values(maps:get(deltas,Final))),
            ?assertEqual(#{<<"v:sent">>=>1,<<"v:retry">>=>1,<<"v:failed">>=>1,<<"v:bounced">>=>2},Counts),
            ?assertEqual(0,maps:get(dropped,Final)),
            ?assert(T=<erlang:system_time(second))
        after ets:delete(Table) end
    end).
