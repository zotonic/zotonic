%% Run explicitly against a development site: analytics_integration_tests:run(C).
%% Own collectors/receipts are removed; existing site analytics are untouched.
-module(analytics_integration_tests).
-export([run/1,popular_content/1,events/3,ordering/1,access_control/1,test_acl/2,lock_contention/1]).
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("eunit/include/eunit.hrl").

run(C) ->
    development=m_site:environment(C),
    Epoch=crypto:strong_rand_bytes(16),
    Registry=z_analytics:registry(C),
    {ok,D}=z_analytics:definition(#{<<"name">>=><<"Analytics integration test">>,
        <<"event">>=><<"page_request">>,<<"dimension">>=><<"language">>,<<"groups">>=>3,
        <<"filters">>=>#{<<"page">>=>[base64:encode(Epoch)]}},Registry),
    {ok,Id}=m_analytics:store_create(D,<<"active">>,C),
    T=z_analytics:bucket(erlang:system_time(second),60)-60,
    Col=D#{id=>Id,state=><<"active">>,started=>T},
    B=#{from=>T,until=>T+60,lost=>false,deltas=>#{{Id,T}=>#{<<"v:nl">>=>2}}},
    try
        ok=m_analytics:store_install(C),
        %% Presentation metadata must not permit duplicate measurement streams,
        %% including pre-description definitions that lack this optional key.
        ?assertEqual({error,{exists,Id}},m_analytics:store_create(
            D#{description=><<"Another description">>},<<"active">>,C)),
        ?assertEqual({error,{exists,Id}},m_analytics:store_create(
            maps:remove(description,D),<<"active">>,C)),
        {ok,{EmptyInfo,[]}}=m_analytics:m_get([<<"info">>,Id],#{},z_acl:sudo(C)),
        ?assertEqual(0,maps:get(buckets,EmptyInfo)),
        ?assertEqual(3578*3,maps:get(reserved,EmptyInfo)),
        ?assertEqual(4,length(maps:get(tiers,EmptyInfo))),
        ?assertEqual({error,enoent},m_analytics:m_get([<<"info">>,<<"invalid">>],#{},z_acl:sudo(C))),
        ok=ordering(C),
        ok=m_analytics:store_flush(Epoch,1,B,[Col],C),
        ok=m_analytics:store_flush(Epoch,1,B,[Col],C),
        [First]=m_analytics:store_rows(Id,60,T,T+60,C),
        ?assertEqual(#{<<"v:nl">>=>2},maps:get(counts,First)),
        ?assertEqual(true,maps:get(complete,First)),
        {ok,{Info,[]}}=m_analytics:m_get([<<"info">>,Id],#{},z_acl:sudo(C)),
        ?assertEqual(4,maps:get(buckets,Info)),
        ?assert(maps:get(payload_bytes,Info)>maps:get(payload_bytes,EmptyInfo)),
        ?assertEqual([1,1,1,1],[maps:get(buckets,X)||X<-maps:get(tiers,Info)]),
        %% An empty follow-up covering the same interval must not rewrite the
        %% unchanged minute/parent rows (and create avoidable MVCC/WAL churn).
        Versions=z_db:q("select resolution,xmin::text from analytics_bucket where collector_id=$1 order by resolution",[Id],C),
        ok=m_analytics:store_flush(Epoch,90,B#{deltas=>#{}},[Col],C),
        ?assertEqual(Versions,z_db:q("select resolution,xmin::text from analytics_bucket where collector_id=$1 order by resolution",[Id],C)),
        Deltas=maps:from_list([{<<"v:",(integer_to_binary(I))/binary>>,1}||I<-lists:seq(1,20)]),
        ok=m_analytics:store_flush(Epoch,2,B#{deltas=>#{{Id,T}=>Deltas}},[Col],C),
        lists:foreach(fun({R,_})->
            Start=z_analytics:bucket(T,R),
            [Row]=m_analytics:store_rows(Id,R,Start,Start+R,C),
            Counts=maps:get(counts,Row),
            ?assert(map_size(Counts)=<3),
            ?assertEqual(22,lists:sum(maps:values(Counts)))
        end,z_analytics:tiers()),
        Report=maps:get(report,m_analytics:dashboard(Id,<<"day">>,z_acl:sudo(C))),
        ?assertEqual(22,maps:get(total,Report)),
        ?assertEqual(22,lists:sum([maps:get(count,G)||G<-maps:get(groups,Report)])),
        ?assert(length(maps:get(groups,Report))=<3),
        ?assertEqual({error,eacces},m_analytics:m_get([<<"recordings">>],#{},z_acl:anondo(C))),
        ?assertEqual({error,eacces},m_analytics:create(#{},<<"active">>,z_acl:anondo(C))),
        %% An expired generation must not overwrite its modulo successor.
        Later=T+360*60,
        ok=m_analytics:store_flush(Epoch,3,B#{from=>Later,until=>Later+60,
            deltas=>#{{Id,Later}=>#{<<"v:en">>=>7}}},[Col],C),
        ok=m_analytics:store_flush(Epoch,4,B#{until=>Later+60},[Col],C),
        [Newest]=m_analytics:store_rows(Id,60,Later,Later+60,C),
        ?assertEqual(7,lists:sum(maps:values(maps:get(counts,Newest)))),
        Reserved=maps:get(reserved,m_analytics:store_usage(C)),
        ok=m_analytics:purge(Id,z_acl:sudo(C)),
        ?assertEqual(Reserved-3578*3,maps:get(reserved,m_analytics:store_usage(C))),
        ?assertNot(lists:any(fun(X)->maps:get(id,X)=:=Id end,m_analytics:store_collectors(C))),
        %% A frozen writer batch must not resurrect a deleted recording.
        ok=m_analytics:store_flush(Epoch,5,B,[Col],C),
        lists:foreach(fun({R,_})->
            ?assertEqual([],m_analytics:store_rows(Id,R,0,Later+86400,C))
        end,z_analytics:tiers()),
        ok
    after
        m_analytics:store_purge(Id,C),
        z_db:q("delete from analytics_batch where epoch=$1",[Epoch],C)
    end.

events(_,Acc,_) ->
    [#{name=><<"mod_analytics_test:sent">>,version=>1,title=><<"Test sent">>,
       dimensions=>#{<<"kind">>=>[<<"digest">>,<<"single">>],<<"language">>=>text}}|Acc].

%% Restores the site's order, including on failure; no history is changed.
ordering(C) ->
    development=m_site:environment(C),
    Admin=z_acl:sudo(C),
    Original=[maps:get(id,X)||X<-m_analytics:store_collectors(C)],
    Reversed=lists:reverse(Original),
    try
        ?assertEqual({error,eacces},m_analytics:reorder(Reversed,z_acl:anondo(C))),
        ?assertEqual({error,eacces},m_analytics:configuration(z_acl:anondo(C))),
        ?assertEqual({error,eacces},m_analytics:overview(<<"day">>,z_acl:anondo(C))),
        ok=m_analytics:reorder(Reversed,Admin),
        ok=m_analytics:store_install(C),
        ?assertEqual(Reversed,[maps:get(id,X)||X<-m_analytics:store_collectors(C)]),
        Visible=[maps:get(id,X)||X<-m_analytics:store_collectors(C),maps:get(state,X)=/= <<"paused">>],
        Initial=m_analytics:overview(<<"week">>,Admin),
        ?assertEqual(length(Visible),length(maps:get(panels,Initial))),
        ?assertEqual(<<"week">>,maps:get(period,Initial)),
        All=m_analytics:overview(<<"day">>,Admin),
        Panels=maps:get(panels,All),
        ?assertEqual(Visible,[maps:get(id,maps:get(collector,P))||P<-Panels]),
        ?assertEqual({error,stale_order},m_analytics:reorder([0|Reversed],Admin)),
        case Original of
            [Id|_] -> ?assertEqual({error,stale_order},m_analytics:reorder([Id|Reversed],Admin));
            [] -> ok
        end,
        ?assertEqual(Reversed,[maps:get(id,X)||X<-m_analytics:store_collectors(C)]),
        ok
    after
        ok=m_analytics:reorder(Original,Admin)
    end.

%% Synthetic contexts exercise independent permissions without changing site ACLs.
access_control(C) ->
    development=m_site:environment(C),
    ok=z_notifier:observe(acl_is_allowed,{?MODULE,test_acl},-100,C),
    try
        Viewer=C#context{user_id=-101,acl={analytics_test,viewer}},
        Config=C#context{user_id=-102,acl={analytics_test,config}},
        Denied=C#context{user_id=-103,acl={analytics_test,denied}},
        lists:foreach(fun({Context,CanView,CanConfigure}) ->
            z_memo:flush(Context),
            {ViewAllowed,_}=controller_admin_analytics:is_authorized(Context),
            {ConfigAllowed,_}=controller_admin_analytics:is_authorized(z_context:set(is_config,true,Context)),
            ?assertEqual(CanView,ViewAllowed),
            ?assertEqual(CanConfigure,ConfigAllowed),
            lists:foreach(fun(Path) ->
                Read=m_analytics:m_get([Path],#{},Context),
                case CanView of
                    true -> ?assertMatch({ok,_},Read);
                    false -> ?assertEqual({error,eacces},Read)
                end
            end,[<<"recordings">>,<<"overview">>,<<"dashboard">>]),
            lists:foreach(fun(Path) ->
                Read=m_analytics:m_get([Path],#{},Context),
                case CanConfigure of
                    true -> ?assertMatch({ok,_},Read);
                    false -> ?assertEqual({error,eacces},Read)
                end
            end,[<<"configuration">>,<<"status">>,<<"registry">>]),
            ?assertEqual(case CanConfigure of true -> {error,enoent}; false -> {error,eacces} end,
                m_analytics:m_get([<<"info">>,0],#{},Context)),
            case CanConfigure of
                true -> ok;
                false ->
                    ?assertEqual({error,eacces},m_analytics:create(#{},<<"active">>,Context)),
                    ?assertEqual({error,eacces},m_analytics:set_state(1,<<"paused">>,Context)),
                    ?assertEqual({error,eacces},m_analytics:reorder([],Context)),
                    ?assertEqual({error,eacces},m_analytics:purge(1,Context))
            end
        end,[{Viewer,true,false},{Config,false,true},{Denied,false,false},
             {z_acl:sudo(C),true,true}]),
        ok
    after
        z_notifier:detach(acl_is_allowed,C),
        z_memo:flush(C)
    end.

test_acl(#acl_is_allowed{action=use,object=mod_analytics},#context{acl={analytics_test,Role}}) ->
    Role =:= viewer;
test_acl(#acl_is_allowed{action=use,object=mod_admin_config},#context{acl={analytics_test,Role}}) ->
    Role =:= config;
test_acl(_,#context{acl={analytics_test,_}}) -> false;
test_acl(_,_) -> undefined.


%% Explicit development-only contention check; holds the budget row briefly.
lock_contention(C) ->
    development=m_site:environment(C),
    Parent=self(), Ref=make_ref(), Epoch=crypto:strong_rand_bytes(16),
    Holder=spawn(fun()->
        Result=z_db:transaction(fun(Tx)->
            z_db:q("select id from analytics_budget where id=1 for update",Tx),
            Parent ! {Ref,locked},
            receive {Ref,release}->ok after 2000->ok end
        end,C),
        Parent ! {Ref,released,Result}
    end),
    try
        receive {Ref,locked}->ok after 3000->error(lock_not_acquired) end,
        Now=erlang:system_time(second),
        Batch=#{from=>Now,until=>Now,deltas=>#{},lost=>false},
        {Micros,Result}=timer:tc(fun()->m_analytics:store_flush(Epoch,1,Batch,[],C) end),
        ?assertMatch({rollback,_},Result),
        ?assert(Micros<1500000),
        ?assertEqual(0,z_db:q1("select count(*) from analytics_batch where epoch=$1",[Epoch],C)),
        Holder ! {Ref,release},
        receive {Ref,released,ok}->ok after 3000->error(lock_not_released) end,
        ok=m_analytics:store_flush(Epoch,1,Batch,[],C),
        ?assertEqual(1,z_db:q1("select count(*) from analytics_batch where epoch=$1",[Epoch],C)),
        #{lock_wait_us=>Micros,retry=>ok}
    after
        Holder ! {Ref,release},
        z_db:q("delete from analytics_batch where epoch=$1",[Epoch],C)
    end.


%% Storage/report regression: legacy conversion, idempotent summary writes,
%% bounded rollups, and an older winner expiring from the one-hour window.
popular_content(C) ->
    development=m_site:environment(C),
    Epoch=crypto:strong_rand_bytes(16),
    {ok,D}=z_analytics:definition(#{<<"name">>=><<"Popular content integration test">>,
        <<"event">>=><<"page_request">>,<<"dimension">>=><<"page">>,<<"groups">>=>100,
        <<"filters">>=>#{<<"page">>=>[base64:encode(Epoch)]}},z_analytics:registry(C)),
    {ok,Id}=m_analytics:store_create(D,<<"active">>,C),
    T=z_analytics:bucket(erlang:system_time(second),60)-120, Old=T-7200,
    Col=D#{id=>Id,state=><<"active">>,started=>Old},
    Legacy=#{<<"v:old">>=>20000,<<"$other">>=>100},
    B0=#{from=>Old,until=>Old+60,lost=>false,deltas=>#{{Id,Old}=>Legacy}},
    Noise=lists:foldl(fun(I,A)->z_analytics_frequency:add(integer_to_binary(I),1,A,100) end,
        z_analytics_frequency:new(),lists:seq(1,10000)),
    Counts=z_analytics_frequency:add(<<"v:new">>,2000,Noise,100),
    B=#{from=>T,until=>T+60,lost=>false,deltas=>#{{Id,T}=>Counts}},
    try
        ok=m_analytics:store_flush(Epoch,1,B0,[Col],C),
        ok=m_analytics:store_flush(Epoch,2,B,[Col],C),
        ok=m_analytics:store_flush(Epoch,2,B,[Col],C),
        lists:foreach(fun({R,_})->
            Start=z_analytics:bucket(T,R),
            [Row]=m_analytics:store_rows(Id,R,Start,Start+R,C),
            S=maps:get(counts,Row),
            ?assert(z_analytics_frequency:size(S)=<100),
            Expected=case Old>=Start of true -> 32100; false -> 12000 end,
            ?assertEqual(Expected,z_analytics_frequency:total(S))
        end,z_analytics:tiers()),
        Admin=z_acl:sudo(C),
        Hour=maps:get(report,m_analytics:dashboard(Id,<<"hour">>,Admin)),
        Six=maps:get(report,m_analytics:dashboard(Id,<<"day">>,Admin)),
        ?assertEqual(12000,maps:get(total,Hour)),
        ?assertEqual(32100,maps:get(total,Six)),
        [#{key:= <<"v:new">>}|_]=maps:get(groups,Hour),
        [#{key:= <<"v:old">>}|_]=maps:get(groups,Six),
        ?assertEqual([#{label=>maps:get(label,G),value=>maps:get(count,G)} || G <- maps:get(groups,Hour)],
            maps:get(chart,Hour)),
        ?assert(length(maps:get(chart,Hour))=<20),
        ?assert(maps:get(error,Six)>=100),
        ok
    after
        m_analytics:store_purge(Id,C),
        z_db:q("delete from analytics_batch where epoch=$1",[Epoch],C)
    end.
