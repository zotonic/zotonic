-module(z_analytics_chart_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

breakdown_series_test() ->
    Rows=[#{start=>0,complete=>true,counts=>#{<<"v:a">>=>3,<<"v:b">>=>2}},
          #{start=>120,complete=>true,counts=>#{<<"v:a">>=>1,<<"$other">>=>4}}],
    Groups=[#{key=><<"v:a">>,label=><<"A">>},#{key=><<"$other">>,label=><<"Other">>}],
    [Total,A,Other]=z_analytics_chart:series(Rows,0,180,60,3,Groups,
        #{dimension=><<"country">>},#context{site=zotonic_site_testsandbox,tz = <<"UTC">>}),
    ?assertEqual([5,null,5],values(Total)),
    ?assertEqual([3,null,1],values(A)),
    ?assertEqual([2,null,4],values(Other)),
    ?assertEqual(3,maps:get(width,Total)),
    ?assertEqual(1,maps:get(width,A)),
    ?assertEqual(<<"#00a6e8">>,maps:get(color,Total)).

values(Series) -> [maps:get(value,P) || P <- maps:get(data,Series)].

zero_and_partial_series_test() ->
    Rows=[#{start=>0,complete=>true,counts=>#{}},
          #{start=>60,complete=>false,counts=>#{<<"v:a">>=>2}}],
    Groups=[#{key=><<"v:a">>,label=><<"A">>},#{key=><<"v:b">>,label=><<"B">>}],
    [Total,A,B]=z_analytics_chart:series(Rows,0,120,60,2,Groups,
        #{dimension=><<"country">>},#context{site=zotonic_site_testsandbox,tz = <<"UTC">>}),
    ?assertEqual([0,2],values(Total)),
    ?assertEqual([0,2],values(A)),
    ?assertEqual([0,null],values(B)).

page_guard_series_test() ->
    Rows = [#{start=>60,complete=>true,counts=>#{<<"v:would_throttle">>=>3}},
            #{start=>120,complete=>true,counts=>#{<<"v:throttle">>=>2}}],
    [Would,Blocked] = z_analytics_chart:guard_series(Rows,0,180,60,3,
        #context{site=zotonic_site_testsandbox,tz = <<"UTC">>}),
    ?assertEqual([null,3,0],values(Would)),
    ?assertEqual([null,0,2],values(Blocked)),
    ?assertEqual(<<"#d98c00">>,maps:get(color,Would)),
    ?assertEqual(<<"#d64343">>,maps:get(color,Blocked)).

page_guard_definition_test() ->
    Default = #{event=><<"page_request">>,dimension=><<>>,filters=>#{}},
    ?assert(m_analytics:is_default_page(Default)),
    ?assertNot(m_analytics:is_default_page(Default#{filters=>#{<<"language">>=>[<<"nl">>]}})),
    Guard = #{event=><<"mod_traffic_guard:request">>,dimension=><<"guard_action">>,groups=>3,
        filters=>#{<<"type">>=>[<<"page">>],<<"traffic">>=>[<<"other">>,<<"unknown">>],
            <<"guard_action">>=>[<<"throttle">>,<<"would_throttle">>]}},
    ?assert(m_analytics:is_page_guard(Guard)),
    ?assertNot(m_analytics:is_page_guard(Guard#{groups=>2})),
    ?assertNot(m_analytics:is_page_guard(Guard#{filters=>#{}})).
