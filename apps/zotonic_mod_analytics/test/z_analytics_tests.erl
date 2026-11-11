-module(z_analytics_tests).
-include_lib("eunit/include/eunit.hrl").

rollup_conservation_test() ->
    lists:foreach(fun(Limit) ->
        Counts=maps:from_list([{integer_to_binary(I),I}||I<-lists:seq(1,200)]),
        Result=z_analytics:reduce(Counts#{<<"$other">>=>9},Limit),
        ?assert(map_size(Result)=<Limit),
        ?assertEqual(20109,lists:sum(maps:values(Result)))
    end,lists:seq(1,100)).

stable_admission_test() ->
    A=z_analytics:add(<<"v:a">>,1,#{},3),
    B=z_analytics:add(<<"v:b">>,1,A,3),
    C=z_analytics:add(<<"v:c">>,10,B,3),
    ?assertEqual(#{<<"v:a">>=>1,<<"v:b">>=>1,<<"$other">>=>10},C),
    ?assertEqual(2,maps:get(<<"v:a">>,z_analytics:add(<<"v:a">>,1,C,3))).

deterministic_ties_test() ->
    ?assertEqual(#{<<"a">>=>3,<<"$other">>=>6},
        z_analytics:reduce(#{<<"c">>=>3,<<"a">>=>3,<<"b">>=>3},2)).

filter_population_test() ->
    Col=#{filters=>#{<<"language">>=>[<<"nl">>,<<"en">>],<<"traffic">>=>[<<"other">>]}},
    ?assert(z_analytics:matches(Col,#{<<"language">>=><<"nl">>,<<"traffic">>=><<"other">>})),
    ?assertNot(z_analytics:matches(Col,#{<<"language">>=><<"nl">>})),
    ?assertNot(z_analytics:matches(Col,#{<<"language">>=><<"de">>,<<"traffic">>=><<"other">>})).

coverage_test() ->
    A=z_analytics:coverage(120,125,150),
    B=z_analytics:coverage(120,150,180),
    ?assertNot(z_analytics:complete(A bor B)),
    ?assert(z_analytics:complete(A bor B bor z_analytics:coverage(120,120,125))),
    ?assertEqual(0,z_analytics:coverage(120,181,190)),
    ?assertEqual(A,z_analytics:coverage(120,125,150) bor A).

ring_generation_test() ->
    lists:foreach(fun({R,N})->
        T=R*17,
        ?assertEqual(z_analytics:slot(T,R),z_analytics:slot(T+R*N,R)),
        ?assertNotEqual(z_analytics:bucket(T,R),z_analytics:bucket(T+R*N,R))
    end,z_analytics:tiers()).

single_dimension_test() ->
    R=registry(),
    D=#{<<"name">>=><<"Languages">>,<<"event">>=><<"page_request">>,<<"dimension">>=><<"language">>},
    ?assertMatch({ok,_},z_analytics:definition(D,R)),
    ?assertEqual({error,invalid_definition},z_analytics:definition(D#{<<"dimension">>=>[<<"language">>,<<"page">>]},R)),
    ?assertEqual({error,invalid_definition},z_analytics:definition(D#{<<"groups">>=>101},R)),
    ?assertEqual({error,invalid_definition},z_analytics:definition(D#{<<"filters">>=>#{<<"language">>=>[]}},R)).

privacy_schema_test() ->
    Schema=maps:get(<<"page_request">>,registry()),
    ?assertEqual({error,invalid_properties},z_analytics:properties(#{<<"ip">>=><<"127.0.0.1">>},Schema)),
    ?assertEqual({error,invalid_properties},z_analytics:properties(#{<<"language">>=>binary:copy(<<"x">>,256)},Schema)).

registry() -> #{<<"page_request">>=>#{version=>1,dimensions=>#{<<"language">>=>text,<<"page">>=>text}}}.

schema_availability_test() ->
    Col = #{event => <<"mod_test:event">>, schema_version => 1},
    ?assertNot(z_analytics:available(Col, #{})),
    ?assert(z_analytics:available(Col, #{<<"mod_test:event">> => #{version => 1}})),
    ?assertNot(z_analytics:available(Col, #{<<"mod_test:event">> => #{version => 2}})).

request_event_test() ->
    Api = #{method => <<"POST">>, resp_status => 403,
        metrics => #{analytics_api => #{<<"country">> => <<"NL">>}}},
    ?assertEqual({<<"api_request">>, #{<<"country">> => <<"NL">>,
        <<"method">> => <<"POST">>, <<"http_status">> => <<"403">>, <<"status">> => <<"4xx">>}},
        z_analytics_request:request_event(Api)),
    lists:foreach(fun({Status,Class}) ->
        {<<"api_request">>,D} = z_analytics_request:request_event(Api#{resp_status=>Status}),
        ?assertEqual(Class,maps:get(<<"status">>,D)),
        ?assertEqual(integer_to_binary(Status),maps:get(<<"http_status">>,D))
    end,[{200,<<"2xx">>},{302,<<"3xx">>},{500,<<"5xx">>}]),
    {_,Other} = z_analytics_request:request_event(Api#{method=><<"CUSTOM">>}),
    ?assertEqual(<<"OTHER">>,maps:get(<<"method">>,Other)),
    ?assertEqual(undefined,z_analytics_request:request_event(Api#{metrics=>#{}})),
    Page = #{method=><<"GET">>,resp_status=>200,metrics=>#{analytics=>#{}}},
    ?assertEqual({<<"page_request">>,#{}},z_analytics_request:request_event(Page)),
    ?assertEqual(undefined,z_analytics_request:request_event(Page#{resp_status=>404})),
    ?assertEqual(undefined,z_analytics_request:request_event(Page#{method=><<"POST">>})).

dispatch_type_test() ->
    ?assertEqual(<<"page">>,z_analytics_request:dispatch_type(undefined,controller_template)),
    ?assertEqual(undefined,z_analytics_request:dispatch_type(false,controller_template)),
    ?assertEqual(<<"page">>,z_analytics_request:dispatch_type(undefined,controller_page)),
    ?assertEqual(<<"api">>,z_analytics_request:dispatch_type(undefined,controller_api)),
    ?assertEqual(undefined,z_analytics_request:dispatch_type(undefined,controller_file)),
    ?assertEqual(undefined,z_analytics_request:dispatch_type(false,controller_api)),
    ?assertEqual(<<"download">>,z_analytics_request:dispatch_type(download,controller_page)),
    ?assertEqual(<<"webhook">>,z_analytics_request:dispatch_type(<<"webhook">>,controller_api)),
    ?assertEqual(undefined,z_analytics_request:dispatch_type(<<"/private/path">>,controller_api)).

typed_event_test() ->
    P = #{method=><<"POST">>,resp_status=>500,
        metrics=>#{analytics_request=>#{<<"type">>=><<"webhook">>}}},
    ?assertEqual({<<"request">>,#{<<"type">>=><<"webhook">>,<<"method">>=><<"POST">>,
        <<"http_status">>=><<"500">>,<<"status">>=><<"5xx">>}},z_analytics_request:typed_event(P)),
    Page = P#{metrics=>#{analytics_request=>#{<<"type">>=><<"page">>}}},
    ?assertEqual(undefined,z_analytics_request:typed_event(Page)),
    ?assertMatch({<<"request">>,_},z_analytics_request:typed_event(Page#{method=><<"GET">>,resp_status=>200})),
    ?assertEqual(undefined,z_analytics_request:typed_event(P#{metrics=>#{}})).

page_id_filter_test() ->
    Col = #{filters=>#{<<"has_page_id">>=>[<<"false">>]}},
    ?assert(z_analytics:matches(Col,#{<<"has_page_id">>=><<"false">>})),
    ?assertNot(z_analytics:matches(Col,#{<<"has_page_id">>=><<"true">>})),
    ?assertNot(z_analytics:matches(Col,#{})).

http_status_validation_test() ->
    Schema=#{dimensions=>#{<<"http_status">>=>http_status}},
    lists:foreach(fun(V)->
        ?assertMatch({ok,_},z_analytics:properties(#{<<"http_status">>=>V},Schema))
    end,[<<"100">>,<<"200">>,<<"404">>,<<"599">>]),
    lists:foreach(fun(V)->
        ?assertEqual({error,invalid_properties},z_analytics:properties(#{<<"http_status">>=>V},Schema))
    end,[<<"99">>,<<"600">>,<<"2xx">>,<<"2000">>,200]).

description_and_event_dimensions_test() ->
    R=#{<<"smtp">>=>#{version=>1,dimensions=>#{<<"outcome">>=>[<<"sent">>,<<"failed">>]}}},
    D=#{<<"name">>=><<"Mail">>,<<"event">>=><<"smtp">>},
    {ok,Default}=z_analytics:definition(D,R),
    ?assertEqual(<<>>,maps:get(description,Default)),
    {ok,WithDescription}=z_analytics:definition(D#{<<"description">>=><<"Transport outcomes">>},R),
    ?assertEqual(<<"Transport outcomes">>,maps:get(description,WithDescription)),
    ?assertEqual({error,invalid_definition},z_analytics:definition(D#{<<"description">>=>binary:copy(<<"x">>,1001)},R)),
    ?assertEqual({error,invalid_definition},z_analytics:definition(D#{<<"description">>=><<255>>},R)),
    ?assertEqual({error,invalid_definition},z_analytics:definition(D#{<<"dimension">>=><<"page">>},R)),
    ?assertEqual({error,invalid_definition},z_analytics:definition(D#{<<"filters">>=>#{<<"language">>=>[<<"en">>]}},R)),
    ?assertEqual({error,invalid_definition},z_analytics:definition(D#{<<"filters">>=>#{<<"outcome">>=>[<<"bogus">>]}},R)).


crawler_measurement_policy_test() ->
    Bot=#{<<"traffic">>=><<"crawler">>},
    Human=#{<<"traffic">>=><<"other">>},
    lists:foreach(fun(Dimension) ->
        Col=#{event=><<"page_request">>,dimension=>Dimension,filters=>#{}},
        ?assertNot(z_analytics:matches(Col,Bot)),
        ?assert(z_analytics:matches(Col,Human)),
        ?assert(z_analytics:matches(Col,#{<<"traffic">>=><<"unknown">>}))
    end,[<<>>,<<"device">>,<<"page">>]),
    ?assert(z_analytics:matches(#{event=><<"page_request">>,dimension=><<"traffic">>,filters=>#{}},Bot)),
    ?assert(z_analytics:matches(#{event=><<"request">>,dimension=><<"traffic">>,filters=>#{}},Bot)),
    ?assert(z_analytics:matches(#{event=><<"api_request">>,dimension=><<>>,filters=>#{}},Bot)),
    ?assertNot(z_analytics:matches(#{event=><<"api_request">>,dimension=><<>>,filters=>#{<<"traffic">>=>[<<"other">>]}},Bot)).

agent_traffic_test() ->
    lists:foreach(fun(UA) ->
        ?assertEqual(<<"agent">>, z_analytics_request:traffic(UA, false)),
        ?assertEqual(<<"agent">>, z_analytics_request:traffic(UA, true))
    end, [<<"ChatGPT-User/1.0">>, <<"Mozilla/5.0 (compatible; Claude-User/1.0)">>,
          <<"CLAUDE-USER">>]),
    lists:foreach(fun(UA) ->
        ?assertEqual(<<"crawler">>, z_analytics_request:traffic(UA, false))
    end, [<<"GPTBot/1.0">>, <<"OAI-SearchBot/1.0">>, <<"ClaudeBot/1.0">>,
          <<"Claude-SearchBot/1.0">>]),
    lists:foreach(fun(UA) ->
        ?assertEqual(<<"other">>, z_analytics_request:traffic(UA, false))
    end, [<<"Mozilla/5.0">>, <<"curl/8.0">>, <<"FakeChatGPT-User/1.0">>,
          <<"Claude-User-Fake/1.0">>, <<"https://example.test/ChatGPT-User">>]),
    ?assertEqual(<<"crawler">>, z_analytics_request:traffic(<<"SomeBot">>, true)),
    ?assertEqual(<<"unknown">>, z_analytics_request:traffic(<<>>, false)),
    ?assertEqual(<<"unknown">>, z_analytics_request:traffic(undefined, false)),
    ?assertEqual(<<"unknown">>, z_analytics_request:traffic(binary:copy(<<"x">>,2049), false)).

agent_measurement_policy_test() ->
    lists:foreach(fun(Traffic) ->
        Props = #{<<"traffic">> => Traffic},
        lists:foreach(fun(Dimension) ->
            Col = #{event => <<"page_request">>, dimension => Dimension, filters => #{}},
            ?assertNot(z_analytics:matches(Col, Props)),
            ?assert(z_analytics:matches(Col#{filters => #{<<"traffic">> => [Traffic]}}, Props)),
            ?assertNot(z_analytics:matches(Col#{filters => #{<<"traffic">> => [<<"other">>]}}, Props))
        end, [<<>>, <<"device">>, <<"page">>]),
        ?assert(z_analytics:matches(#{event => <<"page_request">>, dimension => <<"traffic">>, filters => #{}}, Props)),
        ?assert(z_analytics:matches(#{event => <<"request">>, dimension => <<>>, filters => #{}}, Props))
    end, [<<"agent">>, <<"crawler">>]).

agent_dispatch_analytics_test() ->
    File = filename:join(code:priv_dir(zotonic_mod_agent_discovery), "dispatch/dispatch"),
    {ok, [Rules]} = file:consult(File),
    ?assertEqual(9, length(Rules)),
    lists:foreach(fun({Name, _, Controller, Options}) ->
        Type = atom_to_binary(Name, utf8),
        ?assertEqual(Type, z_analytics_request:dispatch_type(proplists:get_value(analytics, Options), Controller)),
        lists:foreach(fun(Traffic) ->
            Props = #{<<"type">> => Type, <<"traffic">> => Traffic},
            lists:foreach(fun(Status) ->
                {<<"request">>, Event} = z_analytics_request:typed_event(#{
                    method => <<"GET">>, resp_status => Status,
                    metrics => #{analytics_request => Props}}),
                ?assertEqual(Type, maps:get(<<"type">>, Event)),
                ?assertEqual(Traffic, maps:get(<<"traffic">>, Event)),
                ?assertEqual(integer_to_binary(Status), maps:get(<<"http_status">>, Event))
            end, [200, 404, 500])
        end, [<<"agent">>, <<"other">>])
    end, Rules).
