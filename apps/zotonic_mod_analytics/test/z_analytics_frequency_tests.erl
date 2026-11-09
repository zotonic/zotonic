-module(z_analytics_frequency_tests).
-include_lib("eunit/include/eunit.hrl").

exact_and_late_winner_test() ->
    Exact=build([{<<"a">>,5},{<<"b">>,7}],100),
    ?assertEqual(0,z_analytics_frequency:error(Exact)),
    ?assertEqual([#{key=><<"b">>,count=>7,upper=>7},#{key=><<"a">>,count=>5,upper=>5}],
        z_analytics_frequency:ranked(Exact)),
    Noise=[{integer_to_binary(I),1} || I <- lists:seq(1,10000)],
    S=build(Noise++[{<<"late winner">>,2000}],100),
    [#{key:= <<"late winner">>}|_]=z_analytics_frequency:ranked(S),
    bounds(S,Noise++[{<<"late winner">>,2000}],100).

weighted_merge_tree_test() ->
    rand:seed(exsss,{2026,11,7}),
    Chunks=[[{integer_to_binary(rand:uniform(1000)),rand:uniform(20)}
        || _ <- lists:seq(1,300)]++[{<<"winner">>,500}] || _ <- lists:seq(1,64)],
    Sketches=[build(C,100)||C<-Chunks],
    Left=lists:foldl(fun(S,A)->z_analytics_frequency:merge(A,S,100) end,z_analytics_frequency:new(),Sketches),
    Tree=tree(Sketches),
    [bounds(S,lists:append(Chunks),100)||S<-[Left,Tree]],
    [#{key:= <<"winner">>}|_]=z_analytics_frequency:ranked(Tree),
    %% A sketch's exact total is independent of merge order and pruning.
    ?assertEqual(z_analytics_frequency:total(Left),z_analytics_frequency:total(Tree)).

legacy_overflow_test() ->
    Legacy=#{<<"a">>=>20,<<"$other">>=>1000},
    S=z_analytics_frequency:merge(Legacy,build([{<<"b">>,2000}],100),100),
    ?assertEqual(3020,z_analytics_frequency:total(S)),
    ?assertEqual(1000,z_analytics_frequency:error(S)),
    ?assertEqual(false,maps:is_key(<<"$other">>,maps:get(counters,S))),
    ?assertEqual(S,z_analytics:decode(z_analytics:encode(S))).

capacity_one_test() -> bounds(build([{<<"a">>,10},{<<"b">>,11}],1),[{<<"a">>,10},{<<"b">>,11}],1).

%% Window expiry means merging only retained buckets: yesterday's winner
%% cannot dominate today's list after its last bucket leaves the query.
window_expiry_test() ->
    Old=build([{<<"old">>,10000}],100), New=build([{<<"new">>,100}],100),
    [#{key:= <<"old">>}|_]=z_analytics_frequency:ranked(z_analytics_frequency:merge(Old,New,100)),
    ?assertEqual([#{key=><<"new">>,count=>100,upper=>100}],z_analytics_frequency:ranked(New)).

build(Items,K) -> lists:foldl(fun({Key,N},S)->z_analytics_frequency:add(Key,N,S,K) end,z_analytics_frequency:new(),Items).
tree([S]) -> S;
tree(Ss) -> tree(pairs(Ss)).
pairs([A,B|Rest]) -> [z_analytics_frequency:merge(A,B,100)|pairs(Rest)];
pairs(Rest) -> Rest.
bounds(S,Items,K) ->
    Exact=lists:foldl(fun({Key,N},A)->maps:update_with(Key,fun(V)->V+N end,N,A) end,#{},Items),
    Total=lists:sum(maps:values(Exact)), Error=z_analytics_frequency:error(S),
    ?assertEqual(Total,z_analytics_frequency:total(S)),
    ?assert(z_analytics_frequency:size(S)=<K),
    ?assert(Error=<Total div (K+1)),
    maps:foreach(fun(Key,N)->
        Lower=maps:get(Key,maps:get(counters,S),0),
        ?assert(Lower=<N), ?assert(N=<Lower+Error)
    end,Exact).
