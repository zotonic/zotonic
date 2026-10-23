-module(controller_feed_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

feed_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        fun formats/0,
        fun optional_resource/0,
        fun query_arguments/0,
        fun selections/0,
        fun paging_and_options/0,
        fun dispatch/0,
        fun response_and_identity/0
    ]}.

setup() ->
    meck:new(z_notifier, [no_link]),
    meck:expect(z_notifier, first, fun(#url_abs{url=Url}, _) -> <<"https://example.test", Url/binary>> end),
    meck:new(m_rsc, [no_link]),
    meck:expect(m_rsc, rid, fun
        (undefined, _) -> undefined;
        (<<"missing">>, _) -> undefined;
        (<<"news">>, _) -> 1;
        (Id, _) when is_binary(Id) -> binary_to_integer(Id);
        (Id, _) -> Id
    end),
    meck:expect(m_rsc, exists, fun(Id, _) -> is_integer(Id) end),
    meck:expect(m_rsc, is_visible, fun(Id, _) -> is_integer(Id) andalso Id =/= 13 end),
    meck:expect(m_rsc, is_a, fun
        (1, query, _) -> true;
        (2, collection, _) -> true;
        (_, _, _) -> false
    end),
    meck:new(z_search, [no_link]),
    meck:expect(z_search, search, fun(<<"query">>, Query, Page, PageLen, Options, Context) ->
        self() ! {search, Query, Page, PageLen, Options, Context#context.user_id},
        #search_result{result=[3]}
    end).

cleanup(_) -> meck:unload([m_rsc, z_search, z_notifier]).

request(Type, Args) ->
    z_context:set([{type, Type}, {zotonic_dispatch, feed}],
        z_context:set_q_all(Args, #context{user_id=7})).

context(Args) ->
    {false, C} = controller_feed:malformed_request(request(<<"rss">>, Args)),
    C.

search(C) ->
    {ok, [3]} = controller_feed:export_resource_data(
        #export_resource_data{id=z_context:get(feed_id, C)}, C),
    receive
        {search, Query, Page, PageLen, Options, UserId} -> {Query, Page, PageLen, Options, UserId}
    after 1000 -> error(no_search)
    end.

formats() ->
    lists:foreach(fun({Type, Subtype}) ->
        {false, C} = controller_feed:malformed_request(request(Type, [])),
        ?assertEqual({[{<<"application">>, Subtype, []}], C}, controller_feed:content_types_provided(C))
    end, [{<<"rss">>, <<"rss+xml">>}, {<<"atom">>, <<"atom+xml">>},
          {<<"jsonfeed">>, <<"feed+json">>}, {<<"json">>, <<"feed+json">>}]),
    ?assertMatch({true, _}, controller_feed:malformed_request(request(<<"csv">>, []))).

optional_resource() ->
    None = context([]),
    ?assertEqual({true, None}, controller_feed:resource_exists(None)),
    ?assertEqual({false, None}, controller_feed:forbidden(None)),
    Missing = context([{<<"id">>, <<"missing">>}]),
    ?assertEqual({false, Missing}, controller_feed:resource_exists(Missing)),
    ?assertEqual({false, Missing}, controller_feed:forbidden(Missing)),
    Private = context([{<<"id">>, <<"13">>}]),
    ?assertEqual({true, Private}, controller_feed:forbidden(Private)),
    % The path id wins over a conflicting id in the query string.
    {false, Path} = controller_feed:malformed_request(
        z_context:set(id, <<"news">>, request(<<"rss">>, [{<<"id">>, <<"13">>}]))),
    ?assertEqual(1, z_context:get(feed_id, Path)),
    ?assertEqual({false, Path}, controller_feed:forbidden(Path)).

query_arguments() ->
    Bare = context([{<<"cat">>, <<"article">>}, {<<"sort">>, <<"-modified">>},
        {<<"text">>, <<"Erlang">>}, {<<"type">>, <<"rss">>}]),
    Prefixed = context([{<<"qcat">>, <<"article">>}, {<<"qsort">>, <<"-modified">>},
        {<<"qs">>, <<"Erlang">>}]),
    ?assertEqual(z_context:get(feed_query, Bare), z_context:get(feed_query, Prefixed)),
    {#{<<"q">> := Terms}, 1, 20, _, 7} = search(Bare),
    ?assert(lists:member(#{<<"term">> => <<"sort">>, <<"value">> => <<"-modified">>}, Terms)),
    Repeated = context([{<<"cat">>, <<"article">>}, {<<"cat">>, <<"event">>}]),
    RepeatedQ = context([{<<"qcat">>, <<"article">>}, {<<"qcat">>, <<"event">>}]),
    ?assertEqual(z_context:get(feed_query, Repeated), z_context:get(feed_query, RepeatedQ)),
    ?assertMatch({true, _}, controller_feed:malformed_request(
        request(<<"rss">>, [{<<"qqargs">>, <<"true">>}]))).

selections() ->
    lists:foreach(fun({Id, Term, Value}) ->
        C = context([{<<"id">>, Id}, {<<"cat">>, <<"article">>}]),
        {#{<<"q">> := Terms}, _, _, _, _} = search(C),
        ?assert(lists:member(#{<<"term">> => Term, <<"value">> => Value}, Terms)),
        ?assert(lists:any(fun(T) -> maps:get(<<"term">>, T) =:= <<"cat">> end, Terms)),
        case Term of
            <<"query_id">> -> ?assertNot(lists:any(fun(T) -> maps:get(<<"term">>, T) =:= <<"sort">> end, Terms));
            _ -> ok
        end
    end, [{<<"news">>, <<"query_id">>, 1}, {<<"2">>, <<"hassubject">>, [2, haspart]},
          {<<"3">>, <<"rsc_id">>, 3}]).

paging_and_options() ->
    C = context([{<<"page">>, <<"2">>}, {<<"pagelen">>, <<"99999">>},
        {<<"options">>, <<"unsafe">>}, {<<"qoptions">>, <<"unsafe">>}, {<<"z_test">>, <<"ignored">>}]),
    {#{<<"q">> := Terms} = Query, 2, 100, Options, 7} = search(C),
    ?assertNot(maps:is_key(<<"options">>, Query)),
    ?assertEqual(#{is_count_rows => false, is_single_page => true}, Options),
    ?assertEqual([#{<<"term">> => <<"sort">>, <<"value">> => <<"-publication_start">>}], Terms),
    {_, 1, 20, _, _} = search(context([{<<"pagelen">>, <<"invalid">>}])),
    {_, 1, 1, _, _} = search(context([{<<"pagelen">>, <<"0">>}])).

dispatch() ->
    {ok, [Rules]} = file:consult(filename:join(code:priv_dir(zotonic_mod_export), "dispatch/dispatch_export")),
    ?assert(lists:member({feed, ["feed", type], controller_feed, []}, Rules)),
    ?assert(lists:member({feed, ["feed", type, id], controller_feed, []}, Rules)).

response_and_identity() ->
    Base = context([{<<"cat">>, <<"article">>}]),
    C = Base#context{cowreq=#{path => <<"/feed/rss">>, qs => <<"cat=article&pagelen=10">>}},
    ?assertEqual(<<"https://example.test/feed/rss?cat=article&pagelen=10">>,
        export_feed:url(feed, undefined, rss, C)),
    lists:foreach(fun({Mime, Filename}) ->
        {{stream, _}, Result} = controller_feed:process(<<"GET">>, undefined, Mime, C),
        ?assertEqual(controller_feed, z_context:get(export_module, Result)),
        ?assertEqual(<<"inline; filename=", Filename/binary>>,
            z_context:get_resp_header(<<"content-disposition">>, Result))
    end, [{{<<"application">>, <<"rss+xml">>, []}, <<"feed.rss">>},
          {{<<"application">>, <<"feed+json">>, []}, <<"feed.json">>}]).
