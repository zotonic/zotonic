-module(export_feed_http_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

http_test_() ->
    export_test_support:isolated([
        fun conditional_response_check/0,
        fun conditional_headers_check/0,
        fun export_controllers_remain_lazy_check/0
    ]).

conditional_response_check() ->
    meck:new(z_depcache, [no_link]),
    meck:expect(z_depcache, get, fun(Key, _) -> get(Key) end),
    meck:expect(z_depcache, set, fun(Key, Value, _, _) -> put(Key, {ok, Value}), ok end),
    meck:new(m_rsc, [no_link]),
    meck:expect(m_rsc, is_visible, fun(_, _) -> true end),
    meck:expect(m_rsc, p, fun
        (_, <<"modified">>, _) -> {{2020,1,1},{0,0,0}};
        (Id, <<"version">>, _) -> maps:get(Id, get(versions), 1)
    end),
    meck:new(export_feed_query, [no_link]),
    meck:expect(export_feed_query, export_resource_data, fun(_, _) -> {ok, get(rows)} end),
    meck:new(export_helper, [no_link]),
    meck:expect(export_helper, stream, fun(Id, CT, Opts, C) ->
        {export_encoder:stream(Id, CT, Opts, C), C}
    end),
    meck:new(export_encoder, [no_link]),
    meck:expect(export_encoder, stream, fun(_, _, Options, _) ->
        ?assertEqual(get(rows), proplists:get_value(resource_ids, Options)),
        {stream, {<<>>, fun() -> error(should_remain_lazy) end}}
    end),
    try
        put(rows, [1,2]), put(versions, #{}),
        C = z_context:set([{export_module, export_feed_query}, {feed_query, #{<<"pagelen">> => 20}}],
            #context{cowreq=export_test_support:request(#{path => <<"/feed/rss">>})}),
        Mime = {<<"application">>, <<"rss+xml">>, []},
        {{stream, _}, First} = export_feed_http:response(undefined, Mime, C),
        ETag = z_context:get_resp_header(<<"etag">>, First),
        Modified = z_context:get_resp_header(<<"last-modified">>, First),
        Conditional = C#context{cowreq=(C#context.cowreq)#{headers => #{
            <<"if-none-match">> => ETag, <<"if-modified-since">> => Modified}}},
        ?assertMatch({{halt, 304}, _}, export_feed_http:response(undefined, Mime, Conditional)),
        ?assertEqual(1, meck:num_calls(export_encoder, stream, '_')),
        % Versions catch changes even when the modification date is unchanged.
        put(versions, #{2 => 2}),
        ?assertMatch({{stream, _}, _}, export_feed_http:response(undefined, Mime, Conditional)),
        put(versions, #{}), put(rows, [2,1]),
        ?assertMatch({{stream, _}, _}, export_feed_http:response(undefined, Mime, Conditional)),
        put(rows, [1]),
        ?assertMatch({{stream, _}, _}, export_feed_http:response(undefined, Mime, Conditional)),
        DateOnly = C#context{cowreq=(C#context.cowreq)#{headers => #{<<"if-modified-since">> => Modified}}},
        ?assertMatch({{stream, _}, _}, export_feed_http:response(undefined, Mime, DateOnly))
    after meck:unload([z_depcache, m_rsc, export_feed_query, export_encoder, export_helper]) end.

conditional_headers_check() ->
    Date = {{2020,1,1},{0,0,0}},
    Header = cow_date:rfc1123(Date),
    ?assert(export_feed_http:not_modified(undefined, Header, <<"abc">>, Date, true)),
    ?assertNot(export_feed_http:not_modified(undefined, Header, <<"abc">>, Date, false)),
    ?assertNot(export_feed_http:not_modified(undefined, <<"invalid">>, <<"abc">>, Date, true)),
    ?assertNot(export_feed_http:not_modified(<<"\"other\"">>, Header, <<"abc">>, Date, true)),
    ?assert(export_feed_http:not_modified(<<"\"other\", W/\"abc\"">>, undefined, <<"abc">>, Date, false)),
    ?assert(export_feed_http:not_modified(<<"*">>, undefined, <<"abc">>, Date, false)).

export_controllers_remain_lazy_check() ->
    meck:new(z_notifier, [no_link]),
    meck:expect(z_notifier, first, fun
        (#export_resource_filename{}, _) -> {ok, <<"export">>};
        (#export_resource_content_disposition{}, _) -> {ok, <<"inline">>}
    end),
    meck:new(export_feed_http, [no_link]),
    meck:new(z_search, [no_link]),
    try
        lists:foreach(fun({Controller, Dispatch, Id}) ->
            C = z_context:set([{zotonic_dispatch, Dispatch}, {id, {ok, Id}}, {is_query, true}],
                #context{cowreq=export_test_support:request(#{headers => #{<<"if-none-match">> => <<"W/\"old\"">>}})}),
            {{stream, _}, Result} = Controller:process(<<"GET">>, undefined,
                {<<"application">>, <<"feed+json">>, []}, C),
            ?assertEqual(undefined, z_context:get_resp_header(<<"etag">>, Result)),
            ?assertEqual(undefined, z_context:get_resp_header(<<"last-modified">>, Result))
        end, [{controller_export, export, undefined},
            {controller_export_resource, export_rsc_query, 1}]),
        ?assertEqual(0, meck:num_calls(export_feed_http, response, '_')),
        ?assertEqual(0, meck:num_calls(z_search, search, '_'))
    after meck:unload([z_notifier, export_feed_http, z_search]) end.
