-module(export_encoder_atom_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("template_compiler/include/template_compiler.hrl").
-include_lib("xmerl/include/xmerl.hrl").

atom_test_() ->
    export_test_support:isolated({setup, fun setup/0, fun cleanup/1, [
        fun media_entries/0,
        fun calendar_roundtrip/0,
        fun feed_metadata/0,
        fun visible_timestamps/0,
        fun empty_feed/0,
        fun unpublished_without_publication_date/0,
        fun rss_roundtrip/0,
        fun jsonfeed_roundtrip/0,
        fun feed_formats/0,
        fun stream_formats/0,
        fun query_export_limit/0,
        fun pagination_roundtrip/0,
        fun query_next_page/0
    ]}).

%% Mock site services, but compile and render the real Atom templates.
setup() ->
    {ok, _} = application:ensure_all_started(template_compiler),
    meck:new(z_notifier, [no_link]),
    meck:expect(z_notifier, notify, fun(_, _) -> ok end),
    meck:expect(z_notifier, first, fun(_, _) -> undefined end),
    meck:new(z_dispatcher, [no_link]),
    meck:expect(z_dispatcher, url_for, fun(Dispatch, Args, _) ->
        Prefix = case proplists:get_value(absolute_url, Args) of
            true -> <<"https://example.test">>;
            _ -> <<>>
        end,
        iolist_to_binary([Prefix, "/", atom_to_binary(Dispatch), "/",
            z_convert:to_binary(proplists:get_value(id, Args)),
            z_convert:to_binary(proplists:get_value(star, Args))])
    end),
    meck:expect(z_dispatcher, url_for, fun(Dispatch, Args, none, C) ->
        z_dispatcher:url_for(Dispatch, Args, C)
    end),
    meck:new(m_site, [no_link]),
    meck:expect(m_site, title, fun(_) -> <<"Test site">> end),
    meck:new(m_rsc, [no_link]),
    meck:expect(m_rsc, o, fun(_, author, _) -> [] end),
    meck:expect(m_rsc, p, fun(Id, Key, _) ->
        maps:get(z_convert:to_binary(Key), resource(Id), undefined)
    end),
    meck:expect(m_rsc, is_visible, fun(Id, _) -> Id =/= 4 end),
    meck:new(z_template_compiler_runtime, [passthrough, no_link]),
    meck:expect(z_template_compiler_runtime, is_modified, fun(_, _, _) -> false end),
    meck:expect(z_template_compiler_runtime, model_call,
        fun(site, [<<"title">>], _, _) -> {ok, {<<"Test site">>, []}} end),
    meck:expect(z_template_compiler_runtime, map_template, fun(<<"_name.tpl">>, _, _) ->
        {ok, #template_file{template = <<"_name.tpl">>, filename = filename:join(
            code:priv_dir(zotonic_mod_base), "templates/_name.tpl")}}
    end),
    meck:new(z_template, [no_link]),
    meck:expect(z_template, render_to_iolist, fun render/3),
    ok.

cleanup(_) ->
    meck:unload([z_template, z_template_compiler_runtime, m_rsc, m_site, z_dispatcher, z_notifier]).

render({cat, Template}, Vars, Context) ->
    render(Template, Vars, Context);
render(Template, Vars, Context) ->
    App = case Template of "_name.tpl" -> zotonic_mod_base; _ -> zotonic_mod_export end,
    Filename = filename:join([code:priv_dir(App), "templates", Template]),
    {ok, Source} = file:read_file(Filename),
    {ok, Module} = template_compiler:compile_binary(Source, Filename,
        [{runtime, z_template_compiler_runtime}], Context),
    {Module:render(maps:from_list(Vars), #{}, Context), Context}.

resource(Id) ->
    #{
        <<"is_visible">> => Id =/= 4,
        <<"uri">> => <<"https://example.test/id/", (integer_to_binary(Id))/binary>>,
        <<"title">> => <<"Resource ", (integer_to_binary(Id))/binary>>,
        <<"modified">> => {{2026, 10, Id}, {12, 0, 0}},
        <<"date_start">> => {{2026, 11, 1}, {10, 0, 0}},
        <<"is_published">> => Id =/= 5,
        <<"publication_start">> => case Id of
            5 -> undefined;
            _ -> {{2026, 1, 1}, {12, 0, 0}}
        end,
        <<"creator_id">> => #{<<"title">> => <<"An author">>, <<"uri">> => <<"https://example.test/author">>},
        <<"page_url_abs">> => <<"https://example.test/page?a=1&b=2">>,
        <<"summary">> => <<"A summary">>,
        <<"body">> => case Id of 2 -> <<"<p>Body</p>">>; _ -> undefined end,
        <<"medium">> => #{<<"filename">> => <<"file.pdf">>, <<"mime">> => <<"application/pdf">>, <<"size">> => 1234}
    }.

feed(Id, Dispatch, Rows, Header) ->
    Xml = encode(export_encoder_atom, Id, Dispatch, Rows, Header),
    {Doc, []} = xmerl_scan:string(binary_to_list(Xml)),
    Doc.

encode(Encoder, Id, Dispatch, Rows, Header) ->
    Context = #context{language = [en], tz = <<"UTC">>},
    {ok, State0} = Encoder:init([{id, Id}, {dispatch, Dispatch}], Context),
    {ok, Start, State1} = Encoder:header(Header, State0, Context),
    {Entries, State2} = lists:mapfoldl(fun(Row, State) ->
        {ok, Entry, Next} = Encoder:row(Row, State, Context),
        {Entry, Next}
    end, State1, Rows),
    {ok, End} = Encoder:footer(undefined, State2, Context),
    iolist_to_binary([Start, Entries, End]).

value(Path, Doc) ->
    lists:flatten([Text || #xmlText{value=Text} <- xmerl_xpath:string(Path, Doc)]).

media_entries() ->
    Doc = feed(1, export_rsc_query, [2, 3], undefined),
    [WithBody, WithoutBody] = xmerl_xpath:string("/feed/entry", Doc),
    lists:foreach(fun(Entry) ->
        ?assertEqual(1, length(xmerl_xpath:string("content", Entry))),
        [#xmlAttribute{value=Href}] = xmerl_xpath:string("link[@rel='enclosure']/@href", Entry),
        ?assert(lists:prefix("https://example.test/", Href)),
        ?assertEqual([], xmerl_xpath:string("content/@src", Entry))
    end, [WithBody, WithoutBody]),
    ?assertEqual("<p>Body</p>", value("content/text()", WithBody)),
    ?assertEqual("A summary", value("content/text()", WithoutBody)).

feed_metadata() ->
    First = feed(1, export_rsc_query, [], undefined),
    Second = feed(2, export_rsc_query, [], undefined),
    Single = feed(1, export_rsc, [], undefined),
    ?assertNotEqual(value("/feed/id/text()", First), value("/feed/id/text()", Second)),
    ?assertNotEqual(value("/feed/id/text()", First), value("/feed/id/text()", Single)),
    [#xmlAttribute{value=Self}] = xmerl_xpath:string("/feed/link[@rel='self']/@href", First),
    ?assertEqual(Self, value("/feed/id/text()", First)),
    ?assertEqual("Test site - Resource 1", value("/feed/title/text()", First)),
    Custom = feed(1, export_rsc_query, [], <<"Custom title">>),
    ?assertEqual("Test site - Custom title", value("/feed/title/text()", Custom)),
    ?assertEqual([], xmerl_xpath:string("/feed/logo", First)).

visible_timestamps() ->
    Doc = feed(1, export_rsc_query, [3, 2, 4], undefined),
    ?assertEqual(2, length(xmerl_xpath:string("/feed/entry", Doc))),
    ?assertEqual("2026-10-03T12:00:00+00:00", value("/feed/updated/text()", Doc)),
    ?assertEqual(1, length(xmerl_xpath:string("/feed/updated", Doc))),
    Again = feed(1, export_rsc_query, [3, 2, 4], undefined),
    ?assertEqual(value("/feed/updated/text()", Doc), value("/feed/updated/text()", Again)).

empty_feed() ->
    Doc = feed(1, export_rsc_query, [], undefined),
    ?assertEqual("2026-10-01T12:00:00+00:00", value("/feed/updated/text()", Doc)).

rss_roundtrip() ->
    Xml = encode(export_encoder_rss, 1, export_rsc_query, [2, 3, 4], undefined),
    {Doc, []} = xmerl_scan:string(binary_to_list(Xml)),
    ?assertEqual(2, length(xmerl_xpath:string("/rss/channel/item", Doc))),
    ?assertEqual("Test site - Resource 1", value("/rss/channel/title/text()", Doc)),
    {ok, Feed} = z_import_feed_parse:parse(Xml, <<"https://example.test/feed.rss">>),
    assert_imported(Feed).

jsonfeed_roundtrip() ->
    Json = encode(export_encoder_jsonfeed, 1, export_rsc_query, [4, 2, 3, 4], undefined),
    #{<<"version">> := <<"https://jsonfeed.org/version/1.1">>, <<"items">> := [Item, TextItem]} = z_json:decode(Json),
    ?assertEqual(<<"<p>Body</p>">>, maps:get(<<"content_html">>, Item)),
    ?assertEqual(<<"A summary">>, maps:get(<<"content_text">>, TextItem)),
    ?assertEqual([#{<<"name">> => <<"An author">>, <<"url">> => <<"https://example.test/author">>}],
        maps:get(<<"authors">>, Item)),
    {ok, Feed} = z_import_feed_parse:parse(Json, <<"https://example.test/feed.json">>),
    assert_imported(Feed),
    Empty = encode(export_encoder_jsonfeed, 1, export_rsc_query, [4], undefined),
    ?assertEqual([], maps:get(<<"items">>, z_json:decode(Empty))).

assert_imported(Feed) ->
    ?assertEqual(true, maps:get(is_zotonic_feed, Feed)),
    [Entry, _] = maps:get(entries, Feed),
    ?assertEqual(<<"https://example.test/id/2">>, maps:get(uri, Entry)),
    ?assertEqual(<<"https://example.test/api_rsc_export/2">>, maps:get(zotonic_export, Entry)),
    ?assertEqual(<<"<p>Body</p>">>, maps:get(body, Entry)),
    ?assertEqual(<<"A summary">>, maps:get(summary, Entry)),
    ?assertEqual(<<"An author">>, maps:get(author, Entry)),
    ?assertEqual({{2026, 1, 1}, {12, 0, 0}}, maps:get(published, Entry)),
    ?assertEqual({{2026, 10, 2}, {12, 0, 0}}, maps:get(modified, Entry)),
    ?assertEqual([<<"https://example.test/media_attachment/file.pdf">>], maps:get(media, Entry)).

feed_formats() ->
    Context = #context{},
    lists:foreach(fun({Type, Mime}) ->
        ?assert(lists:member(Mime, export_encoder:content_types(Context))),
        ?assertEqual({ok, Mime}, export_helper:get_content_type(1, export_rsc,
            z_context:set(content_type, Type, Context))),
        ?assertEqual({ok, <<"inline">>}, mod_export:observe_export_resource_content_disposition(
            #export_resource_content_disposition{dispatch=export_rsc_query,
                content_type=cowmachine_util:format_content_type(Mime)}, Context))
    end, [{rss, {<<"application">>, <<"rss+xml">>, []}},
          {jsonfeed, {<<"application">>, <<"feed+json">>, []}}]),
    ?assertEqual({ok, <<"application/json">>}, export_helper:get_content_type(1, export_rsc,
        z_context:set(content_type, <<"application/json">>, Context))).

%% Exercise encoder selection and the normal export notification pipeline.
stream_formats() ->
    Context = #context{language=[en], tz= <<"UTC">>},
    lists:foreach(fun(Mime) ->
        {stream, Stream} = export_encoder:stream(2, Mime, [{dispatch, export_rsc}], Context),
        Data = iolist_to_binary(collect_stream(Stream)),
        {ok, Feed} = z_import_feed_parse:parse(Data, <<"https://example.test/feed">>),
        [Entry] = maps:get(entries, Feed),
        ?assertEqual(true, maps:get(is_zotonic_feed, Feed)),
        ?assertEqual(<<"https://example.test/id/2">>, maps:get(uri, Entry))
    end, [{<<"application">>, <<"rss+xml">>, []}, {<<"application">>, <<"feed+json">>, []}]).

query_export_limit() ->
    meck:new(m_config, [no_link]),
    meck:new(z_search, [no_link]),
    meck:expect(m_config, get_value, fun
        (mod_export, query_max_rows_streaming, C) -> z_context:get(test_limit, C)
    end),
    meck:expect(z_search, search, fun(<<"query">>, #{<<"query_id">> := 2},
            3, Limit, #{is_count_rows := false, is_single_page := true}, _) ->
        self() ! {export_limit, Limit},
        #search_result{result=[]}
    end),
    try
        lists:foreach(fun({Value, Expected}) ->
            C = z_context:set(test_limit, Value, z_context:set_q_all(
                [{<<"pagelen">>, <<"999999">>}, {<<"page">>, <<"3">>}],
                #context{language=[en], tz= <<"UTC">>})),
            {stream, Stream} = export_encoder:stream(2,
                {<<"application">>, <<"rss+xml">>, []},
                [{dispatch, export_rsc_query}, {is_query, true}], C),
            _ = collect_stream(Stream),
            receive
                {export_limit, Limit} -> ?assertEqual(Expected, Limit)
            after 1000 -> error(no_query_export)
            end
        end, [{undefined, 50000}, {<<"2500">>, 2500}, {100, 100},
            {<<"invalid">>, 50000}, {<<"0">>, 50000}, {-1, 50000}])
    after
        meck:unload([m_config, z_search])
    end.

collect_stream({Data, done}) -> Data;
collect_stream({Data, Next}) -> [Data | collect_stream(Next())].

page_url_test() ->
    ?assertEqual(<<"https://example.org/feed/rss?cat=article&cat=news&pagelen=10&page=3">>,
        export_feed:page_url(3, <<"https://example.org/feed/rss?cat=article&qpage=2&cat=news&pagelen=10&page=2">>)).


pagination_roundtrip() ->
    Next = <<"https://example.test/feed?page=2&cat=article">>,
    C = z_context:set(feed_next_url, Next, #context{language=[en], tz= <<"UTC">>}),
    lists:foreach(fun(Encoder) ->
        {ok, S0} = Encoder:init([{id, 1}, {dispatch, export_rsc_query}], C),
        {ok, Start, S1} = Encoder:header(undefined, S0, C),
        {ok, Item, S2} = Encoder:row(2, S1, C),
        {ok, End} = Encoder:footer(undefined, S2, C),
        Body = iolist_to_binary([Start, Item, End]),
        ?assertMatch({ok, #{next_url := Next, entries := [_]}},
            z_import_feed_parse:parse(Body, <<"https://example.test/feed">>))
    end, [export_encoder_atom, export_encoder_rss, export_encoder_jsonfeed]).


query_next_page() ->
    meck:new(m_config, [no_link]),
    meck:new(z_search, [no_link]),
    meck:expect(m_config, get_value, fun(mod_export, query_max_rows_streaming, _) -> 10 end),
    meck:expect(z_search, search, fun(<<"query">>, _, 3, 1, _, _) -> #search_result{result=[2]} end),
    meck:expect(z_notifier, first, fun
        (#url_abs{url=Url}, _) -> <<"https://example.test", Url/binary>>;
        (_, _) -> undefined
    end),
    try
        C = z_context:set_q_all([{<<"page">>, <<"3">>}, {<<"pagelen">>, <<"1">>}],
            #context{language=[en], tz= <<"UTC">>, cowreq=export_test_support:request(#{
                path => <<"/export/query/jsonfeed/news">>, qs => <<"page=3&pagelen=1">>})}),
        {stream, Stream} = export_encoder:stream(1, {<<"application">>, <<"feed+json">>, []},
            [{dispatch, export_rsc_query}, {is_query, true}], C),
        %% The search still starts only when consuming the stream.
        ?assertEqual(0, meck:num_calls(z_search, search, '_')),
        Json = z_json:decode(iolist_to_binary(collect_stream(Stream))),
        ?assertEqual(<<"https://example.test/export/query/jsonfeed/news?pagelen=1&page=4">>,
            maps:get(<<"next_url">>, Json))
    after
        meck:unload([m_config, z_search])
    end.


%% A requester with permission to view a draft can export it without a date.
%% SQL NULL publication_start is represented as undefined by the resource model.
unpublished_without_publication_date() ->
    lists:foreach(fun(Encoder) ->
        Body = encode(Encoder, 1, export_rsc_query, [5], undefined),
        case Encoder of
            export_encoder_jsonfeed ->
                #{<<"items">> := [Item]} = z_json:decode(Body),
                ?assertNot(maps:is_key(<<"date_published">>, Item));
            _ ->
                {Doc, []} = xmerl_scan:string(binary_to_list(Body)),
                ?assertEqual([], xmerl_xpath:string("//published | //pubDate", Doc))
        end,
        {ok, #{entries := [Entry]}} = z_import_feed_parse:parse(Body, <<"https://example.test/feed">>),
        ?assertEqual(<<"https://example.test/id/5">>, maps:get(uri, Entry)),
        ?assertEqual(undefined, maps:get(published, Entry)),
        ?assertEqual({{2026, 10, 5}, {12, 0, 0}}, maps:get(modified, Entry))
    end, [export_encoder_atom, export_encoder_rss, export_encoder_jsonfeed]).

calendar_roundtrip() ->
    C = z_context:set(feed_next_url, <<"https://example.test/feed/ics?page=2">>,
        #context{language=[en], tz= <<"UTC">>}),
    {ok, S0} = export_encoder_ics:init([], C),
    {ok, H, S1} = export_encoder_ics:header(undefined, S0, C),
    {ok, F} = export_encoder_ics:footer(undefined, S1, C),
    {ok, #{next_url := Next}} = z_import_feed_parse:parse(iolist_to_binary([H,F]), <<"https://example.test/feed/ics">>),
    ?assertEqual(<<"https://example.test/feed/ics?page=2">>, Next),
    {ok, #{entries := [Native], is_zotonic_feed := true}} = z_import_feed_parse:parse(
        encode(export_encoder_ics, undefined, feed, [1], undefined), <<"https://example.test/feed/ics">>),
    ?assertEqual(<<"https://example.test/api_rsc_export/1">>, maps:get(zotonic_export, Native)),
    Date = {{2026,11,1},{10,0,0}},
    Row = (resource(1))#{<<"date_start">> => Date, <<"date_end">> => Date,
        <<"created">> => Date, <<"title">> => binary:copy(unicode:characters_to_binary("é,;:"), 40)},
    Data = encode(export_encoder_ics, undefined, feed, [Row], <<"Calendar">>),
    ?assertEqual(nomatch, binary:match(Data, <<"DTEND:">>)),
    ?assertEqual(nomatch, binary:match(Data, <<"UID:https\\:">>)),
    lists:foreach(fun(Line) -> ?assert(byte_size(Line) =< 75) end,
        binary:split(Data, <<"\r\n">>, [global])),
    {ok, #{entries := [E]}} = z_import_feed_parse:parse(Data, <<"https://example.test/feed/ics">>),
    ?assertEqual(Date, maps:get(date_start,E)),
    ?assertEqual(maps:get(<<"title">>, Row), maps:get(title,E)),
    AllDay = Row#{<<"date_is_all_day">> => true},
    {ok, #{entries := [D]}} = z_import_feed_parse:parse(
        encode(export_encoder_ics, undefined, feed, [AllDay], undefined), <<"https://example.test/feed/ics">>),
    ?assertEqual({{2026,11,1},{0,0,0}},maps:get(date_end,D)),
    {ok, #{entries := []}} = z_import_feed_parse:parse(
        encode(export_encoder_ics, undefined, feed, [Row#{<<"date_start">> => undefined}, Row#{<<"is_visible">> => false}], undefined),
        <<"https://example.test/feed/ics">>).
