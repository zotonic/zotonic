-module(mod_websub_tests).
-moduledoc("
EUnit tests for WebSub resource headers and HTML head links.
").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_stdlib/include/z_url_metadata.hrl").


resource_headers_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_context:new(zotonic_site_testsandbox),
    Id = 1,
    ContextNoLanguage = z_context:set_language('x-default', Context),
    HubUrl = z_context:abs_url(z_dispatcher:url_for(websub, [], ContextNoLanguage), ContextNoLanguage),
    SelfPath = z_dispatcher:url_for(websub_topic, [{id, Id}], ContextNoLanguage),
    SelfUrl = z_context:abs_url(SelfPath, Context),
    Headers = mod_websub:observe_resource_headers(#resource_headers{ id = Id }, [], Context),
    ?assertEqual({ok, #{topic => SelfUrl, hubs => [HubUrl]}},
        z_websub_discovery:links(SelfUrl, Headers, <<>>, undefined)),
    ok.

html_head_links_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_context:new(zotonic_site_testsandbox),
    Id = 1,
    ContextNoLanguage = z_context:set_language('x-default', Context),
    HubUrl = z_context:abs_url(z_dispatcher:url_for(websub, [], ContextNoLanguage), ContextNoLanguage),
    SelfPath = z_dispatcher:url_for(websub_topic, [{id, Id}], ContextNoLanguage),
    SelfUrl = z_context:abs_url(SelfPath, Context),
    {Html, _} = z_template:render_to_iolist("_html_head.tpl", [{id, Id}], Context),
    Body = iolist_to_binary(Html),
    ?assertNotEqual(nomatch, binary:match(Body, <<"<link rel=\"hub\" href=\"", HubUrl/binary, "\">">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"<link rel=\"self\" href=\"", SelfUrl/binary, "\">">>)),
    ok.

semantic_identity_test() ->
    C = z_context:new(zotonic_site_testsandbox),
    A = z_acl:logon(1, C),
    Id = m_rsc:rid(person, A),
    Uri = m_rsc:uri(Id, A),
    ?assertNotEqual(nomatch, binary:match(Uri, <<"/id/person">>)),
    lists:foreach(fun(Language) ->
        Context = z_context:set_language(Language, A),
        Topic = m_websub:topic_url(Id, Context),
        ?assertNotEqual(Uri, Topic),
        {ok, Export} = m_rsc_export:full(Id, Context),
        ?assertEqual(Uri, maps:get(<<"uri">>, Export)),
        Hub = z_context:abs_url(z_dispatcher:url_for(websub, [],
            z_context:set_language('x-default', Context)), Context),
        ?assertEqual(#{<<"hub">> => Hub, <<"topic">> => Topic}, maps:get(<<"websub">>, Export)),
        ?assertEqual(maps:get(<<"websub">>, Export),
            maps:get(<<"websub">>, z_json:decode(z_json:encode(Export)))),
        ?assertMatch({ok, #{topic := Topic}}, z_websub_discovery:links(Uri, [], <<>>, maps:get(<<"links">>, Export))),
        HeaderContext = z_context:set_resource_headers(Id, Context#context{cowreq = websub_test_support:request(#{})}),
        Headers = maps:to_list(maps:get(resp_headers, HeaderContext#context.cowreq)),
        ?assertMatch({ok, #{topic := Topic}}, z_websub_discovery:links(Uri, Headers, <<>>, undefined))
    end, [en, nl, 'x-default']).

non_authoritative_export_test() ->
    C = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    Uri = <<"https://source.test/id/imported-export">>,
    {ok, Id} = m_rsc:insert(#{
        <<"category_id">> => text,
        <<"is_authoritative">> => false,
        <<"uri">> => Uri
    }, C),
    try
        {ok, Export} = m_rsc_export:full(Id, C),
        ?assertEqual(Uri, maps:get(<<"uri">>, Export)),
        ?assertNot(maps:is_key(<<"websub">>, Export)),
        ?assertNot(maps:is_key(<<"links">>, Export))
    after
        m_rsc:delete(Id, C)
    end.

%% The detected support flag must survive both preview layers. Exercise HTTP
%% Link discovery via the fetch-result observer, rather than mocking the preview.
import_preview_websub_test() ->
    C = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    Uri = <<"https://preview.test/id/16449">>,
    PageUrl = <<"https://preview.test/en/page/16449/example">>,
    Body = z_json:encode(#{
        <<"status">> => <<"ok">>,
        <<"result">> => #{
            <<"id">> => 16449,
            <<"uri">> => Uri,
            <<"is_a">> => [<<"text">>],
            <<"resource">> => #{<<"title">> => <<"Import preview">>}
        }
    }),
    MD = #url_metadata{
        final_url = PageUrl,
        content_type = <<"text/html">>,
        content_type_options = [],
        content_length = 0,
        headers = [{<<"x-resource-uri">>, Uri}],
        links = #{},
        metadata = [],
        partial_data = <<>>
    },
    ok = meck:new(z_fetch, [passthrough]),
    try
        meck:expect(z_fetch, as_data_url, fun(undefined, [], _) -> {error, enoent} end),
        lists:foreach(fun(IsSupported) ->
            Headers = case IsSupported of
                true -> [{"link", "<https://preview.test/.zotonic/websub>; rel=hub, "
                    "<https://preview.test/.zotonic/websub/topic/16449>; rel=self"}];
                false -> []
            end,
            meck:expect(z_fetch, fetch, fun(FetchUri, _, _) when FetchUri =:= Uri ->
                {ok, {Uri, Headers, byte_size(Body), Body}}
            end),
            {ok, Preview} = m_rsc_import:fetch_preview(Uri, C),
            ?assertEqual(IsSupported, maps:get(<<"is_websub_supported">>,
                maps:get(<<"import_options">>, Preview))),
            {ok, Imports} = z_media_import:url_import_props(MD, C),
            [Props] = [P || #media_import_props{importer = rsc_import, rsc_props = P} <- Imports],
            ?assertEqual(IsSupported, maps:get(<<"is_websub_supported">>, Props))
        end, [true, false])
    after
        meck:unload(z_fetch)
    end.

%% Existing persisted denial strings must render without Erlang syntax in the
%% explanation. Unknown remote reasons stay escaped in the diagnostic details.
subscription_error_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_context:set_language(en, z_context:new(zotonic_site_testsandbox)),
    Reason = <<"access-denied-websub">>,
    Legacy = iolist_to_binary(io_lib:format("~p", [Reason])),
    Message = filter_websub_error:websub_error(Reason, C),
    ?assertEqual(Message, filter_websub_error:websub_error(Legacy, C)),
    ?assertNotEqual(nomatch, binary:match(Message, <<"source website">>)),
    ?assertNotEqual(nomatch, binary:match(Message, <<"anonymous visitors">>)),
    ?assertEqual(filter_websub_error:websub_error(unsafe_destination, C),
        filter_websub_error:websub_error(<<"unsafe_destination">>, C)),
    ?assertEqual(<<>>, filter_websub_error:websub_error(undefined, C)),
    {Html, _} = z_template:render_to_iolist("_websub_error.tpl", [{error, Legacy}], C),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Html), Message)),
    Unknown = <<"<script>alert(1)</script>">>,
    {UnknownHtml, _} = z_template:render_to_iolist("_websub_error.tpl", [{error, Unknown}], C),
    Body = iolist_to_binary(UnknownHtml),
    ?assertEqual(nomatch, binary:match(Body, Unknown)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"&lt;script&gt;">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"Technical details">>)).
