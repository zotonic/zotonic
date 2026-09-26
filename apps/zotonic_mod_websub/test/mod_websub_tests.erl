-module(mod_websub_tests).
-moduledoc("
EUnit tests for WebSub resource headers and HTML head links.
").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").


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
        ?assertMatch({ok, #{topic := Topic}}, z_websub_discovery:links(Uri, [], <<>>, maps:get(<<"links">>, Export))),
        HeaderContext = z_context:set_resource_headers(Id, Context#context{cowreq = websub_test_support:request(#{})}),
        Headers = maps:to_list(maps:get(resp_headers, HeaderContext#context.cowreq)),
        ?assertMatch({ok, #{topic := Topic}}, z_websub_discovery:links(Uri, Headers, <<>>, undefined))
    end, [en, nl, 'x-default']).
