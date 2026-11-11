-module(agent_discovery_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

module_dependencies_test() ->
    {mod_agent_discovery, Depends, _} = z_module_manager:dependencies(mod_agent_discovery),
    Provided = lists:append([begin
        {_, _, Ps} = z_module_manager:dependencies(M), Ps
    end || M <- [mod_base, mod_search]]),
    ?assertEqual([], Depends -- Provided).

discovery_test_() ->
    %% Core mocks must not affect background testsandbox workers.
    z_test_support:isolated({foreach, fun setup/0, fun cleanup/1, [
        fun anonymous_links/0,
        fun private_and_noindex/0,
        fun header_coexistence/0,
        fun document_contract/0,
        fun unicode_metadata/0,
        fun controller_contract/0,
        fun anonymous_responses/0,
        fun model_paths/0
    ]}).

setup() ->
    Modules = [z_dispatcher, z_context, m_site, m_config, m_rsc, z_acl, z_module_manager],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
    meck:expect(z_dispatcher, url_for, fun(Dispatch, Args, none, C) ->
        anonymous(C),
        ?assertEqual('x-default', z_context:language(C)),
        dispatch(Dispatch, Args)
    end),
    meck:expect(z_context, abs_url, fun(Path, C) ->
        anonymous(C), <<"https://example.test", Path/binary>>
    end),
    meck:expect(m_site, get, fun(title, C) -> anonymous(C), <<"Example & Museum">> end),
    meck:expect(m_config, get_value, fun(mod_agent_discovery, description, <<>>, C) ->
        anonymous(C), <<"Public collections">>
    end),
    meck:expect(m_config, get_boolean, fun(seo, noindex, C) -> anonymous(C), false end),
    meck:expect(m_rsc, rid, fun(Id, C) -> anonymous(C), Id end),
    meck:expect(z_acl, rsc_visible, fun(Id, C) -> anonymous(C), Id =/= 2 end),
    meck:expect(m_rsc, p, fun
        (_, category_id, C) -> anonymous(C), 10;
        (3, seo_noindex, C) -> anonymous(C), true;
        (_, _, C) -> anonymous(C), false
    end),
    meck:expect(z_module_manager, active, fun(mod_seo_sitemap, C) -> anonymous(C), false end),
    Modules.

cleanup(Modules) -> lists:foreach(fun meck:unload/1, Modules).

anonymous(#context{user_id = undefined, acl = undefined}) -> ok;
anonymous(_) -> error(non_anonymous_context).

context() -> #context{user_id = 1, acl = admin, language = [nl, en]}.

dispatch(agent_search, []) -> <<"/api/agent/search">>;
dispatch(agent_schema, []) -> <<"/api/agent/schema">>;
dispatch(agent_resource, [{id, Id}]) -> <<"/api/agent/resource/", (z_convert:to_binary(Id))/binary>>;
dispatch(agent_related, [{id, Id}]) -> <<"/api/agent/related/", (z_convert:to_binary(Id))/binary>>;
dispatch(agent_markdown, [{id, Id}]) -> <<"/agent/resource/", (z_convert:to_binary(Id))/binary, "/markdown">>;
dispatch(agent_llms, []) -> <<"/llms.txt">>;
dispatch(agent_api_catalog, []) -> <<"/.well-known/api-catalog">>;
dispatch(agent_openapi, []) -> <<"/openapi.json">>;
dispatch(agent_api_help, []) -> <<"/api-help">>;
dispatch(sitemap_xml, []) -> <<"/sitemap.xml">>;
%% Deliberate site overrides: documents must not hardcode the base API paths.
dispatch(api, [{star, Path}]) -> <<"/custom-api/", Path/binary>>;
dispatch(api_rsc_export, [{id, Id}]) ->
    <<"/public-resource/", (z_convert:to_binary(Id))/binary>>.

anonymous_links() ->
    Links = m_agent_discovery:links(1, context()),
    ?assertEqual(6, length(Links)),
    ?assertEqual([#{href => <<"https://example.test/public-resource/1">>,
        rel => <<"alternate">>, type => <<"application/json">>},
        #{href => <<"https://example.test/agent/resource/1/markdown">>, rel => <<"alternate">>, type => <<"text/markdown">>}], alternates(Links)),
    ?assertEqual(4, length(m_agent_discovery:links(undefined, context()))).

private_and_noindex() ->
    lists:foreach(fun(Id) ->
        ?assertEqual([], alternates(m_agent_discovery:links(Id, context())))
    end, [2, 3, undefined, #{<<"id">> => 1}, [1, 2]]),
    meck:expect(m_rsc, p, fun
        (_, category_id, _) -> 10;
        (10, is_seo_noindex_cat, _) -> true;
        (_, _, _) -> false
    end),
    ?assertEqual([], alternates(m_agent_discovery:links(1, context()))),
    meck:expect(m_config, get_boolean, fun(seo, noindex, _) -> true end),
    ?assertEqual([], alternates(m_agent_discovery:links(1, context()))).

header_coexistence() ->
    Headers = mod_agent_discovery:observe_resource_headers(#resource_headers{id = 1}, [
        {<<"link">>, <<"<https://hub.test/>; rel=\"hub\"">>},
        {<<"Link">>, <<"<https://example.test/feed>; rel=\"self\"">>},
        {<<"x-resource-uri">>, <<"https://example.test/id/1">>}
    ], context()),
    ?assertEqual(1, length([V || {<<"link">>, V} <- Headers])),
    Header = proplists:get_value(<<"link">>, Headers),
    lists:foreach(fun(Rel) -> ?assertNotEqual(nomatch, binary:match(Header, Rel)) end,
        [<<"hub">>, <<"self">>, <<"api-catalog">>, <<"alternate">>]),
    ?assertEqual(<<"https://example.test/id/1">>, proplists:get_value(<<"x-resource-uri">>, Headers)),
    [{<<"link">>, Safe}] = agent_discovery_document:link_header([
        #{href => <<"https://example.test/>\r\nInjected: yes">>, rel => <<"alternate">>, type => <<"application/json">>}
    ], []),
    ?assertEqual(nomatch, binary:match(Safe, <<"\r\n">>)),
    ?assertNotEqual(nomatch, binary:match(Safe, <<"%3E%0D%0A">>)).

document_contract() ->
    Info = m_agent_discovery:info(context()),
    Spec = agent_discovery_document:openapi(Info),
    ?assertEqual(<<"3.1.0">>, maps:get(openapi, Spec)),
    Paths = maps:get(paths, Spec),
    ?assertEqual([<<"/api/agent/related/{id}">>, <<"/api/agent/resource/{id}">>, <<"/api/agent/schema">>, <<"/api/agent/search">>, <<"/custom-api/model/search/get/paged">>, <<"/public-resource/{id}">>], lists:sort(maps:keys(Paths))),
    lists:foreach(fun(Item) -> ?assertEqual([get], maps:keys(Item)) end, maps:values(Paths)),
    #{get := #{parameters := [#{required := true, in := <<"path">>}]}} = maps:get(<<"/public-resource/{id}">>, Paths),
    %% Exercise real JSON serialization, including binary path keys.
    #{<<"paths">> := JsonPaths} = z_json:decode(z_json:encode(Spec)),
    ?assertEqual(maps:keys(Paths), maps:keys(JsonPaths)),
    #{linkset := [#{item := [#{href := Endpoint}]}, #{anchor := Endpoint}]} = agent_discovery_document:catalog(Info),
    ?assertEqual(<<"https://example.test/api/agent/search">>, Endpoint),
    Text = agent_discovery_document:llms(Info),
    ?assertNotEqual(nomatch, binary:match(Text, Endpoint)),
    ?assertEqual(nomatch, binary:match(Text, <<"/mcp">>)),
    ?assertEqual(nomatch, binary:match(Text, <<"sitemap.xml">>)),
    meck:expect(z_module_manager, active, fun(mod_seo_sitemap, C) -> anonymous(C), true end),
    ?assertNotEqual(nomatch, binary:match(agent_discovery_document:llms(m_agent_discovery:info(context())), <<"sitemap.xml">>)).

controller_contract() ->
    {Methods, _} = controller_agent_discovery:allowed_methods(context()),
    ?assertEqual([<<"GET">>, <<"HEAD">>], Methods),
    lists:foreach(fun({Document, Type}) ->
        C = z_context:set(document, Document, context()),
        ?assertEqual({[Type], C}, controller_agent_discovery:content_types_provided(C))
    end, [{llms, <<"text/plain">>}, {catalog, <<"application/linkset+json">>},
          {openapi, <<"application/json">>}, {help, <<"text/html">>}]).

model_paths() ->
    ?assertMatch({ok, {#{operations := [_, _, _, _, _, _]}, []}}, m_agent_discovery:m_get([<<"info">>], undefined, context())),
    ?assertMatch({error, unknown_path}, m_agent_discovery:m_get([<<"unknown">>], undefined, context())).

anonymous_responses() ->
    meck:expect(z_context, set_resource_headers, fun(undefined, C) ->
        anonymous(C),
        z_context:set_resp_headers(mod_agent_discovery:observe_resource_headers(
            #resource_headers{}, [], C), C)
    end),
    SignedIn = (context())#context{cowreq = #{
        method => <<"GET">>,
        version => 'HTTP/1.1',
        scheme => <<"https">>,
        host => <<"example.test">>,
        port => 443,
        path => <<"/llms.txt">>,
        qs => <<>>,
        headers => #{},
        peer => {{127, 0, 0, 1}, 12345},
        sock => {{127, 0, 0, 1}, 443},
        cert => undefined,
        ref => agent_discovery_tests,
        pid => self(),
        streamid => 1,
        has_body => false,
        body_length => 0
    }},
    {true, Public} = controller_agent_discovery:service_available(SignedIn),
    anonymous(Public),
    ?assertNotEqual(nomatch, binary:match(
        maps:get(<<"cache-control">>, maps:get(resp_headers, Public#context.cowreq)), <<"no-store">>)),
    lists:foreach(fun(Document) ->
        C = z_context:set(document, Document, Public),
        {Body, Out} = controller_agent_discovery:process(<<"GET">>, undefined, undefined, C),
        anonymous(Out),
        ?assert(maps:is_key(<<"link">>, maps:get(resp_headers, Out#context.cowreq))),
        case Document of
            llms -> ?assertMatch(<<"# Example", _/binary>>, Body);
            catalog ->
                ?assertMatch(#{<<"linkset">> := [_, _]}, z_json:decode(Body)),
                ?assertNotEqual(nomatch, binary:match(
                    maps:get(<<"content-type">>, maps:get(resp_headers, Out#context.cowreq)), <<"profile=">>));
            openapi -> ?assertMatch(#{<<"openapi">> := <<"3.1.0">>}, z_json:decode(Body))
        end
    end, [llms, catalog, openapi]),
    meck:expect(m_config, get_boolean, fun(seo, noindex, _) -> true end),
    {true, NoIndex} = controller_agent_discovery:service_available(SignedIn),
    ?assertEqual(<<"noindex,nofollow">>, maps:get(<<"x-robots-tag">>, maps:get(resp_headers, NoIndex#context.cowreq))).

alternates(Links) -> [L || #{rel := <<"alternate">>} = L <- Links].

unicode_metadata() ->
    meck:expect(m_site, get, fun(title, _) -> "Zøtoñic" end),
    #{<<"info">> := #{<<"title">> := Title}} = z_json:decode(z_json:encode(
        agent_discovery_document:openapi(m_agent_discovery:info(context())))),
    ?assertEqual(<<"Zøtoñic public information API"/utf8>>, Title).
