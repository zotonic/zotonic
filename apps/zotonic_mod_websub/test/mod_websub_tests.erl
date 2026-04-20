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
    SelfPath = z_dispatcher:url_for(id, [{id, Id}], ContextNoLanguage),
    SelfUrl = z_context:abs_url(SelfPath, Context),
    Headers = mod_websub:observe_resource_headers(#resource_headers{ id = Id }, [], Context),
    ?assert(lists:member({<<"link">>, <<"<", HubUrl/binary, ">; rel=\"hub\"">>}, Headers)),
    ?assert(lists:member({<<"link">>, <<"<", SelfUrl/binary, ">; rel=\"self\"">>}, Headers)),
    ok.

html_head_links_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_context:new(zotonic_site_testsandbox),
    Id = 1,
    ContextNoLanguage = z_context:set_language('x-default', Context),
    HubUrl = z_context:abs_url(z_dispatcher:url_for(websub, [], ContextNoLanguage), ContextNoLanguage),
    SelfPath = z_dispatcher:url_for(id, [{id, Id}], ContextNoLanguage),
    SelfUrl = z_context:abs_url(SelfPath, Context),
    {Html, _} = z_template:render_to_iolist("_html_head.tpl", [{id, Id}], Context),
    Body = iolist_to_binary(Html),
    ?assertNotEqual(nomatch, binary:match(Body, <<"<link rel=\"hub\" href=\"", HubUrl/binary, "\">">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"<link rel=\"self\" href=\"", SelfUrl/binary, "\">">>)),
    ok.
