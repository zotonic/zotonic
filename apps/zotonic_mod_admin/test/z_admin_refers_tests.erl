-module(z_admin_refers_tests).

-include_lib("eunit/include/eunit.hrl").

html_links_refers_test() ->
    {timeout, 30,
        fun() ->
            ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
            Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
            Unique = z_convert:to_binary(erlang:unique_integer([positive])),
            PagePath = <<"/refers-target-", Unique/binary>>,
            {ok, TargetId} = m_rsc:insert(#{
                category_id => text,
                title => <<"Refers target">>,
                page_path => PagePath,
                is_published => true
            }, Context),
            {ok, EmbeddedId} = m_rsc:insert(#{
                category_id => text,
                title => <<"Embedded resource">>
            }, Context),
            try
                ?assertEqual(TargetId, m_rsc:uri_lookup(PagePath, Context)),
                test_html_refers(TargetId, EmbeddedId, PagePath, Context)
            after
                ok = m_rsc:delete(EmbeddedId, Context),
                ok = m_rsc:delete(TargetId, Context)
            end
        end}.

test_html_refers(TargetId, EmbeddedId, PagePath, Context) ->
    AbsoluteUrl = z_context:abs_url(PagePath, Context),
    Body = iolist_to_binary([
        <<"<p><a href='">>, PagePath, <<"'>Relative link</a></p>">>,
        <<"<p><A HREF=\"">>, AbsoluteUrl, <<"\">Absolute link</A></p>">>,
        <<"<p><a href=\"https://example.com/not-a-resource\">External link</a></p>">>,
        <<"<img src=\"">>, PagePath, <<"\" alt=\"Not a page link\">">>,
        <<"<!-- z-media ">>, integer_to_binary(EmbeddedId), <<" {\"align\":\"block\"} -->">>
    ]),
    {ok, SourceId} = m_rsc:insert(#{
        category_id => text,
        title => <<"Refers source">>,
        body => Body
    }, Context),
    try
        ok = z_admin_refers:ensure_refers(SourceId, Context),
        ?assertEqual(
            lists:sort([TargetId, EmbeddedId]),
            lists:sort(m_edge:objects(SourceId, refers, Context))),

        {ok, SourceId} = m_rsc:update(SourceId, #{
            body => <<"<p><a href=\"https://example.com/not-a-resource\">External link</a></p>">>
        }, Context),
        ok = z_admin_refers:ensure_refers(SourceId, Context),
        ?assertEqual([], m_edge:objects(SourceId, refers, Context))
    after
        ok = m_rsc:delete(SourceId, Context)
    end.
