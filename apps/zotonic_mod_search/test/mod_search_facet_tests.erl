-module(mod_search_facet_tests).
-moduledoc("Database-backed tests for faceted searches.").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").


fulltext_facet_search_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    Token = z_string:to_name(<<"facet_fulltext_", (z_ids:id(12))/binary>>),
    Title = <<"Facet ", Token/binary>>,
    {ok, MatchId} = m_rsc:insert(#{
        <<"category">> => article,
        <<"title">> => Title,
        <<"summary">> => <<"Experimental fuzzy matching">>,
        <<"is_published">> => true
    }, Context),
    {ok, LessSimilarId} = m_rsc:insert(#{
        <<"category">> => article,
        <<"title">> => Title,
        <<"summary">> => <<"Experimental fuzzy mapping">>,
        <<"is_published">> => true
    }, Context),
    try
        ok = search_facet:pivot_rsc(MatchId, Context),
        ok = search_facet:pivot_rsc(LessSimilarId, Context),
        Exact = <<"facet ", Token/binary, " experimental fuzzy matching">>,
        Typo = <<"facet ", Token/binary, " experimantal fuzzy matching">>,
        ExactResult = search_title_summary(Exact, Context),
        TypoResult = search_title_summary(Typo, Context),
        ?assertEqual(MatchId, hd(ExactResult)),
        ?assert(lists:member(LessSimilarId, ExactResult)),
        ?assertEqual(MatchId, hd(TypoResult)),
        ?assert(lists:member(LessSimilarId, TypoResult))
    after
        ok = m_rsc:delete(MatchId, Context),
        ok = m_rsc:delete(LessSimilarId, Context)
    end.

search_title_summary(Text, Context) ->
    Query = #{
        <<"q">> => [
            #{
                <<"term">> => <<"facet:title_summary">>,
                <<"value">> => Text
            }
        ]
    },
    #search_result{ result = Result } = z_search:search(<<"query">>, Query, 1, 20, Context),
    Result.
