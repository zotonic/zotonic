-module(mod_search_db_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").


wait_for(QueryId, ItemId) ->
    receive
        {'$gen_cast',
         {#rsc_query_item{query_id=QueryId, match_id=ItemId}, _}} ->
            ok;
        _X ->
            ?DEBUG("Got wrong message, waiting for rsc_query_item:"),
            ?DEBUG(_X),
            ?assert(false)
    after 1000 ->
            ?debugMsg("Did not receive a rsc_query_item."),
            ?assert(false)
    end.


query_hooks_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),

    z_notifier:observe(rsc_query_item, self(), C),

    %% Create a new query
    {ok, Query1} = m_rsc:insert([{category, 'query'},
                                 {title, <<"All featured articles">>},
                                 {is_query_live, true},
                                 {'query', <<"cat=article\nis_featured">>}], C),

    %% Create an item which fits the query
    {ok, Id} = m_rsc:insert([{category, article},
                             {is_featured, true},
                             {title, <<"A test article">>}], C),
    %% Wait for the notification
    wait_for(Query1, Id),

    m_rsc:delete(Query1, C),
    m_rsc:delete(Id, C),

    z_notifier:detach(rsc_query_item, self(), C),

    ok.



search_query_notify_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),

    Q = <<"cat=keyword\nis_featured">>,

    %% Create a new query
    {ok, Query1} = m_rsc:insert([{category, 'query'},
                                 {title, <<"All featured keywords">>},
                                 {is_query_live, true},
                                 {'query', Q}], C),

    %% There's exactly one query
    Watches = search_query_notify:init(C),
    ?assert(lists:member({Query1, search_query:parse_query_text(Q)}, Watches)),

    %% Create an item which fits the query
    {ok, Id} = m_rsc:insert([{category, keyword},
                             {is_featured, true},
                             {title, <<"A test keyword">>}], C),

    %% It should match
    ?assertEqual([Query1], search_query_notify:check_rsc(Id, Watches, C)),

    %% Create an item which does not fit the query
    {ok, Id2} = m_rsc:insert([{category, collection}, {title, <<"A test collection">>}], C),
    %% It should not match
    ?assertEqual([], search_query_notify:check_rsc(Id2, Watches, C)),

    m_rsc:delete(Id, C),
    m_rsc:delete(Id2, C),
    m_rsc:delete(Query1, C),
    ok.


language_search_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    {ok, Id} = m_rsc:insert([
        {is_published, true},
        {category, other},
        {language, [ en ]},
        {title, #trans{ tr = [ {en, <<"Blah">>} ]}}
    ], C),
    #search_result{ result = Ids } = z_search:search({query, [ {language, en}, {sort, <<"-id">>} ]}, C),
    ?assertEqual( Id, hd(Ids) ),
    m_rsc:delete(Id, C),
    ok.
