-module(mod_search_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").


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
    C = z_acl:sudo(z_context:new(testsandbox)),

    z_notifier:observe(rsc_query_item, self(), C),
    
    %% Create a new query
    {ok, Query1} = m_rsc:insert([{category, 'query'},
                                 {title, <<"All featured articles">>},
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
    C = z_acl:sudo(z_context:new(testsandbox)),

    Q = <<"cat=keyword\nis_featured">>,

    %% Create a new query
    {ok, Query1} = m_rsc:insert([{category, 'query'},
                                 {title, <<"All featured keywords">>},
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

