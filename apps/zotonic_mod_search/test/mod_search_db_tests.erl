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


search_count_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    Query = #{
        <<"q">> => [
            #{
                <<"term">> => <<"text">>,
                <<"value">> => <<"test">>
            }
        ],
        <<"options">> => #{
            <<"is_count_rows">> => true
        }
    },
    #search_result{ result = [ N ] } = z_search:search(<<"query">>, Query, 1, 20, Context),
    ?assert(is_integer(N) andalso N >= 0),
    ok.

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
    ?assert(lists:member({Query1, z_search_props:from_text(Q)}, Watches)),

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


search_all_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),

    {ok, ObjId} = m_rsc:insert([{category, article},
                                {title, <<"A test article">>}], C),

    Q = {query,[
        {cat,text},
        {hasobject,[ObjId,author]},
        {hasobject,[ObjId,relation]}
    ]},
    Q1 = {query,[
         {cat,text},
         {hasobject, ObjId}
    ]},

    #search_result{ result = [] } = m_search:search(Q, C),
    #search_result{ result = [] } = m_search:search(Q1, C),

    {ok, SubjId} = m_rsc:insert([{category, article},
                             {title, <<"A test article">>}], C),

    m_edge:insert(SubjId, author, ObjId, C),
    #search_result{ result = [] } = m_search:search(Q, C),
    #search_result{ result = [SubjId] } = m_search:search(Q1, C),

    m_edge:insert(SubjId, relation, ObjId, C),
    #search_result{ result = [SubjId] } = m_search:search(Q, C),
    #search_result{ result = [SubjId] } = m_search:search(Q1, C),

    Q2 = {query,[
        {cat,text},
        {hassubject,[SubjId,author]},
        {hassubject,[SubjId,relation]}
    ]},
    Q3 = {query,[
        {cat,text},
        {hassubject, SubjId}
    ]},

    #search_result{ result = [ObjId] } = m_search:search(Q2, C),
    #search_result{ result = [ObjId] } = m_search:search(Q3, C),

    Q4 = {query, [{hasobjectpredicate, relation}]},
    #search_result{ result = R1 } = m_search:search(Q4, C),
    true = lists:member(SubjId, R1),
    R1 = uniq(R1), % The result should not contain duplicates

    Q5 = {query, [{hassubjectpredicate, author}]},
    #search_result{ result = R2 } = m_search:search(Q5, C),
    true = lists:member(ObjId, R2),
    R2 = uniq(R2), % The result should not contain duplicates

    Q6 = {query, [{hasanyobject, [ObjId]}]},
    #search_result{ result = [SubjId] } = m_search:search(Q6, C),

    m_rsc:delete(ObjId, C),
    m_rsc:delete(SubjId, C),

    ok.

search_edge_exclude_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),

    {ok, A} = m_rsc:insert([{category, article}, {title, <<"Article A">>}], C),
    {ok, B} = m_rsc:insert([{category, article}, {title, <<"Article B">>}], C),

    Q1 = #{
        <<"q">> => [
            #{
                <<"term">> => <<"hasobject">>,
                <<"value">> => <<"{}">>
            },
            #{
                <<"term">> => <<"id">>,
                <<"value">> => [ A, B ]
            },
            #{
                <<"term">> => <<"sort">>,
                <<"value">> => <<"id">>
            }
        ]
    },
    Q2 = #{
        <<"q">> => [
            #{
                <<"term">> => <<"hassubject">>,
                <<"value">> => <<"{}">>
            },
            #{
                <<"term">> => <<"id">>,
                <<"value">> => [ A, B ]
            },
            #{
                <<"term">> => <<"sort">>,
                <<"value">> => <<"id">>
            }
        ]
    },
    #search_result{ result = [ A, B ] } = z_search:search(<<"query">>, Q1, 1, 100, C),
    #search_result{ result = [ A, B ] } = z_search:search(<<"query">>, Q2, 1, 100, C),

    {ok, _} = m_edge:insert(A, author, B, C),
    #search_result{ result = [ B ] } = z_search:search(<<"query">>, Q1, 1, 100, C),
    #search_result{ result = [ A ] } = z_search:search(<<"query">>, Q2, 1, 100, C),

    m_rsc:delete(A, C),
    m_rsc:delete(B, C),
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
    #search_result{ result = Ids } = z_search:search(
        <<"query">>,
        #{ <<"language">> => en, <<"sort">> => <<"-id">> },
        1, 100, C),
    ?assertEqual( Id, hd(Ids) ),
    m_rsc:delete(Id, C),
    ok.

properties_search_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    % Default properties
    #search_result{
        result = [
            #{
                <<"id">> := _,
                <<"thumbnail_url">> := _,
                <<"title">> := _,
                <<"short_title">> := _,
                <<"summary">> := _,
                <<"category_id">> := _,
                <<"category">> := #{
                    <<"id">> := _,
                    <<"name">> := _,
                    <<"title">> := _
                }
            }
            | _
        ]
    } = z_search:search(<<"query">>, #{}, 1, 20, #{ properties => true}, C),
    % Specific properties
    #search_result{
        result = [ R1 | _ ]
    } = z_search:search(<<"query">>, #{}, 1, 20, #{ properties => [ <<"body">> ] }, C),
    [ <<"body">>, <<"id">> ] = lists:sort(maps:keys(R1)),
    % No properties, just resource ids
    #search_result{
        result = [ R2 | _ ]
    } = z_search:search(<<"query">>, #{}, 1, 20, #{}, C),
    true = is_integer(R2),
    ok.

uniq([]) ->
  [];
uniq(List) when is_list(List) ->
  uniq(List, []).

uniq([], Acc) ->
  lists:reverse(Acc);
uniq([H | Tail], Acc) ->
  case lists:member(H, Acc) of
    true ->
      uniq(Tail, Acc);
    false ->
      uniq(Tail, [H | Acc])
  end.


