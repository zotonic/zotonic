-module(mod_search_db_tests).
-moduledoc("
EUnit tests for database-backed search query behavior and notifications.
").

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

expand_content_groups_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),

    %% Empty list gives no groups and WithDefaultGroup = false
    ?assertEqual({false, []}, search_query:expand_content_groups([], C)),

    %% Unknown resource name is silently skipped
    ?assertEqual({false, []}, search_query:expand_content_groups([nonexistent_cg_name_for_testing], C)),

    %% default_content_group sets WithDefaultGroup = true
    DefaultCGId = m_rsc:rid(default_content_group, C),
    ?assert(is_integer(DefaultCGId)),
    {true, DefaultGroups} = search_query:expand_content_groups([default_content_group], C),
    ?assert(lists:member(DefaultCGId, DefaultGroups)),

    %% A non-default content group leaves WithDefaultGroup = false
    {ok, CGId} = m_rsc:insert([{category, content_group}, {title, <<"Test CG for expand">>}], C),
    {false, CustomGroups} = search_query:expand_content_groups([CGId], C),
    ?assert(lists:member(CGId, CustomGroups)),
    m_rsc:delete(CGId, C),
    ok.


content_group_exclude_sql_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),

    %% Undefined value produces an empty result (no SQL term)
    ?assertEqual([], search_query:qterm(
        #{ <<"term">> => <<"content_group_exclude">>, <<"value">> => undefined },
        false, C)),

    %% Wildcard '*' matches only resources with a null content_group_id
    #search_sql_term{ where = WhereWild, extra = ExtraWild } =
        search_query:qterm(
            #{ <<"term">> => <<"content_group_exclude">>, <<"value">> => <<"*">> },
            false, C),
    ?assertEqual(<<"rsc.content_group_id is null">>, WhereWild),
    ?assert(lists:member(no_content_group_check, ExtraWild)),

    %% Non-default content group: SQL must exclude explicitly assigned resources only
    {ok, CGId} = m_rsc:insert([{category, content_group}, {title, <<"Test CG for SQL">>}], C),
    #search_sql_term{ where = WhereNonDefault, args = [ArgsNonDefault], extra = ExtraNonDefault } =
        search_query:qterm(
            #{ <<"term">> => <<"content_group_exclude">>, <<"value">> => [CGId] },
            false, C),
    ?assert(lists:member(CGId, ArgsNonDefault)),
    ?assert(lists:member(no_content_group_check, ExtraNonDefault)),
    %% Where clause must allow resources with null content_group_id through (they are in the default group, not in the excluded CG)
    ?assert(has_fragment(<<"is null">>, WhereNonDefault)),

    %% default_content_group: SQL must also exclude resources with null content_group_id
    #search_sql_term{ where = WhereDefault, args = [ArgsDefault], extra = ExtraDefault } =
        search_query:qterm(
            #{ <<"term">> => <<"content_group_exclude">>, <<"value">> => [default_content_group] },
            false, C),
    DefaultCGId = m_rsc:rid(default_content_group, C),
    ?assert(lists:member(DefaultCGId, ArgsDefault)),
    ?assert(lists:member(no_content_group_check, ExtraDefault)),
    %% Where clause must exclude resources with null content_group_id (they are in the default group)
    ?assert(has_fragment(<<"is not null">>, WhereDefault)),

    m_rsc:delete(CGId, C),

    %% Non-admin (anonymous) context: no_content_group_check must NOT be set
    Anon = z_context:new(zotonic_site_testsandbox),

    %% Wildcard: non-admin must not get no_content_group_check
    #search_sql_term{ extra = ExtraWildAnon } =
        search_query:qterm(
            #{ <<"term">> => <<"content_group_exclude">>, <<"value">> => <<"*">> },
            false, Anon),
    ?assertNot(lists:member(no_content_group_check, ExtraWildAnon)),

    %% List (non-default CG): non-admin must not get no_content_group_check
    {ok, CGId2} = m_rsc:insert([{category, content_group}, {title, <<"Test CG for SQL anon">>}], C),
    #search_sql_term{ extra = ExtraNonDefaultAnon } =
        search_query:qterm(
            #{ <<"term">> => <<"content_group_exclude">>, <<"value">> => [CGId2] },
            false, Anon),
    ?assertNot(lists:member(no_content_group_check, ExtraNonDefaultAnon)),

    %% List (default_content_group): non-admin must not get no_content_group_check
    #search_sql_term{ extra = ExtraDefaultAnon } =
        search_query:qterm(
            #{ <<"term">> => <<"content_group_exclude">>, <<"value">> => [default_content_group] },
            false, Anon),
    ?assertNot(lists:member(no_content_group_check, ExtraDefaultAnon)),

    m_rsc:delete(CGId2, C),
    ok.


content_group_exclude_search_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),

    {ok, CGId} = m_rsc:insert([{category, content_group}, {title, <<"CG for exclude search test">>}], C),

    %% Resource in the custom content group
    {ok, AId} = m_rsc:insert([
        {category, article},
        {title, <<"In custom CG">>},
        {content_group_id, CGId}
    ], C),
    %% Resource in the default content group
    {ok, BId} = m_rsc:insert([
        {category, article},
        {title, <<"In default CG">>},
        {content_group_id, default_content_group}
    ], C),

    %% Excluding the custom CG: A should be absent, B (default CG) should be present
    QExcludeCustom = #{
        <<"q">> => [
            #{ <<"term">> => <<"content_group_exclude">>, <<"value">> => CGId },
            #{ <<"term">> => <<"id">>, <<"value">> => [AId, BId] }
        ]
    },
    #search_result{ result = R1 } = z_search:search(<<"query">>, QExcludeCustom, 1, 100, C),
    ?assertNot(lists:member(AId, R1)),
    ?assert(lists:member(BId, R1)),

    %% Excluding the default CG: B should be absent, A (custom CG) should be present
    QExcludeDefault = #{
        <<"q">> => [
            #{ <<"term">> => <<"content_group_exclude">>, <<"value">> => default_content_group },
            #{ <<"term">> => <<"id">>, <<"value">> => [AId, BId] }
        ]
    },
    #search_result{ result = R2 } = z_search:search(<<"query">>, QExcludeDefault, 1, 100, C),
    ?assert(lists:member(AId, R2)),
    ?assertNot(lists:member(BId, R2)),

    m_rsc:delete(AId, C),
    m_rsc:delete(BId, C),
    m_rsc:delete(CGId, C),
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

%% Check whether a binary fragment appears in a where clause (which may be a list or binary).
has_fragment(Fragment, Where) when is_binary(Where) ->
    binary:match(Where, Fragment) =/= nomatch;
has_fragment(Fragment, Where) when is_list(Where) ->
    lists:any(
        fun(Part) when is_binary(Part) ->
                binary:match(Part, Fragment) =/= nomatch;
           (_) ->
                false
        end,
        Where).


