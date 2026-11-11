-module(agent_content_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

content_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun limits/0, fun localized_content/0, fun markdown_api_access/0, fun survey_blocks/0, fun private_content/0,
        fun bounded_search/0, fun bounded_edges/0, fun conditional_content/0,
        fun rendering_failure/0, fun relationship_boundaries/0,
        fun explicit_sort_precedes_relevance/0, fun search_filters/0, fun schema_constraints/0, fun private_page_block/0, fun page_block_dependencies/0,
        {timeout, 8, fun execution_budget/0}
    ]}.

setup() ->
    Modules = [z_language, z_context, z_acl, m_rsc, m_media, m_category, z_template, z_template_compiler_runtime, m_search, m_config, z_dispatcher, z_db],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
    meck:expect(z_language, to_language_atom, fun
        (nl) -> {ok, nl};
        (<<"en">>) -> {ok, en};
        (<<"de">>) -> {ok, de};
        (_) -> {error, not_a_language}
    end),
    meck:expect(z_language, default_language, fun(_) -> nl end),
    meck:expect(z_language, enabled_languages, fun(_) -> [nl, en, de] end),
    meck:expect(z_context, set_language, fun(L, C) -> C#context{language = [L]} end),
    meck:expect(m_rsc, rid, fun(Id, C) -> anonymous(C), Id end),
    meck:expect(z_acl, rsc_visible, fun(Id, C) -> anonymous(C), Id =/= 2 end),
    %% The model must reject unpublished, future and expired resources alike.
    meck:expect(m_rsc, is_published_date, fun(Id, C) ->
        anonymous(C), not lists:member(Id, [3, 5, 6])
    end),
    meck:expect(m_rsc, get, fun(_, C) -> anonymous(C), #{
        <<"title">> => #trans{tr = [{nl, <<"Titel">>}, {en, <<"Title">>}]},
        <<"summary">> => <<"Summary">>, <<"body">> => <<"<p>Public body</p>">>,
        <<"language">> => [nl, en], <<"blocks">> => [#{<<"type">> => <<"unknown">>}],
        <<"private_secret">> => <<"MUST NOT APPEAR">>}
    end),
    meck:expect(m_category, get, fun(_, _) -> [{parent_id, undefined}] end),
    meck:expect(z_template_compiler_runtime, map_template, fun({cat, Name, _}, _, C) ->
        anonymous(C),
        case Name of <<"_agent_block_unknown.tpl">> -> {error, enoent}; _ -> {ok, Name} end
    end),
    meck:expect(z_template, render_to_iolist_result, fun
        ({cat, "_agent_content.tpl"}, Vars, C) ->
            anonymous(C), {ok, [proplists:get_value(body_html, Vars), proplists:get_value(blocks_html, Vars)], C};
        (<<"_agent_block_text.tpl">>, Vars, C) -> {ok, maps:get(<<"body">>, proplists:get_value(blk, Vars)), C};
        (<<"_agent_block_header.tpl">>, Vars, C) -> {ok, ["<h2>", maps:get(<<"header">>, proplists:get_value(blk, Vars)), "</h2>"], C};
        (<<"_agent_block_page.tpl">>, Vars, C) ->
            {ok, case proplists:get_value(reference, Vars) of undefined -> []; Ref -> maps:get(body, Ref) end, C}
    end),
    meck:expect(m_media, get, fun(_, _) -> undefined end),
    meck:expect(m_rsc, uri, fun(_, C) -> anonymous(C), <<"https://example.test/id/1">> end),
    meck:expect(m_rsc, p, fun
        (_, <<"page_url_abs">>, C) -> anonymous(C), <<"https://example.test/page">>;
        (4, <<"seo_noindex">>, _) -> true;
        (_, _, C) -> anonymous(C), undefined
    end),
    meck:expect(m_config, get_boolean, fun(seo, noindex, C) -> anonymous(C), false end),
    meck:expect(z_dispatcher, url_for, fun(D, _, none, C) -> anonymous(C), <<"/", (atom_to_binary(D))/binary>> end),
    meck:expect(z_context, abs_url, fun(P, _) -> <<"https://example.test", P/binary>> end),
    Modules.

cleanup(Modules) -> lists:foreach(fun meck:unload/1, Modules).
context() -> #context{user_id = 1, acl = admin, language = [en], cowreq = #{}, cowenv = #{}}.
anonymous(#context{user_id = undefined, acl = undefined}) -> ok;
anonymous(_) -> error(non_anonymous).

limits() ->
    lists:foreach(fun(Args) ->
        ?assertEqual({error, bad_request}, m_agent_content:read(search, undefined, Args, context()))
    end, [#{<<"page">> => 0}, #{<<"pagelen">> => 101}, #{<<"page">> => 501},
        #{<<"page">> => <<"invalid">>}, #{<<"text">> => binary:copy(<<"x">>, 513)},
        #{<<"sort">> => <<"random">>}, #{<<"text">> => <<255>>},
        #{<<"publication_after">> => <<"0000-01-01">>},
        #{<<"publication_after">> => <<"2026-+1-01">>}, #{<<"publication_after">> => <<"2026-02-30">>},
        #{<<"publication_after">> => <<"2026-12-01">>, <<"publication_before">> => <<"2026-01-01">>},
        #{<<"options">> => #{}}, #{<<"language">> => <<"zz-invalid">>}]),
    ?assertMatch({ok, #{language := nl, pagelen := 20}}, m_agent_content:validate(search, #{}, context())).

localized_content() ->
    {ok, R} = m_agent_content:read(resource, 1, #{<<"language">> => <<"de">>}, context()),
    ?assertMatch(#{title := <<"Titel">>, language := nl, requested_language := de,
        is_fallback := true, content_complete := false}, R),
    ?assertEqual(false, maps:is_key(<<"private_secret">>, R)),
    Markdown = controller_agent_content:markdown(R, context()),
    ?assertNotEqual(nomatch, binary:match(Markdown, <<"Public body">>)),
    ?assertNotEqual(nomatch, binary:match(Markdown, <<"Unsupported block">>)),
    {ok, English} = m_agent_content:read(resource, 1, #{<<"language">> => <<"en">>}, context()),
    ?assertMatch(#{title := <<"Title">>, is_fallback := false}, English).

markdown_api_access() ->
    meck:expect(z_dispatcher, url_for, fun(Dispatch, Args, none, C) ->
        anonymous(C),
        case Dispatch of
            agent_api_help ->
                ?assertEqual([], Args),
                ?assertEqual('x-default', z_context:language(C)),
                <<"/api-help">>;
            agent_openapi ->
                ?assertEqual([], Args),
                ?assertEqual('x-default', z_context:language(C)),
                <<"/openapi.json">>;
            agent_resource ->
                ?assertEqual(1, proplists:get_value(id, Args)),
                ?assertEqual(nl, proplists:get_value(language, Args)),
                <<"/api/agent/resource/1?language=nl">>;
            _ -> <<"/other">>
        end
    end),
    %% A German request falls back to Dutch; the JSON link follows actual content.
    {ok, R} = m_agent_content:read(resource, 1, #{<<"language">> => <<"de">>}, context()),
    Text = controller_agent_content:markdown(R, context()),
    ?assertNotEqual(nomatch, binary:match(Text, <<"\n## API access\n\n",
        "- [API help](<https://example.test/api-help>)\n",
        "- [OpenAPI specification](<https://example.test/openapi.json>)\n",
        "- [This resource as JSON](<https://example.test/api/agent/resource/1?language=nl>)\n">>)),
    ?assertEqual(nomatch, binary:match(maps:get(markdown, R), <<"## API access">>)).

private_content() ->
    lists:foreach(fun(Id) ->
        ?assertEqual({error, not_found}, m_agent_content:read(resource, Id, #{}, context())),
        ?assertEqual({error, not_found}, m_agent_content:read(related, Id, #{}, context()))
    end, [2, 3, 5, 6, undefined]),
    ?assertMatch({ok, _}, m_agent_content:read(resource, 4, #{}, context())).

rendering_failure() ->
    meck:expect(z_template, render_to_iolist_result, fun(_, _, _) -> {error, enoent} end),
    ?assertEqual({error, unavailable}, m_agent_content:read(resource, 1, #{}, context())),
    meck:expect(z_context, ensure_qs, fun(C) -> C end),
    meck:expect(z_context, get_q, fun(<<"id">>, _) -> 1 end),
    C = z_context:set(agent_operation, markdown, (context())#context{cowreq = #{qs => <<>>}}),
    {{halt, 503}, Failed} = controller_agent_content:resource_exists(C),
    Headers = maps:get(resp_headers, Failed#context.cowreq),
    ?assertNotEqual(nomatch, binary:match(maps:get(<<"cache-control">>, Headers), <<"no-store">>)),
    ?assertEqual(undefined, z_context:get(agent_body, Failed)).

relationship_boundaries() ->
    %% Two fully filtered windows, then one public row, then an empty last page.
    meck:expect(z_db, qmap, fun(_, [1, 3, Offset], _, _) ->
        {ok, case Offset of
            0 -> [#{endpoint_id => 2, predicate_id => 1} || _ <- lists:seq(1,3)];
            2 -> [#{endpoint_id => 2, predicate_id => 1} || _ <- lists:seq(1,3)];
            4 -> [#{endpoint_id => 1, predicate_id => 1}, #{endpoint_id => 2, predicate_id => 1}, #{endpoint_id => 2, predicate_id => 1}];
            6 -> [#{endpoint_id => 2, predicate_id => 1}]
        end}
    end),
    lists:foreach(fun(Page) ->
        ?assertMatch({ok, #{items := [], next := N}} when N =:= Page + 1,
            m_agent_content:read(related, 1, #{<<"page">> => Page, <<"pagelen">> => 2}, context()))
    end, [1,2]),
    ?assertMatch({ok, #{items := [_], next := 4}},
        m_agent_content:read(related, 1, #{<<"page">> => 3, <<"pagelen">> => 2}, context())),
    ?assertMatch({ok, #{items := [], next := false, next_url := undefined}},
        m_agent_content:read(related, 1, #{<<"page">> => 4, <<"pagelen">> => 2}, context())),
    meck:expect(z_db, qmap, fun(_, [1, 101, 9900], _, _) ->
        {ok, [#{endpoint_id => 2, predicate_id => 1} || _ <- lists:seq(1,101)]}
    end),
    ?assertMatch({ok, #{items := [], next := false, next_url := undefined, limit_reached := true}},
        m_agent_content:read(related, 1, #{<<"page">> => 100, <<"pagelen">> => 100}, context())),
    ?assertEqual({error, bad_request},
        m_agent_content:read(related, 1, #{<<"page">> => 101, <<"pagelen">> => 100}, context())).

bounded_search() ->
    meck:expect(m_search, search, fun(<<"query">>, Args, C) ->
        anonymous(C),
        ?assertEqual(#{is_single_page => true}, maps:get(<<"options">>, Args)),
        ?assertEqual(20, maps:get(<<"pagelen">>, Args)),
        {ok, #search_result{result = [1, 2, 3, 4], next = 2, total = 999}}
    end),
    {ok, R} = m_agent_content:read(search, undefined, #{}, context()),
    ?assertMatch(#{items := [#{id := 1}], next := 2}, R),
    ?assertNot(maps:is_key(total, R)),
    ?assertMatch({ok, #{items := [_]}}, m_agent_content:read(schema, undefined, #{}, context())).

bounded_edges() ->
    meck:expect(z_db, qmap, fun(Sql, [1, 21, 0], Options, C) ->
        anonymous(C),
        ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Sql), <<"limit $2 offset $3">>)),
        ?assertEqual(3000, proplists:get_value(timeout, Options)),
        {ok, [#{endpoint_id => 1, predicate_id => 1}, #{endpoint_id => 2, predicate_id => 1}]}
    end),
    ?assertMatch({ok, #{items := [_], next := false}}, m_agent_content:read(related, 1, #{}, context())).

conditional_content() ->
    meck:expect(z_context, ensure_qs, fun(C) -> C end),
    meck:expect(z_context, get_q, fun(<<"id">>, _) -> 1 end),
    C0 = (context())#context{cowreq = #{qs => <<>>}},
    {true, Public} = controller_agent_content:service_available(C0),
    anonymous(Public),
    ?assertEqual(<<"public, max-age=0, must-revalidate">>,
        maps:get(<<"cache-control">>, maps:get(resp_headers, Public#context.cowreq))),
    ?assertEqual({[<<"GET">>, <<"HEAD">>], Public}, controller_agent_content:allowed_methods(Public)),
    C = z_context:set(agent_operation, resource, Public),
    {true, Fresh} = controller_agent_content:resource_exists(C),
    {ETag, _} = controller_agent_content:generate_etag(Fresh),
    {true, Again} = controller_agent_content:resource_exists(C),
    ?assertEqual({ETag, Again}, controller_agent_content:generate_etag(Again)),
    meck:expect(m_rsc, get, fun(_, _) -> #{<<"title">> => <<"Changed">>} end),
    {true, Changed} = controller_agent_content:resource_exists(C),
    {ChangedETag, _} = controller_agent_content:generate_etag(Changed),
    ?assertNotEqual(ETag, ChangedETag),
    meck:expect(z_acl, rsc_visible, fun(_, _) -> false end),
    ?assertMatch({{halt, 404}, _}, controller_agent_content:resource_exists(C)).

execution_budget() ->
    Parent = self(),
    meck:expect(m_search, search, fun(_, _, _) ->
        Parent ! {worker, self()},
        receive never -> {error, unavailable} end
    end),
    ?assertEqual({error, timeout}, m_agent_content:read(search, undefined, #{}, context())),
    receive {worker, Pid} -> ?assertNot(is_process_alive(Pid)) after 100 -> error(no_worker) end.

survey_blocks() ->
    meck:expect(m_rsc, get, fun(_, _) -> #{
        <<"title">> => <<"Article">>, <<"body">> => <<"<p>Editorial text</p>">>,
        <<"blocks">> => [#{<<"type">> => <<"survey_short_answer">>, <<"body">> => <<"Hidden question">>},
            #{<<"type">> => <<"survey_future_type">>, <<"body">> => <<"Hidden future question">>}]
    } end),
    {ok, R} = m_agent_content:read(resource, 1, #{}, context()),
    ?assertEqual(true, maps:get(content_complete, R)),
    Text = controller_agent_content:markdown(R, context()),
    ?assertEqual(nomatch, binary:match(Text, <<"Hidden">>)),
    ?assertEqual(nomatch, binary:match(Text, <<"Unsupported">>)),
    ?assertNotEqual(nomatch, binary:match(Text, <<"Editorial text">>)).

search_filters() ->
    meck:expect(m_search, search, fun(_, Args, C) ->
        anonymous(C),
        ?assertEqual([<<"-publication_start">>, <<"id">>], maps:get(<<"asort">>, Args)),
        ?assertEqual({{2026,1,1},{0,0,0}}, maps:get(<<"publication_after">>, Args)),
        ?assertEqual({{2026,11,10},{23,59,59}}, maps:get(<<"publication_before">>, Args)),
        {ok, #search_result{result = [1], next = 2}}
    end),
    meck:expect(z_dispatcher, url_for, fun(_, Args, none, _) ->
        case proplists:get_value(page, Args) of
            2 -> ?assertEqual(<<"newest">>, proplists:get_value(sort, Args)),
                 ?assertEqual(<<"2026-01-01">>, proplists:get_value(publication_after, Args));
            _ -> ok
        end,
        <<"/next">>
    end),
    ?assertMatch({ok, #{next := 2}}, m_agent_content:read(search, undefined,
        #{<<"sort">> => <<"newest">>, <<"publication_after">> => <<"2026-01-01">>,
          <<"publication_before">> => <<"2026-11-10">>}, context())).

schema_constraints() ->
    meck:expect(m_search, search, fun(_, _, _) -> {ok, #search_result{result = [1]}} end),
    meck:expect(z_db, qmap, fun(_, [1], _, _) -> {ok, [
        #{category_id => 1, is_subject => true}, #{category_id => 2, is_subject => false}]} end),
    ?assertMatch({ok, #{items := [#{subject_categories := [#{id := 1}], object_categories := []}]}},
        m_agent_content:read(schema, undefined, #{<<"kind">> => <<"predicate">>}, context())).

private_page_block() ->
    meck:expect(m_rsc, get, fun(1, _) -> #{<<"body">> => <<"<p>Public text</p>">>,
        <<"blocks">> => [#{<<"type">> => <<"page">>, <<"rsc_id">> => 2}]} end),
    {ok, R} = m_agent_content:read(resource, 1, #{}, context()),
    ?assertEqual(true, maps:get(content_complete, R)),
    ?assertEqual(<<"Public text">>, string:trim(maps:get(markdown, R))).

page_block_dependencies() ->
    SetChild = fun(Text) -> meck:expect(m_rsc, get, fun
        (1, _) -> #{<<"body">> => <<"<p>Parent</p>">>,
            <<"blocks">> => [#{<<"type">> => <<"page">>, <<"rsc_id">> => 7}]};
        (7, _) -> #{<<"body">> => Text}
    end) end,
    SetChild(<<"<p>Original child</p>">>),
    {ok, Before} = m_agent_content:read(resource, 1, #{}, context()),
    ?assertNotEqual(nomatch, binary:match(maps:get(markdown, Before), <<"Original child">>)),
    SetChild(<<"<p>Updated child</p>">>),
    {ok, After} = m_agent_content:read(resource, 1, #{}, context()),
    ?assertNotEqual(maps:get(markdown, Before), maps:get(markdown, After)),
    meck:expect(z_acl, rsc_visible, fun(Id, _) -> Id =/= 7 end),
    {ok, Hidden} = m_agent_content:read(resource, 1, #{}, context()),
    ?assertEqual(nomatch, binary:match(maps:get(markdown, Hidden), <<"child">>)).

%% Exercise the real SQL-term combiner with an earlier relevance term, rather
%% than merely asserting which arguments the facade passes to m_search.
explicit_sort_precedes_relevance() ->
    lists:foreach(fun({Sort, Prefix}) ->
        meck:expect(m_search, search, fun(_, Args, C) ->
            Rank = #search_sql_term{sort = [<<"relevance_rank desc">>]},
            OrderTerms = [search_query:qterm(#{<<"term">> => Key, <<"value">> => Value}, false, C)
                || {Key, Value} <- maps:to_list(Args),
                   Key =:= <<"sort">> orelse Key =:= <<"asort">>],
            SQL = z_search_terms:combine(#search_sql_terms{terms = [Rank | OrderTerms]}),
            ?assertEqual(<<Prefix/binary, ", relevance_rank desc">>, SQL#search_sql.order),
            {ok, #search_result{}}
        end),
        ?assertMatch({ok, _}, m_agent_content:read(search, undefined,
            #{<<"text">> => <<"template">>, <<"sort">> => Sort}, context()))
    end, [{<<"id">>, <<"rsc.id ASC">>},
          {<<"newest">>, <<"rsc.publication_start DESC, rsc.id ASC">>},
          {<<"oldest">>, <<"rsc.publication_start ASC, rsc.id ASC">>},
          {<<"modified">>, <<"rsc.modified DESC, rsc.id ASC">>}]).
