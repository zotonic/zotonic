%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Model providing aggregate keyword usage and overlap data.
%% @end

%% Copyright 2026 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(m_admin_keyword).
-moduledoc(#{
    zotonic_keywords => ["reference", "backend_developer", "model", "metadata", "categorization", "query"]
}).
-moduledoc("
Provide aggregate keyword usage and overlap data for `mod_admin_keyword`.

Keywords are resources in the `keyword` category or one of its descendants.
Usage means a resource has an outgoing `subject` edge to the keyword. The
dashboard can restrict both keyword and content category subtrees, and can
filter on the resource publication flag.

The overlap data contains three complementary measures:

* `intersection`: number of resources using both keywords;
* `jaccard`: intersection divided by the union of both keyword resource sets;
* `overlap`: intersection divided by the smaller keyword resource set.

The Jaccard percentage is useful for general similarity. Smaller-set overlap is
useful for finding a narrow keyword that is nearly contained in a broader one.

The `use` permission for `mod_admin_keyword` grants access to aggregate counts
over all matching resources. Individual content titles are not returned by
this model.


Available Model API Paths
-------------------------

| Method | Path pattern | Description |
| --- | --- | --- |
| `get` | `/dashboard` | Return the analyzed resource count, usage, overlap matrix, ranked pairs, normalized filters, and truncation metadata. Accepts `keyword_category`, `content_category`, `publication`, `minimum`, `matrix_limit`, and `metric` in the payload. |
").

-behaviour(zotonic_model).

-export([
    m_get/3,
    dashboard/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(DEFAULT_MATRIX_LIMIT, 15).
-define(MAX_MATRIX_LIMIT, 30).
-define(MAX_USAGE_ROWS, 250).
-define(MAX_PAIR_ROWS, 100).


-spec m_get(list(), zotonic_model:opt_msg(), z:context()) -> zotonic_model:return().
m_get([<<"dashboard">> | Rest], #{payload := Payload}, Context) when is_map(Payload) ->
    case dashboard(Payload, Context) of
        {ok, Result} -> {ok, {Result, Rest}};
        {error, _} = Error -> Error
    end;
m_get([<<"dashboard">> | Rest], _Msg, Context) ->
    case dashboard(#{}, Context) of
        {ok, Result} -> {ok, {Result, Rest}};
        {error, _} = Error -> Error
    end;
m_get(_Path, _Msg, _Context) ->
    {error, unknown_path}.


-spec dashboard(map(), z:context()) -> {ok, map()} | {error, eacces}.
dashboard(Payload, Context) ->
    case z_acl:is_allowed(use, mod_admin_keyword, Context) of
        true -> {ok, dashboard_allowed(Payload, Context)};
        false -> {error, eacces}
    end.


dashboard_allowed(Payload, Context) ->
    case m_category:name_to_id(keyword, Context) of
        {ok, KeywordRootId} ->
            Filters = normalize_filters(Payload, KeywordRootId, Context),
            KeywordCategoryIds = m_category:contains(
                maps:get(keyword_category, Filters),
                Context),
            ContentCategoryIds = content_category_ids(
                maps:get(content_category, Filters),
                Context),
            SubjectPredicateId = case m_rsc:rid(subject, Context) of
                undefined -> 0;
                Id -> Id
            end,
            case content_count(ContentCategoryIds, Filters, Context) of
                {ok, #{content_count := ContentCount}} ->
                    case usage_rows(
                        KeywordCategoryIds,
                        SubjectPredicateId,
                        ContentCategoryIds,
                        Filters,
                        Context)
                    of
                        {ok, Rows0} ->
                            report(
                                Rows0,
                                ContentCount,
                                SubjectPredicateId,
                                ContentCategoryIds,
                                Filters,
                                Context);
                        {error, Reason} ->
                            log_query_error(<<"Could not calculate keyword usage">>, Reason),
                            empty_report(Filters, true)
                    end;
                {error, Reason} ->
                    log_query_error(<<"Could not count analyzed resources">>, Reason),
                    empty_report(Filters, true)
            end;
        {error, _} ->
            empty_report(default_filters(), false)
    end.


normalize_filters(Payload, KeywordRootId, Context) ->
    KeywordCategoryId = keyword_category(
        maps:get(<<"keyword_category">>, Payload, undefined),
        KeywordRootId,
        Context),
    #{
        keyword_category => KeywordCategoryId,
        content_category => content_category(
            maps:get(<<"content_category">>, Payload, undefined),
            Context),
        publication => publication(maps:get(<<"publication">>, Payload, <<"all">>)),
        minimum => bounded_integer(maps:get(<<"minimum">>, Payload, 0), 0, 100000, 0),
        matrix_limit => bounded_integer(
            maps:get(<<"matrix_limit">>, Payload, ?DEFAULT_MATRIX_LIMIT),
            5,
            ?MAX_MATRIX_LIMIT,
            ?DEFAULT_MATRIX_LIMIT),
        metric => metric(maps:get(<<"metric">>, Payload, <<"jaccard">>))
    }.


default_filters() ->
    #{
        keyword_category => undefined,
        content_category => undefined,
        publication => <<"all">>,
        minimum => 0,
        matrix_limit => ?DEFAULT_MATRIX_LIMIT,
        metric => <<"jaccard">>
    }.


keyword_category(undefined, KeywordRootId, _Context) ->
    KeywordRootId;
keyword_category(<<>>, KeywordRootId, _Context) ->
    KeywordRootId;
keyword_category(Value, KeywordRootId, Context) ->
    KeywordCategoryIds = m_category:contains(KeywordRootId, Context),
    case m_category:name_to_id(Value, Context) of
        {ok, Id} ->
            case lists:member(Id, KeywordCategoryIds) of
                true -> Id;
                false -> KeywordRootId
            end;
        {error, _} ->
            KeywordRootId
    end.


content_category(undefined, _Context) ->
    undefined;
content_category(<<>>, _Context) ->
    undefined;
content_category(Value, Context) ->
    case m_category:name_to_id(Value, Context) of
        {ok, Id} -> Id;
        {error, _} -> undefined
    end.


content_category_ids(undefined, Context) ->
    m_category:all(Context);
content_category_ids(CategoryId, Context) ->
    m_category:contains(CategoryId, Context).


publication(<<"published">>) -> <<"published">>;
publication(<<"unpublished">>) -> <<"unpublished">>;
publication(_) -> <<"all">>.


metric(<<"count">>) -> <<"count">>;
metric(<<"overlap">>) -> <<"overlap">>;
metric(_) -> <<"jaccard">>.


bounded_integer(Value, Minimum, Maximum, Default) ->
    case z_convert:to_integer(Value) of
        N when is_integer(N), N >= Minimum, N =< Maximum -> N;
        N when is_integer(N), N < Minimum -> Minimum;
        N when is_integer(N), N > Maximum -> Maximum;
        _ -> Default
    end.


content_count(ContentCategoryIds, Filters, Context) ->
    z_db:qmap_row("
        select count(*)::integer as content_count
        from rsc
        where category_id = any($1::int[])
          and (
                $2 = 'all'
                or ($2 = 'published' and is_published)
                or ($2 = 'unpublished' and not is_published)
          )",
        [ContentCategoryIds, maps:get(publication, Filters)],
        [{keys, atom}, {timeout, 60000}],
        Context).


usage_rows(KeywordCategoryIds, PredicateId, ContentCategoryIds, Filters, Context) ->
    z_db:qmap("
        select
            k.id,
            k.category_id,
            coalesce(k.pivot_title, k.name, '#' || k.id::text) as title,
            coalesce(kc.pivot_title, kc.name, '') as category_title,
            count(distinct content.id)::integer as usage_count,
            (count(*) over())::integer as total_count
        from rsc k
        join rsc kc on kc.id = k.category_id
        left join edge e
            on e.object_id = k.id
           and e.predicate_id = $2
        left join rsc content
            on content.id = e.subject_id
           and content.category_id = any($3::int[])
           and (
                $4 = 'all'
                or ($4 = 'published' and content.is_published)
                or ($4 = 'unpublished' and not content.is_published)
           )
        where k.category_id = any($1::int[])
        group by k.id, k.category_id, k.pivot_title, k.name,
                 kc.pivot_title, kc.name
        having count(distinct content.id) >= $5
        order by usage_count desc,
                 lower(coalesce(k.pivot_title, k.name, '#' || k.id::text)),
                 k.id
        limit $6",
        [
            KeywordCategoryIds,
            PredicateId,
            ContentCategoryIds,
            maps:get(publication, Filters),
            maps:get(minimum, Filters),
            ?MAX_USAGE_ROWS
        ],
        [{keys, atom}, {timeout, 60000}],
        Context).


report(Rows0, ContentCount, PredicateId, ContentCategoryIds, Filters, Context) ->
    TotalCount = case Rows0 of
        [#{total_count := Count} | _] -> Count;
        [] -> 0
    end,
    Usage = [maps:remove(total_count, Row) || Row <- Rows0],
    Used = [Row || #{usage_count := Count} = Row <- Usage, Count > 0],
    MatrixKeywords = lists:sublist(Used, maps:get(matrix_limit, Filters)),
    KeywordIds = [Id || #{id := Id} <- Used],
    IntersectionRows = intersection_rows(
        KeywordIds,
        PredicateId,
        ContentCategoryIds,
        maps:get(publication, Filters),
        Context),
    case IntersectionRows of
        {ok, Intersections} ->
            IntersectionsByPair = intersection_map(Intersections),
            MatrixPairs = make_pairs(
                MatrixKeywords,
                IntersectionsByPair,
                maps:get(metric, Filters)),
            Pairs = rank_pairs(make_observed_pairs(
                Used,
                Intersections,
                maps:get(metric, Filters))),
            #{
                is_available => true,
                has_error => false,
                filters => Filters,
                content_count => ContentCount,
                usage => Usage,
                total_keywords => TotalCount,
                displayed_keywords => length(Usage),
                is_truncated => TotalCount > length(Usage),
                max_usage => max_usage(Usage),
                matrix_keywords => MatrixKeywords,
                matrix => make_matrix(MatrixKeywords, MatrixPairs),
                pairs => lists:sublist(Pairs, ?MAX_PAIR_ROWS),
                total_pairs => length(Pairs),
                is_percent => maps:get(metric, Filters) =/= <<"count">>
            };
        {error, Reason} ->
            log_query_error(<<"Could not calculate keyword overlap">>, Reason),
            empty_report(Filters, true)
    end.


log_query_error(Text, Reason) ->
    ?LOG_ERROR(#{
        in => zotonic_mod_admin_keyword,
        text => Text,
        result => error,
        reason => Reason
    }).


intersection_rows([], _PredicateId, _ContentCategoryIds, _Publication, _Context) ->
    {ok, []};
intersection_rows([_], _PredicateId, _ContentCategoryIds, _Publication, _Context) ->
    {ok, []};
intersection_rows(KeywordIds, PredicateId, ContentCategoryIds, Publication, Context) ->
    z_db:qmap("
        with filtered_edges as (
            select distinct e.subject_id, e.object_id
            from edge e
            join rsc content on content.id = e.subject_id
            where e.predicate_id = $1
              and e.object_id = any($2::int[])
              and content.category_id = any($3::int[])
              and (
                    $4 = 'all'
                    or ($4 = 'published' and content.is_published)
                    or ($4 = 'unpublished' and not content.is_published)
              )
        )
        select
            a.object_id as a_id,
            b.object_id as b_id,
            count(*)::integer as intersection
        from filtered_edges a
        join filtered_edges b
          on b.subject_id = a.subject_id
         and b.object_id > a.object_id
        group by a.object_id, b.object_id",
        [PredicateId, KeywordIds, ContentCategoryIds, Publication],
        [{keys, atom}, {timeout, 60000}],
        Context).


intersection_map(Rows) ->
    maps:from_list([
        {{AId, BId}, Count}
        || #{a_id := AId, b_id := BId, intersection := Count} <- Rows
    ]).


make_observed_pairs(Keywords, Intersections, Metric) ->
    KeywordsById = maps:from_list([
        {Id, Keyword}
        || #{id := Id} = Keyword <- Keywords
    ]),
    [
        make_pair(
            maps:get(AId, KeywordsById),
            maps:get(BId, KeywordsById),
            #{{AId, BId} => Intersection},
            Metric)
        || #{a_id := AId, b_id := BId, intersection := Intersection} <- Intersections
    ].


make_pairs(Keywords, Intersections, Metric) ->
    Pairs0 = make_pairs_1(Keywords, Intersections, Metric),
    MaxScore = lists:foldl(
        fun(#{score := Score}, Acc) -> erlang:max(Score, Acc) end,
        0,
        Pairs0),
    [Pair#{bucket => bucket(Pair, Metric, MaxScore)} || Pair <- Pairs0].


make_pairs_1([], _Intersections, _Metric) ->
    [];
make_pairs_1([A | Rest], Intersections, Metric) ->
    [make_pair(A, B, Intersections, Metric) || B <- Rest]
        ++ make_pairs_1(Rest, Intersections, Metric).


make_pair(#{id := AId, usage_count := ACount} = A,
          #{id := BId, usage_count := BCount} = B,
          Intersections,
          Metric) ->
    Intersection = maps:get(ordered_pair(AId, BId), Intersections, 0),
    Jaccard = percentage(Intersection, ACount + BCount - Intersection),
    Overlap = percentage(Intersection, erlang:min(ACount, BCount)),
    Score = selected_score(Metric, Intersection, Jaccard, Overlap),
    #{
        a => A,
        b => B,
        a_id => AId,
        b_id => BId,
        intersection => Intersection,
        jaccard => Jaccard,
        overlap => Overlap,
        score => Score
    }.


ordered_pair(AId, BId) when AId < BId -> {AId, BId};
ordered_pair(AId, BId) -> {BId, AId}.


percentage(_Numerator, 0) ->
    0;
percentage(Numerator, Denominator) ->
    round(100 * Numerator / Denominator).


selected_score(<<"count">>, Intersection, _Jaccard, _Overlap) -> Intersection;
selected_score(<<"overlap">>, _Intersection, _Jaccard, Overlap) -> Overlap;
selected_score(_Metric, _Intersection, Jaccard, _Overlap) -> Jaccard.


bucket(#{score := 0}, _Metric, _MaxScore) ->
    0;
bucket(#{score := Score}, <<"count">>, MaxScore) ->
    erlang:min(10, 1 + (9 * Score div erlang:max(1, MaxScore)));
bucket(#{score := Score}, _Metric, _MaxScore) ->
    erlang:min(10, 1 + (Score div 10)).


rank_pairs(Pairs) ->
    lists:sort(
        fun(A, B) -> pair_sort_key(A) > pair_sort_key(B) end,
        [Pair || #{intersection := Count} = Pair <- Pairs, Count > 0]).


pair_sort_key(#{score := Score, intersection := Intersection, a_id := AId, b_id := BId}) ->
    {Score, Intersection, -AId, -BId}.


make_matrix(Keywords, Pairs) ->
    PairMap = maps:from_list([
        {ordered_pair(AId, BId), Pair}
        || #{a_id := AId, b_id := BId} = Pair <- Pairs
    ]),
    [
        #{
            keyword => Keyword,
            cells => [matrix_cell(Keyword, Column, PairMap) || Column <- Keywords]
        }
        || Keyword <- Keywords
    ].


matrix_cell(#{id := Id, usage_count := Count}, #{id := Id} = Keyword, _PairMap) ->
    #{
        is_diagonal => true,
        keyword => Keyword,
        usage_count => Count,
        bucket => 0
    };
matrix_cell(#{id := RowId}, #{id := ColumnId} = Keyword, PairMap) ->
    Pair = maps:get(ordered_pair(RowId, ColumnId), PairMap),
    Pair#{
        is_diagonal => false,
        keyword => Keyword
    }.


max_usage([]) -> 0;
max_usage([#{usage_count := Count} | _]) -> Count.


empty_report(Filters, IsError) ->
    #{
        is_available => false,
        has_error => IsError,
        filters => Filters,
        content_count => 0,
        usage => [],
        total_keywords => 0,
        displayed_keywords => 0,
        is_truncated => false,
        max_usage => 0,
        matrix_keywords => [],
        matrix => [],
        pairs => [],
        total_pairs => 0,
        is_percent => true
    }.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

percentage_test() ->
    ?assertEqual(0, percentage(0, 0)),
    ?assertEqual(25, percentage(1, 4)),
    ?assertEqual(67, percentage(2, 3)).

pair_metrics_test() ->
    A = #{id => 1, title => <<"A">>, usage_count => 8},
    B = #{id => 2, title => <<"B">>, usage_count => 5},
    Pair = make_pair(A, B, #{{1, 2} => 4}, <<"jaccard">>),
    ?assertEqual(4, maps:get(intersection, Pair)),
    ?assertEqual(44, maps:get(jaccard, Pair)),
    ?assertEqual(80, maps:get(overlap, Pair)),
    ?assertEqual(44, maps:get(score, Pair)).

matrix_is_symmetric_test() ->
    A = #{id => 1, title => <<"A">>, usage_count => 8},
    B = #{id => 2, title => <<"B">>, usage_count => 5},
    Pairs = make_pairs([A, B], #{{1, 2} => 4}, <<"overlap">>),
    [#{cells := [_, AB]}, #{cells := [BA, _]}] = make_matrix([A, B], Pairs),
    ?assertEqual(maps:get(score, AB), maps:get(score, BA)),
    ?assertEqual(80, maps:get(score, AB)).

observed_pairs_include_keywords_outside_matrix_test() ->
    Keywords = [
        #{id => 1, title => <<"A">>, usage_count => 10},
        #{id => 2, title => <<"B">>, usage_count => 8},
        #{id => 3, title => <<"C">>, usage_count => 3}
    ],
    Intersections = [#{a_id => 2, b_id => 3, intersection => 2}],
    [Pair] = make_observed_pairs(Keywords, Intersections, <<"jaccard">>),
    ?assertEqual(2, maps:get(a_id, Pair)),
    ?assertEqual(3, maps:get(b_id, Pair)),
    ?assertEqual(2, maps:get(intersection, Pair)).

-endif.
