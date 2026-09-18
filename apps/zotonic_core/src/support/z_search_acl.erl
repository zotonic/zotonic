%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Add access-control and category restrictions to SQL searches.
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

-module(z_search_acl).

-export([
    reformat_sql_query/3,
    add_sql_checks/2
]).

-include_lib("zotonic.hrl").


%% @doc Inject ACL and category checks into a complete SQL query.
-spec reformat_sql_query(Query, Options, Context) -> Query1
    when
        Query :: #search_sql{},
        Options :: z_search:search_options(),
        Context :: z:context(),
        Query1 :: #search_sql{}.
reformat_sql_query(#search_sql{ where = Where } = Query, Options, Context) ->
    {ExtraWhere, Args} = add_sql_checks(Query, Context),
    Where1 = case Where of
        <<>> -> [];
        B when is_binary(B) -> [B];
        L when is_list(L) -> L
    end,
    Where2 = iolist_to_binary(concat_where(ExtraWhere, Where1)),
    Query1 = Query#search_sql{ where = Where2, args = Args },
    case Options of
        #{ is_count_rows := true } ->
            Query1#search_sql{
                select = "count(*)",
                limit = "",
                order = ""
            };
        #{} ->
            Query1
    end.

%% @doc Return ACL and category conditions for all declared table aliases.
-spec add_sql_checks(Query, Context) -> {Conditions, Args}
    when
        Query :: #search_sql{},
        Context :: z:context(),
        Conditions :: [iodata()],
        Args :: list().
add_sql_checks(#search_sql{
        tables = Tables0,
        args = Args,
        cats = TabCats0,
        cats_exclude = TabCatsExclude0,
        cats_exact = TabCatsExact0
    } = Query, Context) ->
    TabCats = normalize_aliases(TabCats0),
    TabCatsExclude = normalize_aliases(TabCatsExclude0),
    TabCatsExact = normalize_aliases(TabCatsExact0),
    Tables = [
        {z_convert:to_binary(Table), z_convert:to_binary(Alias)}
        || {Table, Alias} <- Tables0
    ],
    CatsPerAlias = cats_per_alias(TabCats, TabCatsExclude, TabCatsExact, Context),
    {AclWhere, Args1} = lists:foldl(
        fun(Table, {Conditions, AccArgs}) ->
            {Condition, NewArgs} = add_acl_check(
                Table, AccArgs, Query, CatsPerAlias, Context),
            {[Condition | Conditions], NewArgs}
        end,
        {[], Args},
        Tables),
    lists:foldl(
        fun({Alias, Cats}, {Conditions, AccArgs}) ->
            case add_cat_check(Alias, Cats, AccArgs, Context) of
                {[], NewArgs} -> {Conditions, NewArgs};
                {Condition, NewArgs} -> {[Condition | Conditions], NewArgs}
            end
        end,
        {AclWhere, Args1},
        CatsPerAlias).

normalize_aliases(AliasValues) ->
    [ {z_convert:to_binary(Alias), Value} || {Alias, Value} <- AliasValues ].

%% @doc Compute the final category IDs for every restricted alias.
cats_per_alias(TabCats, TabExclude, TabExact, Context) ->
    AllAlias = lists:usort(
        [ Alias || {Alias, _} <- TabCats ] ++
        [ Alias || {Alias, _} <- TabExclude ] ++
        [ Alias || {Alias, _} <- TabExact ]),
    lists:map(
        fun(Alias) ->
            Include = make_rids(flatten(proplists:get_value(Alias, TabCats, [])), Context),
            Exclude = make_rids(flatten(proplists:get_value(Alias, TabExclude, [])), Context),
            Exact = case flatten(proplists:get_value(Alias, TabExact, [])) of
                [] -> [];
                ExactIds ->
                    case make_rids(ExactIds, Context) of
                        [] -> none;
                        EIds -> EIds
                    end
            end,
            {Alias, cats_to_find(Include, Exclude, Exact, Context)}
        end,
        AllAlias).

flatten(L) when is_list(L) -> lists:flatten(L);
flatten(undefined) -> [];
flatten(A) -> [A].

make_rids(Ids, Context) ->
    lists:filtermap(
        fun(Id) ->
            case m_rsc:rid(Id, Context) of
                undefined -> false;
                RId -> {true, RId}
            end
        end,
        lists:flatten(Ids)).

cats_to_find(_Include, _Exclude, none, _Context) ->
    [];
cats_to_find([], [], [], _Context) ->
    all;
cats_to_find([], Exclude, [], Context) ->
    IncludeSet = sets:from_list(m_category:all(Context)),
    ExcludeSet = sets:from_list(lists:flatmap(fun(C) -> m_category:contains(C, Context) end, Exclude)),
    lists:sort(sets:to_list(sets:subtract(IncludeSet, ExcludeSet)));
cats_to_find(Include, Exclude, [], Context) ->
    IncludeSet = sets:from_list(lists:flatmap(fun(C) -> m_category:contains(C, Context) end, Include)),
    ExcludeSet = sets:from_list(lists:flatmap(fun(C) -> m_category:contains(C, Context) end, Exclude)),
    lists:sort(sets:to_list(sets:subtract(IncludeSet, ExcludeSet)));
cats_to_find([], Exclude, Exact, Context) ->
    ExcludeContains = lists:usort(lists:flatmap(fun(C) -> m_category:contains(C, Context) end, Exclude)),
    Exact -- ExcludeContains;
cats_to_find(Include, Exclude, Exact, Context) ->
    IncludeSet = sets:from_list(lists:flatmap(fun(C) -> m_category:contains(C, Context) end, Include)),
    ExcludeSet = sets:from_list(lists:flatmap(fun(C) -> m_category:contains(C, Context) end, Exclude)),
    ExactSet = sets:from_list(Exact),
    ToFind = sets:intersection(sets:subtract(ExactSet, ExcludeSet), IncludeSet),
    lists:sort(sets:to_list(ToFind)).

concat_where([], Acc) ->
    Acc;
concat_where([<<>> | Rest], Acc) ->
    concat_where(Rest, Acc);
concat_where([[] | Rest], Acc) ->
    concat_where(Rest, Acc);
concat_where([Where | Rest], []) ->
    concat_where(Rest, [Where]);
concat_where([Where | Rest], Acc) ->
    concat_where(Rest, [Where, " and " | Acc]).

add_acl_check({<<"rsc">>, Alias}, Args, Query, CatsPerAlias, Context) ->
    Cats = proplists:get_value(Alias, CatsPerAlias, all),
    case z_notifier:first(#acl_add_sql_check{
        alias = Alias,
        args = Args,
        search_sql = Query,
        cats = Cats
    }, Context) of
        undefined ->
            case z_acl:is_admin(Context) of
                true -> {[], Args};
                false -> {publish_check(Alias, Query), Args}
            end;
        {_NewSql, _NewArgs} = Result ->
            Result
    end;
add_acl_check(_Table, Args, _Query, _CatsPerAlias, _Context) ->
    {[], Args}.

publish_check(Alias, #search_sql{ extra = Extra }) ->
    case lists:member(no_publish_check, Extra) of
        true ->
            [];
        false ->
            [
                Alias, ".is_published = true and ",
                Alias, ".publication_start <= now() and ",
                Alias, ".publication_end >= now()"
            ]
    end.

add_cat_check(_Alias, all, Args, _Context) ->
    {[], Args};
add_cat_check(_Alias, [], Args, _Context) ->
    {["false"], Args};
add_cat_check(Alias, Cats, Args, Context) ->
    All = m_category:all(Context),
    case lists:usort(Cats) of
        All ->
            {[], Args};
        _ ->
            case m_category:is_tree_dirty(Context) of
                false -> add_cat_check_pivot(Alias, Cats, Args, Context);
                true -> add_cat_check_any(Alias, Cats, Args)
            end
    end.

add_cat_check_pivot(Alias, Cats, Args, Context) ->
    CatChecks = [ cat_check_pivot1(Alias, Range) || Range <- m_category:ranges(Cats, Context) ],
    case CatChecks of
        [] -> {[], Args};
        _ -> {["(", lists:join(" or ", CatChecks), ")"], Args}
    end.

cat_check_pivot1(Alias, {From, From}) ->
    [Alias, ".pivot_category_nr = ", integer_to_list(From)];
cat_check_pivot1(Alias, {From, To}) ->
    [
        Alias, ".pivot_category_nr >= ", integer_to_list(From),
        " and ", Alias, ".pivot_category_nr <= ", integer_to_list(To)
    ].

add_cat_check_any(Alias, Cats, Args) ->
    Args1 = Args ++ [Cats],
    {
        [Alias, ".category_id = any($", integer_to_list(length(Args1)), "::int[])"],
        Args1
    }.
