%all(% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2026 Marc Worrell
%% @doc Model for predicates
%% @end

%% Copyright 2009-2026 Marc Worrell
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

-module(m_predicate).
-moduledoc(#{
    zotonic_keywords => ["reference", "backend_developer", "model", "content_relationships", "predicate", "query"]
}).
-moduledoc("
Retrieve information about predicates. Predicates are the labels on edges (connections between resources) that give
meaning to an edge. An example is the predicate \"author\" which refers to the authors of an article. Predicates form
together with the referring and the referred page a triple `{subject, predicate, object}`.

Each predicate has a list of valid subject categories and valid object categories. This is used to filter the list of
predicates in the admin edit page, and also to filter the list of found potential objects when making a connection.

The predicate property `is_dependency_ignored` defaults to false. When true,
connections using this predicate do not keep their object resource alive during
`is_dependent` cleanup. Protected resources are still kept. This does not change
ordinary connection queries. `dependency_ignored/1` returns the cached list of
predicate IDs with this property enabled.

`all/1` and `m.predicate.all` return a map keyed by binary predicate name.
Each value contains the binary keys `id`, `name`, `uri`, `title`, `summary` and `reversed`.
`get/2`, `m.predicate.author` and `m.predicate[104]` return the same minimal
map for a visible predicate, or `undefined`. Visibility is checked per caller,
after reading the shared structural cache.

The four display properties are read with property permissions for the current
caller; denied or missing values are `undefined`. Read other resource properties
via `m.rsc[p.id]`. Use `object_category` and `subject_category` for
restriction lists (lists of integer category IDs). Model routes reject hidden
predicates/categories. The `is_used` model route additionally requires edit
permission because it inspects connections irrespective of their visibility.

Available Model API Paths
-------------------------

| Method | Path pattern | Description |
| --- | --- | --- |
| `get` | `/` | Return minimal visible predicate maps (same as `/all`), keyed by binary name. |
| `get` | `/all/...` | Return minimal visible predicate maps with binary `id`, `name`, `uri`, `title`, `summary` and `reversed` keys. |
| `get` | `/is_used/+pred/...` | Return whether predicate `+pred` is referenced by at least one row in `edge`. |
| `get` | `/object_resources/+key/...` | Return visible/published resources allowed as objects for predicate `+key`, bucketed by matching valid object category and title-sorted. |
| `get` | `/object_category/+key/...` | Return list of valid object category ids for predicate `+key` (empty means unrestricted). |
| `get` | `/subject_category/+key/...` | Return list of valid subject category ids for predicate `+key` (empty means unrestricted). |
| `get` | `/is_valid_object_in_category/+predicate/+category/...` | Return whether category `+category` is valid for objects of `+predicate`, including inverse ancestor matching fallback. |
| `get` | `/is_valid_object_subcategory/+predicate/+category/...` | Return whether `+category` is valid object category for `+predicate`, allowing ancestor matches (`IsSubcats=true`). |
| `get` | `/is_valid_object_category/+predicate/+category/...` | Return whether `+category` is directly valid object category for `+predicate` (`IsSubcats=false`). |
| `get` | `/is_valid_subject_subcategory/+predicate/+category/...` | Return whether `+category` is valid subject category for `+predicate`, including subject ancestor/subcategory matching (`IsSubcats=true`). |
| `get` | `/is_valid_subject_category/+predicate/+category/...` | Return whether `+category` is directly valid subject category for `+predicate` (`IsSubcats=false`). |
| `get` | `/predicate/+key/...` | Return predicate definition for `+key` (id/name), including identity and ACL-checked display fields; `undefined` if unknown. |
| `get` | `/+key/...` | Alias for `/predicate/+key/...`; return predicate definition for `+key` or `undefined`. |

`/+name` marks a variable path segment. A trailing `/...` means extra path segments are accepted for further lookups.
").
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3,

    object_resources/2,
    is_predicate/2,
    is_used/2,
    id_to_name/2,
    name_to_id/2,
    objects/2,
    subjects/2,
    all/1,
    dependency_ignored/1,
    get/2,
    insert/2,
    flush/1,
    update_noflush/4,
    object_category/2,
    subject_category/2,
    for_subject/2,

    update_predicate_category/4
]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([], Msg, Context) -> m_get_visible([], Msg, Context);
m_get([<<"all">> | _] = Path, Msg, Context) -> m_get_visible(Path, Msg, Context);
m_get(Path, Msg, Context) ->
    case model_predicate(Path) of
        undefined -> {error, unknown_path};
        Pred ->
            case visible_predicate(m_rsc:rid(Pred, Context), Context) of
                true -> model_get(Path, Msg, Context);
                false -> {error, eacces}
            end
    end.

model_predicate([Route, Pred | _])
    when
        Route =:= <<"predicate">>;
        Route =:= <<"is_used">>;
        Route =:= <<"object_resources">>;
        Route =:= <<"object_category">>;
        Route =:= <<"subject_category">>;
        Route =:= <<"is_valid_object_in_category">>;
        Route =:= <<"is_valid_object_subcategory">>;
        Route =:= <<"is_valid_object_category">>;
        Route =:= <<"is_valid_subject_subcategory">>;
        Route =:= <<"is_valid_subject_category">> -> Pred;
model_predicate([Pred | _]) -> Pred;
model_predicate(_) -> undefined.

%% Usage counts reveal private connections; reserve this route for predicate editors.
model_get([<<"is_used">>, Pred | _] = Path, Msg, Context) ->
    case z_acl:rsc_editable(m_rsc:rid(Pred, Context), Context) of
        true -> m_get_visible(Path, Msg, Context);
        false -> {error, eacces}
    end;
model_get([Route, _Pred, Category | _] = Path, Msg, Context)
    when
        Route =:= <<"is_valid_object_in_category">>;
        Route =:= <<"is_valid_object_subcategory">>;
        Route =:= <<"is_valid_object_category">>;
        Route =:= <<"is_valid_subject_subcategory">>;
        Route =:= <<"is_valid_subject_category">> ->
    CatId = m_rsc:rid(Category, Context),
    case is_integer(CatId) andalso m_rsc:is_a(CatId, category, Context)
        andalso z_acl:rsc_visible(CatId, Context) of
        true -> m_get_visible(Path, Msg, Context);
        false -> {error, eacces}
    end;
model_get(Path, Msg, Context) -> m_get_visible(Path, Msg, Context).

%% Do not return a partial restriction list: [] means unrestricted.
visible_categories(Ids, Rest, Context) ->
    case lists:all(fun(Id) -> z_acl:rsc_visible(Id, Context) end, Ids) of
        true -> {ok, {Ids, Rest}};
        false -> {error, eacces}
    end.

m_get_visible([], _Msg, Context) ->
    {ok, {all(Context), []}};
m_get_visible([ <<"all">> | Rest ], _Msg, Context) ->
    {ok, {all(Context), Rest}};
m_get_visible([ <<"is_used">>, Pred | Rest ], _Msg, Context) ->
    {ok, {is_used(Pred, Context), Rest}};
m_get_visible([ <<"object_resources">>, Key | Rest ], _Msg, Context) ->
    case object_resources(Key, Context) of
        {ok, List} -> {ok, {List, Rest}};
        {error, _} = Error -> Error
    end;
m_get_visible([ <<"object_category">>, Key | Rest ], _Msg, Context) ->
    visible_categories(object_category(Key, Context), Rest, Context);
m_get_visible([ <<"subject_category">>, Key | Rest ], _Msg, Context) ->
    visible_categories(subject_category(Key, Context), Rest, Context);
m_get_visible([ <<"is_valid_object_in_category">>, Predicate, Category | Rest ], _Msg, Context) ->
    IsValid = is_valid_object_in_category(Predicate, Category, Context),
    {ok, {IsValid, Rest}};
m_get_visible([ <<"is_valid_object_subcategory">>, Predicate, Category | Rest ], _Msg, Context) ->
    IsValid = is_valid_object_category(Predicate, Category, true, Context),
    {ok, {IsValid, Rest}};
m_get_visible([ <<"is_valid_object_category">>, Predicate, Category | Rest ], _Msg, Context) ->
    IsValid = is_valid_object_category(Predicate, Category, false, Context),
    {ok, {IsValid, Rest}};
m_get_visible([ <<"is_valid_subject_subcategory">>, Predicate, Category | Rest ], _Msg, Context) ->
    IsValid = is_valid_subject_category(Predicate, Category, true, Context),
    {ok, {IsValid, Rest}};
m_get_visible([ <<"is_valid_subject_category">>, Predicate, Category | Rest ], _Msg, Context) ->
    IsValid = is_valid_subject_category(Predicate, Category, false, Context),
    {ok, {IsValid, Rest}};
m_get_visible([ <<"predicate">>, Key | Rest ], _Msg, Context) ->
    {ok, {get(Key, Context), Rest}};
m_get_visible([ Key | Rest ], _Msg, Context) ->
    {ok, {get(Key, Context), Rest}};
m_get_visible(_Vs, _Msg, _Context) ->
    {error, unknown_path}.


%% @doc Fetch a list of all resources matching valid as an object for the given predicates.
%% The list is bucket-sorted by valid category. All buckets are sorted by title.
-spec object_resources(Predicate, Context) -> {ok, Result} | {error, unknown_predicate | eacces} when
    Predicate :: m_rsc:resource(),
    Context :: z:context(),
    Result :: [ #{ binary() => term() } ].
object_resources(Predicate, Context) ->
    case visible_predicate(m_rsc:rid(Predicate, Context), Context) of
        true ->
            Cats = object_category(Predicate, Context),
            case visible_categories(Cats, [], Context) of
                {ok, _} -> object_resources_1(Cats, Context);
                {error, _} = Error -> Error
            end;
        false ->
            {error, unknown_predicate}
    end.

object_resources_1([], _Context) ->
    {ok, []};
object_resources_1(ValidCats, Context) ->
    Query = #{
        cat => ValidCats,
        is_published => true
    },
    #search_result{ result = Ids } = z_search:search(<<"query">>, Query, 1, 10000, Context),
    Ids1 = lists:filter(fun(Id) -> z_acl:rsc_visible(Id, Context) end, Ids),
    % Place the list of object categories in the order of the category tree.
    AllCats = m_category:tree_flat_meta(Context),
    ObjCats = lists:filtermap(
        fun(Cat) ->
            CatId = proplists:get_value(id, Cat),
            case lists:member(CatId, ValidCats) of
                true ->
                    Name = m_rsc:p(CatId, <<"name">>, Context),
                    {true, Name};
                false ->
                    false
            end
        end,
        AllCats),
    ObjCatsRev = lists:reverse(ObjCats),
    Buckets = lists:foldl(
        fun(Id, Acc) ->
            IsA = [atom_to_binary(Cat, utf8) || Cat <- lists:reverse(m_rsc:is_a(Id, Context))],
            case first_match(IsA, ObjCatsRev) of
                undefined ->
                    Acc;
                IdCat ->
                    Acc#{
                        IdCat => [ Id | maps:get(IdCat, Acc, []) ]
                    }
            end
        end,
        #{},
        Ids1),
    ByCat = lists:filtermap(
        fun(CatId) ->
            case maps:get(CatId, Buckets, []) of
                [] ->
                    false;
                OIds ->
                    OIds1 = filter_sort:sort(OIds, <<"title">>, Context),
                    {true, #{
                        <<"category">> => CatId,
                        <<"resources">> => OIds1
                    }}
            end
        end,
        ObjCats),
    {ok, ByCat}.

first_match([], _ObjCats) ->
    undefined;
first_match([Cat|IsA], ObjCats) ->
    case lists:member(Cat, ObjCats) of
        true -> Cat;
        false -> first_match(IsA, ObjCats)
    end.



%% @doc Test if the resource id is a predicate.
-spec is_predicate(Id, Context) -> boolean() when
    Id :: m_rsc:resource(),
    Context :: z:context().
is_predicate(Id, Context) when is_integer(Id) ->
    m_rsc:is_a(Id, predicate, Context);
is_predicate(Pred, Context) ->
    case m_rsc:name_to_id(Pred, Context) of
        {ok, Id} -> is_predicate(Id, Context);
        _ -> false
    end.

%% @doc Check if a predicate is actually in use for an existing edge.
-spec is_used(Predicate, Context) -> boolean() when
    Predicate :: m_rsc:resource(),
    Context :: z:context().
is_used(Predicate, Context) ->
    case m_rsc:rid(Predicate, Context) of
        undefined ->
            false;
        Id ->
            is_integer(z_db:q1("select id from edge where predicate_id = $1 limit 1", [Id], Context))
    end.

is_valid_object_in_category(Predicate, Category, Context) ->
    case is_valid_object_category(Predicate, Category, true, Context) of
        true ->
            true;
        false ->
            CatId = m_rsc:rid(Category, Context),
            ValidCats = object_category(Predicate, Context),
            lists:any(
                fun(ValidCat) ->
                    IsACats = m_category:is_a(ValidCat, Context),
                    IsACatIds = [ m_rsc:rid(Cat, Context) || Cat <- IsACats ],
                    lists:member(CatId, IsACatIds)
                end,
                ValidCats)
    end.

is_valid_object_category(Predicate, Category, IsSubcats, Context) ->
    CatId = m_rsc:rid(Category, Context),
    ValidCats = object_category(Predicate, Context),
    case lists:member(CatId, ValidCats) of
        true ->
            true;
        false when ValidCats =:= [] ->
            true;
        false when IsSubcats ->
            IsA = m_category:is_a(CatId, Context),
            lists:any(
                fun(IsACat) ->
                    IsACatId = m_rsc:rid(IsACat, Context),
                    lists:member(IsACatId, ValidCats)
                end,
                IsA);
        false ->
            false
    end.

is_valid_subject_category(Predicate, Category, IsSubcats, Context) ->
    CatId = m_rsc:rid(Category, Context),
    ValidCats = subject_category(Predicate, Context),
    case lists:member(CatId, ValidCats) of
        true ->
            true;
        false when ValidCats =:= [] ->
            true;
        false ->
            IsA = m_category:is_a(CatId, Context),
            case lists:any(
                fun(IsACat) ->
                    IsACatId = m_rsc:rid(IsACat, Context),
                    lists:member(IsACatId, ValidCats)
                end,
                IsA)
            of
                true ->
                    true;
                false when IsSubcats ->
                    % Check subcategories
                    SubCats = m_category:tree_flat(CatId, Context),
                    SubCatIds = lists:map(
                        fun(C) -> proplists:get_value(id, C) end,
                        SubCats),
                    lists:any(
                        fun(CId) -> lists:member(CId, ValidCats) end,
                        SubCatIds);
                false ->
                    false
            end
    end.

%% @doc Lookup the name of a predicate with an id
-spec id_to_name(m_rsc:resource_id(), z:context()) -> {ok, atom()} | {error, {unknown_predicate, term()}}.
id_to_name(Id, Context) when is_integer(Id) ->
    F = fun() ->
                {L, R} = cat_bounds(Context),
                case z_db:q1("
                            select r.name
                            from rsc r
                                join hierarchy c
                                on r.category_id = c.id and c.name = '$category'
                            where r.id = $1
                              and $2 <= c.nr
                              and c.nr <= $3", [Id, L, R], Context) of
                    undefined -> {error, {unknown_predicate, Id}};
                    Name -> {ok, z_convert:to_atom(Name)}
                end
        end,
    z_depcache:memo(F, {predicate_name, Id}, ?DAY, [predicate], Context).

%% @doc Return the id of the predicate
-spec name_to_id( m_rsc:resource_name(), z:context() ) -> {ok, m_rsc:resource_id()} | {error, {unknown_predicate, term()}}.
name_to_id(Name, Context) ->
    case m_rsc:name_to_id(Name, Context) of
        {ok, Id} ->
            case is_predicate(Id, Context) of
                true -> {ok, Id};
                false -> {error, {unkown_predicate, Id}}
            end;
        _ -> {error, {unknown_predicate, Name}}
    end.

%% @doc Minimal visible definition with explicitly allowed, ACL-checked properties.
-spec get(Predicate, Context) -> map() | undefined when
    Predicate :: atom() | m_rsc:resource_id() | string() | binary(),
    Context :: z:context().
get(Predicate, Context) ->
    Id = case Predicate of
        P when is_binary(P); is_list(P) -> m_rsc:rid(z_string:to_lower(P), Context);
        P -> m_rsc:rid(P, Context)
    end,
    case visible_predicate(Id, Context) of
        true -> definition(maps:get(Id, catalogue(Context), undefined), Context);
        false -> undefined
    end.

%% @doc Return the category ids that are valid as objects of the predicate.
-spec objects(PredicateId, Context) -> [ CategoryId ] when
    PredicateId :: m_rsc:resource_id(),
    Context :: z:context(),
    CategoryId :: m_rsc:resource_id().
objects(PredicateId, Context) ->
    Objects = z_db:q("
        select category_id
        from predicate_category
        where predicate_id = $1
          and is_subject = false",
        [PredicateId], Context),
    [R || {R} <- Objects].

%% @doc Return the category ids that are valid as subjects of the predicate.
-spec subjects(PredicateId, Context) -> [ CategoryId ] when
    PredicateId :: m_rsc:resource_id(),
    Context :: z:context(),
    CategoryId :: m_rsc:resource_id().
subjects(PredicateId, Context) ->
    Subjects = z_db:q("
        select category_id
        from predicate_category
        where predicate_id = $1
          and is_subject = true",
        [PredicateId], Context),
    [R || {R} <- Subjects].

%% @doc Visible predicate definitions keyed by binary name.
-spec all(Context) -> map() when Context :: z:context().
all(Context) ->
    maps:from_list([
        {Name, definition(P, Context)}
        || {Id, #{<<"name">> := Name} = P} <- maps:to_list(catalogue(Context)),
           is_binary(Name), z_acl:rsc_visible(Id, Context)
    ]).

%% Display values are evaluated outside the shared cache: property visibility and
%% resource observers can depend on the caller's context.
definition(undefined, _Context) -> undefined;
definition(#{<<"id">> := Id} = Identity, Context) ->
    maps:merge(
        Identity,
        maps:from_list([
            {Key, m_rsc:p(Id, Key, Context)}
            || Key <- [<<"uri">>, <<"title">>, <<"summary">>, <<"reversed">>, <<"is_published">>]
        ])
    ).

%% Shared site cache contains structural identity only, never user ACL decisions.
catalogue(Context) ->
    F = fun() ->
        {L, R} = cat_bounds(Context),
        Rows = z_db:q("
            select r.id, r.name
            from rsc r
            join hierarchy c
                on r.category_id = c.id
                and c.name = '$category'
            where $1 <= c.nr
              and c.nr <= $2
            order by r.name", [L, R], Context),
        maps:from_list([{Id, #{<<"id">> => Id, <<"name">> => Name}} || {Id, Name} <- Rows])
    end,
    z_depcache:memo(F, predicate_catalogue, ?DAY, [predicate, category], Context).

visible_predicate(Id, Context) ->
    is_integer(Id) andalso is_predicate(Id, Context) andalso z_acl:rsc_visible(Id, Context).

%% @doc Internal policy lookup, intentionally independent of public visibility.
-spec dependency_ignored(Context) -> [m_rsc:resource_id()] when Context :: z:context().
dependency_ignored(Context) ->
    F = fun() ->
        [Id || Id <- maps:keys(catalogue(Context)),
            z_convert:to_bool(m_rsc:p_no_acl(Id, <<"is_dependency_ignored">>, Context))]
    end,
    z_depcache:memo(F, predicate_dependency_ignored, ?DAY, [predicate, category], Context).

%% @doc Insert a new predicate, sets some defaults.
-spec insert(Title, Context) -> {ok, m_rsc:resource_id()} | {error, term()} when
    Title :: binary() | string(),
    Context :: z:context().
insert(Title, Context) ->
    Title1 = z_string:trim(Title),
    case z_string:to_name(Title1) of
        <<>> ->
            {error, invalid_title};
        <<"_">> ->
            {error, invalid_title};
        Name ->
            Uri  = iolist_to_binary(["http://zotonic.net/predicate/", Name]),
            Props = #{
                <<"title">> => Title1,
                <<"name">> => Name,
                <<"uri">> => Uri,
                <<"category">> => predicate,
                <<"is_published">> => true
            },
            case m_rsc:insert(Props, Context) of
                {ok, PredicateId} ->
                    flush(Context),
                    {ok, PredicateId};
                {error, _Reason} = Error ->
                    Error
            end
    end.


%% @doc Flush all cached data about predicates.
flush(Context) ->
    z_depcache:flush(predicate, Context).


%% @doc Reset the list of valid subjects and objects. Do not flush the caches.
%% This is part of the resource updates, when saving a resource. The caches
%% should be flushed afterwards.
-spec update_noflush(PredicateId, Subjects, Objects, Context) -> ok when
    PredicateId :: m_rsc:resource_id(),
    Subjects :: [ m_rsc:resource() ],
    Objects :: [ m_rsc:resource() ],
    Context :: z:context().
update_noflush(PredicateId, Subjects, Objects, Context) ->
    ok = z_db:transaction(
        fun(Ctx) ->
            update_predicate_category(PredicateId, true, Subjects, Ctx),
            update_predicate_category(PredicateId, false, Objects, Ctx),
            ok
        end,
        Context).

filter_cat(Cat, Context) ->
    case m_rsc:rid(Cat, Context) of
        undefined -> false;
        CatId ->
            case m_rsc:is_a(CatId, category, Context) of
                true -> {true, CatId};
                false -> false
            end
    end.

%% @doc Update the list of valid subject or object categories for a predicate.
-spec update_predicate_category(PredicateId, IsSubject, NewCats, Context) -> ok when
    PredicateId :: m_rsc:resource_id(),
    IsSubject :: boolean(),
    NewCats :: [ m_rsc:resource() ],
    Context :: z:context().
update_predicate_category(PredicateId, IsSubject, NewCats, Context) ->
    NewCatIds0 = lists:filtermap(fun(Cat) -> filter_cat(Cat, Context) end, NewCats),
    NewCatIds = lists:usort(NewCatIds0),
    CurrentIdsR = z_db:q("
        select category_id
        from predicate_category
        where predicate_id = $1
          and is_subject = $2",
        [PredicateId, IsSubject],
        Context),
    CurrentIds = [ N || {N} <- CurrentIdsR ],
    % Delete categories that are no longer valid.
    case CurrentIds -- NewCatIds of
        [] -> ok;
        DelIds ->
            z_db:q("
                delete from predicate_category
                where predicate_id = $1
                  and is_subject = $2
                  and category_id = ANY($3)",
                [PredicateId, IsSubject, DelIds],
                Context)
    end,
    % Insert new categories that are not yet in the list.
    lists:foreach(
        fun(NewCatId) ->
            z_db:q("
                insert into predicate_category (predicate_id, is_subject, category_id)
                values ($1, $2, $3)
                on conflict (predicate_id, is_subject, category_id)
                do nothing",
                [PredicateId, IsSubject, NewCatId],
                Context)
        end,
        NewCatIds -- CurrentIds).


%% @doc Return all the valid categories for objects.
%% Return the empty list when there is no constraint.
-spec object_category(Id, Context) -> Categories when
    Id :: m_rsc:resource(),
    Context :: z:context(),
    Categories :: [ m_rsc:resource_id() ].
object_category(Id, Context) ->
    F = fun() ->
        case name_to_id(Id, Context) of
            {ok, PredId} ->
                Ids = z_db:q(
                    "select category_id from predicate_category where predicate_id = $1 and "
                        ++ "is_subject = false",
                    [PredId], Context),
                [ CId || {CId} <- Ids ];
            _ ->
                []
        end
        end,
    z_depcache:memo(F, {object_category, Id}, ?WEEK, [predicate], Context).

%% @doc Return all the valid categories for subjects.
%% Return the empty list when there is no constraint.
-spec subject_category(Id, Context) -> Categories when
    Id :: m_rsc:resource(),
    Context :: z:context(),
    Categories :: [ m_rsc:resource_id() ].
subject_category(Id, Context) ->
    F = fun() ->
        case name_to_id(Id, Context) of
            {ok, PredId} ->
                Ids = z_db:q("select category_id from predicate_category where predicate_id = $1 "
                    "and is_subject = true",
                    [PredId], Context),
                [ CId || {CId} <- Ids ];
            _ ->
                []
        end
        end,
    z_depcache:memo(F, {subject_category, Id}, ?WEEK, [predicate], Context).

%% @doc Return the list of predicates that are valid for the given resource id.
%% Append all predicates that have no restrictions.
-spec for_subject(Id, Context) -> Predicates when
    Id :: m_rsc:resource_id(),
    Context :: z:context(),
    Predicates :: [ m_rsc:resource_id() ].
for_subject(Id, Context) ->
    F = fun() ->
        {L, R} = cat_bounds(Context),
        ValidIds = z_db:q("
                    select p.predicate_id
                    from predicate_category p,
                         hierarchy pc,
                         rsc r,
                         hierarchy rc
                    where p.category_id = pc.id
                      and pc.name = '$category'
                      and r.category_id = rc.id
                      and rc.name = '$category'
                      and rc.nr >= pc.lft
                      and rc.nr <= pc.rght
                      and r.id = $1
                      and is_subject = true
                    ", [Id], Context),
        Valid = [ValidId || {ValidId} <- ValidIds],
        NoRestrictionIds = z_db:q("
                        select r.id
                        from rsc r left join predicate_category p on p.predicate_id = r.id and p.is_subject = true
                            join hierarchy c on (r.category_id = c.id and c.name = '$category')
                        where p.predicate_id is null
                          and $1 <= c.nr and c.nr <= $2
                    ", [L, R], Context),
        NoRestriction = [NoRestrictionId || {NoRestrictionId} <- NoRestrictionIds],
        Valid ++ NoRestriction
    end,
    z_depcache:memo(F, {predicate_for_subject, Id}, ?WEEK, [predicate, category, Id], Context).

%% @doc Return the id of the predicate category
-spec cat_id(#context{}) -> m_rsc:resource_id().
cat_id(Context) ->
    {ok, Id} = m_category:name_to_id(predicate, Context),
    Id.

%% @doc Return the left and right bounds of the predicate category in the category tree.
%% This is used to filter predicates in the database queries.
-spec cat_bounds(#context{}) -> {integer(), integer()}.
cat_bounds(Context) ->
    m_category:get_range(cat_id(Context), Context).
