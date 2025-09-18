%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2023 Marc Worrell
%% @doc Model for predicates
%% @end

%% Copyright 2009-2023 Marc Worrell
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
-moduledoc("
Retrieve information about predicates. Predicates are the labels on edges (connections between resources) that give
meaning to an edge. An example is the predicate “author” which refers to the authors of an article. Predicates form
together with the referring and the referred page a triple `{subject, predicate, object}`.

Each predicate has a list of valid subject categories and valid object categories. This is used to filter the list of
predicates in the admin edit page, and also to filter the list of found potential objects when making a connection.

A full predicate definition can be fetched by name or id with:


```erlang
{{ m.predicate.author }}
{{ m.predicate[104] }}
```

Which both return a property list with information about the predicate. The property list contains all page properties
and the properties: “pred” which is the atomic predicate name, “subject” which is a list of valid subject
categories and “object” with is a list of valid object categories.

The following m\\_predicate model properties are available in templates:

| Property           | Description                                                                      | Example value                                                         |
| ------------------ | -------------------------------------------------------------------------------- | --------------------------------------------------------------------- |
| all                | Return a property list of all predicates. Keys are the atomic predicate name, values are property lists with information about the predicate. The property list contains all page properties and the properties: “pred” which is the atomic predicate name, “subject” which is a list of valid subject categories and “object” with is a list of valid object categories. | `[{about, [{pred,,about},{subject,[104]}, {object,[]}, {id,300}, … ]` |
| object\\\\_category  | Used to derive the list of valid object categories for a predicate. Example usage: `m.predicate.object_category.author` Note: Each id is a 1-tuple. | `[{104}, … ]`                                                         |
| subject\\\\_category | Used to derive the list of valid subject categories for a predicate. Example usage: `m.predicate.subject_category.author` Note: Each id is a 1-tuple. | `[{674}, … ]`                                                         |
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
    get/2,
    insert/2,
    flush/1,
    update_noflush/4,
    object_category/2,
    subject_category/2,
    for_subject/2
]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([], _Msg, Context) ->
    {ok, {all(Context), []}};
m_get([ <<"all">> | Rest ], _Msg, Context) ->
    {ok, {all(Context), Rest}};
m_get([ <<"is_used">>, Pred | Rest ], _Msg, Context) ->
    {ok, {is_used(Pred, Context), Rest}};
m_get([ <<"object_resources">>, Key | Rest ], _Msg, Context) ->
    case object_resources(Key, Context) of
        {ok, List} -> {ok, {List, Rest}};
        {error, _} = Error -> Error
    end;
m_get([ <<"object_category">>, Key | Rest ], _Msg, Context) ->
    {ok, {object_category(Key, Context), Rest}};
m_get([ <<"subject_category">>, Key | Rest ], _Msg, Context) ->
    {ok, {subject_category(Key, Context), Rest}};
m_get([ <<"is_valid_object_in_category">>, Predicate, Category | Rest ], _Msg, Context) ->
    IsValid = is_valid_object_in_category(Predicate, Category, Context),
    {ok, {IsValid, Rest}};
m_get([ <<"is_valid_object_subcategory">>, Predicate, Category | Rest ], _Msg, Context) ->
    IsValid = is_valid_object_category(Predicate, Category, true, Context),
    {ok, {IsValid, Rest}};
m_get([ <<"is_valid_object_category">>, Predicate, Category | Rest ], _Msg, Context) ->
    IsValid = is_valid_object_category(Predicate, Category, false, Context),
    {ok, {IsValid, Rest}};
m_get([ <<"is_valid_subject_subcategory">>, Predicate, Category | Rest ], _Msg, Context) ->
    IsValid = is_valid_subject_category(Predicate, Category, true, Context),
    {ok, {IsValid, Rest}};
m_get([ <<"is_valid_subject_category">>, Predicate, Category | Rest ], _Msg, Context) ->
    IsValid = is_valid_subject_category(Predicate, Category, false, Context),
    {ok, {IsValid, Rest}};
m_get([ <<"predicate">>, Key | Rest ], _Msg, Context) ->
    {ok, {get(Key, Context), Rest}};
m_get([ Key | Rest ], _Msg, Context) ->
    {ok, {get(Key, Context), Rest}};
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.


%% @doc Fetch a list of all resources matching valid as an object for the given predicates.
%% The list is bucket-sorted by valid category. All buckets are sorted by title.
-spec object_resources(Predicate, Context) -> {ok, Result} | {error, unknown_predicate} when
    Predicate :: m_rsc:resource(),
    Context :: z:context(),
    Result :: [ #{ category := atom(), resources := list( m_rsc:resource_id() ) } ].
object_resources(Predicate, Context) ->
    case is_predicate(Predicate, Context) of
        true ->
            object_resources_1(object_category(Predicate, Context), Context);
        false ->
            {error, unknown_predicate}
    end.

object_resources_1([], _Context) ->
    {ok, #{}};
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
                    Name = z_convert:to_atom( m_rsc:p(CatId, <<"name">>, Context) ),
                    {true, Name};
                false ->
                    false
            end
        end,
        AllCats),
    ObjCatsRev = lists:reverse(ObjCats),
    Buckets = lists:foldl(
        fun(Id, Acc) ->
            IsA = lists:reverse(m_rsc:is_a(Id, Context)),
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
                        category => CatId,
                        resources => OIds1
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

%% @doc Return the definition of the predicate
-spec get( atom() | m_rsc:resource_id() | string() | binary(), z:context() ) -> list() | undefined.
get(PredId, Context) when is_integer(PredId) ->
    case id_to_name(PredId, Context) of
        {error, _} -> undefined;
        {ok, Name} -> get(Name, Context)
    end;
get(Pred, Context) when is_list(Pred) orelse is_binary(Pred) ->
    Pred1 = z_string:to_lower(Pred),
    try
        PredAsAtom = erlang:binary_to_existing_atom(Pred1, utf8),
        get(PredAsAtom, Context)
    catch
        error:badarg ->
            % The predicate list might not have been read yet - then not all atoms are known.
            % Reaad all predicates and try again.
            % TODO: replace all keys with binaries (and a map)
            All = all(Context),
            try
                PredAsAtom1 = erlang:binary_to_existing_atom(Pred1, utf8),
                proplists:get_value(PredAsAtom1, All)
            catch
                error:badarg ->
                    undefined
            end
    end;
get(Pred, Context) ->
    case z_depcache:get(predicate, Pred, Context) of
        {ok, undefined} ->
            undefined;
        {ok, Value} ->
            Value;
        undefined ->
            proplists:get_value(Pred, all(Context))
    end.

%% @doc Return the category ids that are valid as objects
objects(Id, Context) ->
    Objects = z_db:q("
        select category_id
        from predicate_category
        where predicate_id = $1
          and is_subject = false",
        [Id], Context),
    [R || {R} <- Objects].

%% @doc Return the category ids that are valid as subjects
subjects(Id, Context) ->
    Subjects = z_db:q("
        select category_id
        from predicate_category
        where predicate_id = $1
          and is_subject = true",
        [Id], Context),
    [R || {R} <- Subjects].

%% @doc Return the list of all predicates
%% @spec all(Context) -> PropList
all(Context) ->
    F = fun() ->
        {L, R} = cat_bounds(Context),
        Preds = z_db:assoc_props("
                                select *
                                from rsc r
                                    join hierarchy c
                                    on r.category_id = c.id and c.name = '$category'
                                where $1 <= c.nr
                                  and c.nr <= $2
                                order by r.name", [L, R], Context),
        FSetPred = fun(Pred) ->
            Id = proplists:get_value(id, Pred),
            Atom = case proplists:get_value(name, Pred) of
                       undefined -> undefined;
                       B -> list_to_atom(binary_to_list(B))
                   end,
            {Atom, [{pred, Atom}, {subject, subjects(Id, Context)}, {object, objects(Id, Context)} | Pred]}
                   end,
        [FSetPred(Pred) || Pred <- Preds]
    end,
    z_depcache:memo(F, predicate, ?DAY, Context).

%% @doc Insert a new predicate, sets some defaults.
-spec insert(binary()|list(), z:context()) -> {ok, integer()} | {error, any()}.
insert(Title, Context) ->
    Name = z_string:to_name(Title),
    Uri  = "http://zotonic.net/predicate/" ++ Name,
    Props = #{
        <<"title">> => Title,
        <<"name">> => Name,
        <<"uri">> => Uri,
        <<"category">> => predicate,
        <<"group">> => admins,
        <<"is_published">> => true
    },
    case m_rsc:insert(Props, Context) of
        {ok, Id} ->
            flush(Context),
            {ok, Id};
        {error, Reason} ->
            {error, Reason}
    end.


%% @doc Flush all cached data about predicates.
flush(Context) ->
    z_depcache:flush(predicate, Context).


%% @doc Reset the list of valid subjects and objects.
-spec update_noflush(integer(), list(), list(), z:context()) -> ok.
update_noflush(Id, Subjects, Objects, Context) ->
    SubjectIds0 = [m_rsc:rid(N, Context) || N <- Subjects, N /= [], N /= <<>>],
    ObjectIds0 = [m_rsc:rid(N, Context) || N <- Objects, N /= [], N /= <<>>],
    SubjectIds = [N || N <- SubjectIds0, N =/= undefined],
    ObjectIds = [ N || N <- ObjectIds0, N =/= undefined ],
    ok = z_db:transaction(
        fun(Ctx) ->
            update_predicate_category(Id, true, SubjectIds, Ctx),
            update_predicate_category(Id, false, ObjectIds, Ctx),
            ok
        end,
        Context).

update_predicate_category(Id, IsSubject, CatIds, Context) ->
    OldIdsR = z_db:q("select category_id from predicate_category where predicate_id = $1 and is_subject = $2",
        [Id, IsSubject], Context),
    OldIds = [N || {N} <- OldIdsR],
    % Delete the ones that are not there anymore
    [z_db:q("delete from predicate_category where predicate_id = $1 and category_id = $2 and is_subject = $3",
        [Id, OldId, IsSubject], Context)
        || OldId <- OldIds, not lists:member(OldId, CatIds)
    ],
    [z_db:insert(predicate_category, [{predicate_id, Id}, {category_id, NewId}, {is_subject, IsSubject}],
        Context)
        || NewId <- CatIds, not lists:member(NewId, OldIds)
    ],
    ok.


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
-spec cat_id(#context{}) -> integer().
cat_id(Context) ->
    {ok, Id} = m_category:name_to_id(predicate, Context),
    Id.

-spec cat_bounds(#context{}) -> {integer(), integer()}.
cat_bounds(Context) -> m_category:get_range(cat_id(Context), Context).
