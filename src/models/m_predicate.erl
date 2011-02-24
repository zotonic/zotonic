%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-09
%%
%% @doc Model for predicates

%% Copyright 2009 Marc Worrell
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
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    
    is_predicate/2,
    id_to_name/2,
    name_to_id/2,
    name_to_id_check/2,
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
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(all, #m{value=undefined}, Context) ->
    all(Context);
m_find_value(object_category, #m{value=undefined} = M, _Context) ->
    M#m{value=object_category};
m_find_value(subject_category, #m{value=undefined} = M, _Context) ->
    M#m{value=subject_category};
m_find_value(Key, #m{value=object_category}, Context) ->
    object_category(Key, Context);
m_find_value(Key, #m{value=subject_category}, Context) ->
    subject_category(Key, Context);
m_find_value(Key, #m{value=undefined}, Context) ->
    get(Key, Context).

%% @doc Transform a model value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> List
m_to_list(#m{value=undefined}, Context) ->
    all(Context);
m_to_list(#m{}, _Context) ->
    [].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{}, Context) ->
    all(Context).


%% @doc Test if the property is the name of a predicate
%% @spec is_predicate(Pred, Context) -> bool()
is_predicate(Id, Context) when is_integer(Id) -> 
    case m_rsc:p(Id, category_id, Context) of
        undefined -> false;
        CatId ->
            m_category:is_a(CatId, predicate, Context)
    end;

is_predicate(Pred, Context) ->
    case m_rsc:name_to_id(Pred, Context) of
        {ok, Id} ->
            is_predicate(Id, Context);
        _ -> false
    end.


%% @doc Lookup the name of a predicate with an id
%% @spec id_to_name(Id, Context) -> {ok, atom()} | {error, Reason}
id_to_name(Id, Context) when is_integer(Id) ->
    F = fun() ->
                {L,R} = cat_bounds(Context),
                case z_db:q1("select name from rsc r join category c on (r.category_id = c.id) where r.id = $1 and $2 <= c.nr and c.nr <= $3", [Id, L, R], Context) of
                    undefined -> {error, {unknown_predicate, Id}};
                    Name -> {ok, z_convert:to_atom(Name)}
                end
        end,
    z_depcache:memo(F, {predicate_name, Id}, ?DAY, [predicate], Context).

    
%% @doc Return the id of the predicate
%% @spec name_to_id(Pred, Context) -> {ok, int()} | {error, Reason}
name_to_id(Name, Context) ->
    case m_rsc:name_to_id(Name, Context) of
        {ok, Id} ->
            case is_predicate(Id, Context) of
                true -> {ok, Id};
                false -> {error, {unkown_predicate, Id}}
            end;
        _ -> {error, {unknown_predicate, Name}}
    end.

name_to_id_check(Name, Context) ->
    {ok, Id} = name_to_id(Name, Context),
    Id.


%% @doc Return the definition of the predicate
%% @spec get(PredId, Context) -> PredicatePropList | undefined
get(PredId, Context) when is_integer(PredId) ->
    case id_to_name(PredId, Context) of
        {error, _} -> undefined;
		{ok, Name} -> get(Name, Context)
    end;
get(Pred, Context) when is_list(Pred) orelse is_binary(Pred) ->
    get(list_to_atom(z_string:to_lower(Pred)), Context);
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
    Objects = z_db:q("select category_id from predicate_category where predicate_id = $1 and is_subject = false", [Id], Context),
    [ R || {R} <- Objects  ].

%% @doc Return the category ids that are valid as subjects
subjects(Id, Context) ->
    Subjects = z_db:q("select category_id from predicate_category where predicate_id = $1 and is_subject = true", [Id], Context),
    [ R || {R} <- Subjects  ].


%% @doc Return the list of all predicates
%% @spec all(Context) -> PropList
all(Context) ->
    F = fun() ->
                {L,R} = cat_bounds(Context),
                Preds = z_db:assoc_props("select * from rsc r join category c on (r.category_id = c.id) where $1 <= c.nr and c.nr <= $2 order by name", [L, R], Context),
                FSetPred = fun(Pred) ->
                                   Id = proplists:get_value(id, Pred),
                                   Atom = case proplists:get_value(name, Pred) of
                                              undefined -> undefined;
                                              B -> list_to_atom(binary_to_list(B))
                                          end,
                                   {Atom, [{pred, Atom},{subject,subjects(Id,Context)},{object,objects(Id,Context)}|Pred]}
                           end,
                [ FSetPred(Pred) || Pred <- Preds]
        end,
    z_depcache:memo(F, predicate, ?DAY, Context).


%% @doc Insert a new predicate, sets some defaults.
%% @spec insert(Title, Context) -> {ok, Id} | {error, Reason}
insert(Title, Context) ->
    Name = z_string:to_name(Title),
    Uri  = "http://zotonic.net/predicate/" ++ Name,
    Props = [
        {title, Title},
        {name, Name},
        {uri, Uri},
        {category, predicate},
        {group, admins},
        {is_published, true},
        {visible_for, 0}
    ],
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


%% @doc Update a predicate, save the reversed flag, reset the list of valid subjects and objects.
%% @spec update_noflush(Id, Subjects, Objects, Context) -> void()
update_noflush(Id, Subjects, Objects, Context) ->
    SubjectIds = [ z_convert:to_integer(N) || N <- Subjects, N /= [], N /= <<>> ],
    ObjectIds = [ z_convert:to_integer(N) || N <- Objects, N /= [], N /= <<>> ],
    F = fun(Ctx) ->
        update_predicate_category(Id, true, SubjectIds, Ctx),
        update_predicate_category(Id, false, ObjectIds, Ctx),
        ok
    end,
    ok = z_db:transaction(F, Context).


update_predicate_category(Id, IsSubject, CatIds, Context) ->
    OldIdsR = z_db:q("select category_id from predicate_category where predicate_id = $1 and is_subject = $2", [Id, IsSubject], Context),
    OldIds  = [ N || {N} <- OldIdsR ],
    % Delete the ones that are not there anymore
    [ z_db:q("delete from predicate_category where predicate_id = $1 and category_id = $2 and is_subject = $3", [Id, OldId, IsSubject], Context)
    || OldId <- OldIds, not lists:member(OldId, CatIds)
    ],
    [ z_db:insert(predicate_category, [{predicate_id, Id}, {category_id, NewId}, {is_subject, IsSubject}], Context)
    || NewId <- CatIds, not lists:member(NewId, OldIds)
    ],
    ok.


%% @doc Return all the valid categories for objects.  Return the empty list when there is no constraint.  Note that the resulting array
%% is a bit strangely formatted [{id}, {id2}, ...], this is compatible with the category name lookup and prevents mixups with strings (lists of integers).
%% @spec object_category(Id, Context) -> List
object_category(Id, Context) ->
    F = fun() ->
        case name_to_id(Id, Context) of
            {ok, PredId} ->
                z_db:q("select category_id from predicate_category where predicate_id = $1 and is_subject = false", [PredId], Context);
            _ -> 
                []
        end
    end,
    z_depcache:memo(F, {object_category, Id}, ?WEEK, [predicate], Context).


%% @doc Return all the valid categories for subjects.  Return the empty list when there is no constraint.  Note that the resulting array
%% is a bit strangely formatted [{id}, {id2}, ...], this is compatible with the category name lookup and prevents mixups with strings (lists of integers).
%% @spec subject_category(Id, Context) -> List
subject_category(Id, Context) ->
    F = fun() ->
        case name_to_id(Id, Context) of
            {ok, PredId} ->
                z_db:q("select category_id from predicate_category where predicate_id = $1 and is_subject = true", [PredId], Context);
            _ -> 
                []
        end
    end,
    z_depcache:memo(F, {subject_category, Id}, ?WEEK, [predicate], Context).


%% @doc Return the list of predicates that are valid for the given resource id. Append all predicates that have no restrictions.
for_subject(Id, Context) ->
    {L,R} = cat_bounds(Context),
    ValidIds = z_db:q("
                select p.predicate_id 
                from predicate_category p,
                     category pc,
                     rsc r,
                     category rc
                where p.category_id = pc.id
                  and r.category_id = rc.id
                  and rc.nr >= pc.lft
                  and rc.nr <= pc.rght
                  and r.id = $1 
                  and is_subject = true
                ", [Id], Context),
    Valid = [ ValidId || {ValidId} <- ValidIds ],
    NoRestrictionIds = z_db:q("
                    select r.id
                    from rsc r left join predicate_category p on p.predicate_id = r.id and p.is_subject = true
                        join category c on (r.category_id = c.id)
                    where p.predicate_id is null
                      and $1 <= c.nr and c.nr <= $2
                ", [L, R], Context),
    NoRestriction = [ NoRestrictionId || {NoRestrictionId} <- NoRestrictionIds ],
    Valid ++ NoRestriction.
                
    


%% @doc Return the id of the predicate category
%% @spec cat_id(Context) -> integer()
cat_id(Context) ->
    m_category:name_to_id_check(predicate, Context).

cat_bounds(Context) ->
    m_category:boundaries(cat_id(Context), Context).
        
