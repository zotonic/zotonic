%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Model for resource data. Interfaces between zotonic, templates and the database.

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

-module(m_rsc).
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(gen_model).

-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    
    name_to_id/2,
    name_to_id_check/2,
    name_to_id_cat/3,
    name_to_id_cat_check/3,

    page_path_to_id/2,
    
    get/2,
    get_raw/2,
    get_acl_props/2,
    insert/2,
    delete/2,
    update/3,
    duplicate/3,
    touch/2,
    
	exists/2, 
	
	is_visible/2, is_editable/2, is_ingroup/2, is_me/2, 
	is_cat/3,
	is_a/2,
	is_a/3,
	
	p/3,
	p_no_acl/3,
	
	op/2, o/2, o/3, o/4,
	sp/2, s/2, s/3, s/4,
	media/2,
	page_url/2,
	rid/2
]).

-include_lib("zotonic.hrl").


%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(Id, #m{value=undefined} = M, Context) ->
    case rid(Id, Context) of
        undefined -> undefined;
        RId -> 
			case z_acl:rsc_visible(Id, Context) of
				true -> M#m{value=RId};
				false -> undefined
			end
    end;
m_find_value(is_cat, #m{value=Id} = M, _Context) when is_integer(Id) -> 
    M#m{value={is_cat, Id}};
m_find_value(Key, #m{value={is_cat, Id}}, Context) -> 
    is_cat(Id, Key, Context);
m_find_value(Key, #m{value=Id}, Context) when is_integer(Id) ->
    p_no_acl(Id, Key, Context).

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> List
m_to_list(#m{value=#rsc_list{list=List}}, _Context) ->
    List;
m_to_list(#m{}, _Context) ->
    [].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
    undefined;
m_value(#m{value=V}, _Context) ->
    V.

%% @doc Return the id of the resource with the name
% @spec name_to_id(NameString, Context) -> int() | {error, Reason}
name_to_id(Name, Context) ->
    case is_list(Name) andalso z_utils:only_digits(Name) of
        true ->
            {ok, z_convert:to_integer(Name)};
        false ->
            case rid_name(Name, Context) of
                Id when is_integer(Id) -> {ok, Id};
                _ -> {error, {unknown_rsc, Name}}
            end
    end.

name_to_id_check(Name, Context) ->
    {ok, Id} = name_to_id(Name, Context),
    Id.

name_to_id_cat(Name, Cat, Context) when is_integer(Name) ->
    F = fun() ->
        CatId = m_category:name_to_id_check(Cat, Context),
        case z_db:q1("select id from rsc where id = $1 and category_id = $2", [Name, CatId], Context) of
            undefined -> {error, {unknown_rsc_cat, Name, Cat}};
            Id -> {ok, Id}
        end
    end,
    z_depcache:memo(F, {rsc_name, Name, Cat}, ?DAY, [Cat], Context);
name_to_id_cat(Name, Cat, Context) ->
    F = fun() ->
        CatId = m_category:name_to_id_check(Cat, Context),
        case z_db:q1("select id from rsc where Name = $1 and category_id = $2", [Name, CatId], Context) of
            undefined -> {error, {unknown_rsc_cat, Name, Cat}};
            Id -> {ok, Id}
        end
    end,
    z_depcache:memo(F, {rsc_name, Name, Cat}, ?DAY, [Cat], Context).

name_to_id_cat_check(Name, Cat, Context) ->
    {ok, Id} = name_to_id_cat(Name, Cat, Context),
    Id.

page_path_to_id(Path, Context) ->
    Path1 = [ $/, string:strip(Path, both, $/)],
    case z_db:q1("select id from rsc where page_path = $1", [Path1], Context) of
        undefined -> {error, {unknown_page_path, Path1}};
        Id -> {ok, Id}
    end.


%% @doc Read a whole resource
%% @spec get(Id, Context) -> PropList | undefined
get(Id, Context) ->
    case rid(Id, Context) of
        Rid when is_integer(Rid) ->
            F = fun() ->
                get_raw(Rid, Context)
            end,
            z_depcache:memo(F, Rid, ?WEEK, Context)
    end.

%% @doc Get the resource from the database, do not fetch the pivot fields.
get_raw(Id, Context) when is_integer(Id) ->
    z_db:assoc_props_row("
            select
                id, uri, name, page_path, 
                is_authoritative, is_published, is_featured, is_protected,
                publication_start, publication_end,
                group_id, creator_id, modifier_id, version, category_id,
                visible_for, comment_by, comments, rating, rating_count,
                slug, props, created, modified
            from rsc
            where id = $1", [Id], Context).


%% @doc Get the ACL fields for the resource with the id. The id must be an integer
%% @spec get_acl_fields(Id, #context) -> #acl_props
get_acl_props(Id, Context) when is_integer(Id) ->
    F = fun() ->
        case z_db:q_row("
            select is_published, visible_for, group_id, publication_start, publication_end 
            from rsc 
            where id = $1", [Id], Context) of
    
            {IsPub, Vis, Group, PubS, PubE} ->
                #acl_props{is_published=IsPub, visible_for=Vis, group_id=Group, publication_start=PubS, publication_end=PubE};
            undefined ->
                #acl_props{is_published=false, visible_for=3, group_id=0}
        end
    end,
    z_depcache:memo(F, {rsc_acl_fields, Id}, ?DAY, [Id], Context);

get_acl_props(Name, Context) ->
    get_acl_props(name_to_id_check(Name, Context), Context).


%% @doc Insert a new resource
%% @spec insert(Props, Context) -> {ok, Id} | {error, Reason}
insert(Props, Context) ->
    m_rsc_update:insert(Props, Context).

%% @doc Delete a resource
%% @spec delete(Props, Context) -> void()
delete(Id, Context) when is_integer(Id) ->
    m_rsc_update:delete(Id, Context).


%% @doc Update a resource
%% @spec update(Id, Props, Context) -> {ok, Id} | {error, Reason}
update(Id, Props, Context) when is_integer(Id) ->
    m_rsc_update:update(Id, Props, Context).


%% @doc Duplicate a resource.
%% @spec duplicate(Id, Props, Context) -> {ok, NewId} | {error, Reason}
duplicate(Id, Props, Context) ->
    m_rsc_update:duplicate(Id, Props, Context).


%% @doc "Touch" the rsc, incrementing the version nr and the modification date/ modifier_id. 
%% This should be called as part of another update or transaction and does not resync the caches,
%% and does not check the ACL.  After "touching" the resource will be re-pivoted.
%% @spec touch(Id, Context) -> {ok, Id} | {error, Reason}
touch(Id, Context) when is_integer(Id) ->
    case z_db:q("update rsc set version = version + 1, modifier_id = $1, modified = now() where id = $2", [z_acl:user(Context), Id], Context) of
        1 -> {ok, Id};
        0 -> {error, {unknown_rsc, Id}}
    end.
    

exists([C|_] = Name, Context) when is_list(Name) and is_integer(C) ->
    case rid_name(Name, Context) of
        undefined -> 
            case z_utils:only_digits(Name) of
                true -> exists(list_to_integer(Name), Context);
                false -> false
            end;
        _ -> true
    end;
exists(Name, Context) when is_binary(Name) ->
    case rid_name(Name, Context) of
        undefined -> false;
        _ -> true
    end;
exists(Id, Context) -> 
    case rid(Id, Context) of
        Rid when is_integer(Rid) ->
            case z_depcache:get({exists, Rid}, Context) of
                {ok, Exists} ->
                    Exists;
                undefined -> 
                    Exists = case z_db:q1("select id from rsc where id = $1", [Rid], Context) of
                        undefined -> false;
                        _ -> true
                    end,
                    z_depcache:set({exists, Rid}, Exists, ?DAY, [Rid], Context),
                    Exists
            end;
        undefined -> false
    end.
    
is_visible(Id, Context) ->
    case rid(Id, Context) of
        RscId when is_integer(RscId) ->
            z_acl:rsc_visible(RscId, Context);
        _ ->
            false
    end.

is_editable(Id, Context) -> 
    case rid(Id, Context) of
        RscId when is_integer(RscId) ->
            z_acl:rsc_editable(RscId, Context);
        _ ->
            false
    end.
    
is_ingroup(Id, Context) -> 
    case rid(Id, Context) of
        RscId when is_integer(RscId) ->
            z_acl:rsc_ingroup(RscId, Context);
        _ ->
            false
    end.

is_me(Id, Context) -> 
    case rid(Id, Context) of
        RscId when is_integer(RscId) ->
            z_acl:user(Context) == RscId;
        _ ->
            false
    end.


%% @doc Fetch a property from a resource. When the rsc does not exist, the property does not
%% exist or the user does not have access rights to the property then return 'undefined'.
%% p(ResourceId, atom(), Context) -> term() | undefined
p(Id, Property, Context) 
	when Property =:= category_id orelse Property =:= page_url 
	orelse Property =:= group orelse Property =:= category 
	orelse Property =:= category_id orelse Property =:= is_a 
	orelse Property =:= default_page_url ->
		p_no_acl(rid(Id, Context), Property, Context);
p(Id, Property, Context) ->
    case rid(Id, Context) of
        undefined -> 
			undefined;
		RId ->
			case z_acl:rsc_visible(RId, Context) of
				true -> p_no_acl(RId, Property, Context);
				false -> undefined
			end
	end.

%% @doc Fetch a property from a resource, no ACL check is done.
p_no_acl(undefined, _Predicate, _Context) -> undefined;
p_no_acl(Id, Prop, Context) when not is_integer(Id) -> p_no_acl(rid(Id, Context), Prop, Context);
p_no_acl(Id, o, Context)  -> o(Id, Context);
p_no_acl(Id, s, Context)  -> s(Id, Context);
p_no_acl(Id, op, Context) -> op(Id, Context);
p_no_acl(Id, sp, Context) -> sp(Id, Context);
p_no_acl(Id, is_me, Context) -> is_me(Id, Context);
p_no_acl(Id, is_visible, Context) -> is_visible(Id, Context);
p_no_acl(Id, is_editable, Context) -> is_editable(Id, Context);
p_no_acl(Id, is_ingroup, Context) -> is_ingroup(Id, Context);
p_no_acl(Id, is_a, Context) -> [ {C,true} || C <- is_a(Id, Context) ];
p_no_acl(Id, exists, Context) -> exists(Id, Context);
p_no_acl(Id, page_url, Context) -> 
    case p_no_acl(Id, page_path, Context) of
        undefined -> page_url(Id, Context);
        PagePath -> PagePath
    end;
p_no_acl(Id, default_page_url, Context) -> page_url(Id, Context);
p_no_acl(Id, resource_uri, Context) ->
    case p_no_acl(Id, is_authoritative, Context) of
        true ->  iolist_to_binary(z_context:abs_url(z_dispatcher:url_for(id, [{id, Id}], Context), Context));
        false -> p_cached(Id, resource_uri, Context) 
    end;
p_no_acl(Id, group, Context) -> 
    case p_no_acl(Id, group_id, Context) of
        undefined -> undefined;
        GroupId -> m_group:get(GroupId, Context)
    end;
p_no_acl(Id, category, Context) -> 
    m_category:get(p_no_acl(Id, category_id, Context), Context);
p_no_acl(Id, media, Context) -> media(Id, Context);
p_no_acl(Id, medium, Context) -> m_media:get(Id, Context);
p_no_acl(Id, depiction, Context) -> m_media:depiction(Id, Context);
p_no_acl(Id, predicates_edit, Context) -> predicates_edit(Id, Context);
    
% Check if the requested predicate is a readily available property or an edge
p_no_acl(Id, Predicate, Context) when is_integer(Id) -> 
    p_cached(Id, Predicate, Context).


    p_cached(Id, Predicate, Context) ->
        Value = case z_depcache:get(Id, Predicate, Context) of
            {ok, V} -> 
                V;
            undefined ->
                case get(Id, Context) of
					undefined -> undefined;
                    PropList ->  proplists:get_value(Predicate, PropList)
                end
        end,
        case Value of
            undefined ->
                % Unknown properties will be checked against the predicates, returns o(Predicate).
                case m_predicate:is_predicate(Predicate, Context) of
                    true -> o(Id, Predicate, Context);
                    false -> undefined
                end;
            _ ->
                Value
        end.


%% Return a list of all edge predicates of this resource
op(Id, Context) when is_integer(Id) ->
    m_edge:object_predicates(Id, Context);
op(undefined, _Context) -> 
    [];
op(Id, Context) ->
    op(rid(Id, Context), Context).

%% Used for dereferencing object edges inside template expressions
o(Id, _Context) ->
	fun(P, Context) -> o(Id, P, Context) end.

%% Return the list of objects with a certain predicate
o(Id, Predicate, Context) when is_integer(Id) ->
	#rsc_list{list=m_edge:objects(Id, Predicate, Context)};
o(undefined, _Predicate, _Context) ->
    #rsc_list{list=[]};
o(Id, Predicate, Context) ->
    o(rid(Id, Context), Predicate, Context).


%% Return the nth object in the predicate list
o(Id, Predicate, N, Context) when is_integer(Id) ->
    case m_edge:object(Id, Predicate, N, Context) of
        undefined -> undefined;
        ObjectId -> ObjectId
    end;
o(undefined, _Predicate, _N, _Context) ->
    undefined;
o(Id, Predicate, N, Context) ->
    o(rid(Id, Context), Predicate, N, Context).

	
%% Return a list of all edge predicates to this resource
sp(Id, Context) when is_integer(Id) ->
    m_edge:subject_predicates(Id, Context);
sp(undefined, _Context) -> 
    [];
sp(Id, Context) ->
    sp(rid(Id, Context), Context).

%% Used for dereferencing subject edges inside template expressions
s(Id, _Context) ->
	fun(P, Context) -> s(Id, P, Context) end.

%% Return the list of subjects with a certain predicate
s(Id, Predicate, Context) when is_integer(Id) ->
	#rsc_list{list=m_edge:subjects(Id, Predicate, Context)};
s(undefined, _Predicate, _Context) ->
    #rsc_list{list=[]};
s(Id, Predicate, Context) ->
    s(rid(Id, Context), Predicate, Context).

%% Return the nth object in the predicate list
s(Id, Predicate, N, Context) when is_integer(Id) ->
    case m_edge:subject(Id, Predicate, N, Context) of
        undefined -> undefined;
        SubjectId -> SubjectId
    end;
s(undefined, _Predicate, _N, _Context) ->
    undefined;
s(Id, Predicate, N, Context) ->
    s(rid(Id, Context), Predicate, N, Context).


%% Return the list of all media attached to the resource
media(Id, Context) when is_integer(Id) -> 
    m_edge:objects(Id, depiction, Context);
media(undefined, _Context) -> 
	[];
media(Id, Context) -> 
	media(rid(Id, Context), Context).

	
%% @doc Fetch a resource id from any input
rid(Id, _Context) when is_integer(Id) ->
	Id;
rid({Id}, _Context) when is_integer(Id) ->
    Id;
rid(#rsc_list{list=[R|_]}, _Context) ->
	R;
rid(#rsc_list{list=[]}, _Context) ->
	undefined;
rid([C|_] = UniqueName, Context) when is_integer(C) ->
    case z_utils:only_digits(UniqueName) of
        true -> list_to_integer(UniqueName);
        false -> rid_name(UniqueName, Context)
    end;
rid(UniqueName, Context) when is_binary(UniqueName) ->
    rid_name(binary_to_list(UniqueName), Context);
rid(undefined, _Context) -> 
	undefined;
rid(UniqueName, Context) when is_atom(UniqueName) -> 
    rid_name(atom_to_list(UniqueName), Context);
rid(<<>>, _Context) -> 
	undefined.


%% @doc Return the id of the resource with a certain unique name.
%% rid_name(Name, Context) -> int() | undefined
rid_name(Name, _Context) when is_integer(Name) ->
    Name;
rid_name(Name, Context) ->
    Lower = z_string:to_name(Name),
    case z_depcache:get({rsc_name, Lower}, Context) of
        {ok, undefined} ->
            undefined;
        {ok, Id} ->
            Id;
        undefined ->
            Id = case z_db:q1("select id from rsc where name = $1", [Lower], Context) of
                undefined -> undefined;
                Value -> Value
            end,
            z_depcache:set({rsc_name, Lower}, Id, ?DAY, [Id, {rsc_name, Lower}], Context),
            Id
    end.

%% @doc Check if the resource is exactly the category
is_cat(Id, Cat, Context) ->
    case m_category:name_to_id(Cat, Context) of
        {ok, CatId} ->
            RscCatId = p(Id, category_id, Context),
            case RscCatId of
                CatId ->
                    true;
                _ ->
                    Path = m_category:get_path(RscCatId, Context),
                    lists:any(fun(X) -> X == CatId end, Path)
            end;
        _ ->
            false
    end.

%% @doc Return the categories and the inherited categories of the resource. Returns a list with category atoms
%% @spec is_a(int(), Context) -> list()
is_a(Id, Context) ->
    RscCatId = p(Id, category_id, Context),
    m_category:is_a(RscCatId, Context).

%% @doc Check if the resource is in a categorie.
%% @spec is_a(int(), atom(), Context) -> bool()
is_a(Id, Cat, Context) ->
    RscCatId = p(Id, category_id, Context),
    m_category:is_a(RscCatId, Cat, Context).
    

page_url(Id, Context) ->
    case rid(Id, Context) of
        RscId when is_integer(RscId) ->
            Args = [{id,RscId}, {slug, p(RscId, slug, Context)}],
            CatPath = lists:reverse(is_a(Id, Context)),
            page_url_path(CatPath, Args, Context);
        _ ->
            undefined
    end.

page_url_path([], Args, Context) ->
    z_dispatcher:url_for(page, Args, Context);
page_url_path([CatName|Rest], Args, Context) ->
    case z_dispatcher:url_for(CatName, Args, Context) of
        undefined -> page_url_path(Rest, Args, Context);
        Url -> Url
    end.

%% @doc Return the predicates that are valid combined with the predicates that are actually used by the subject.
%% This list is to show which predicates are editable for the subject rsc.
%% @spec predicate_edit
predicates_edit(Id, Context) ->
    ByCategory = m_predicate:for_subject(Id, Context),
    Present = m_edge:object_predicate_ids(Id, Context),
    ByCategory ++ Present.
    
    
