%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2010 Marc Worrell
%% Date: 2009-04-24
%%
%% @doc Update routines for resources.  For use by the m_rsc module.

%% Copyright 2009-2010 Marc Worrell, Arjan Scherpenisse
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

-module(m_rsc_update).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    insert/2,
    insert/3,
    delete/2,
    update/3,
    update/4,
    duplicate/3,

    flush/2,
    
    delete_nocheck/2,
    props_filter/3,

    test/0
]).

-include_lib("zotonic.hrl").


%% @doc Insert a new resource. Crashes when insertion is not allowed.
%% @spec insert(Props, Context) -> {ok, Id} | {error, Reason}
insert(Props, Context) ->
    insert(Props, true, Context).

insert(Props, EscapeTexts, Context) ->
    PropsDefaults = props_defaults(Props, Context),
    update(insert_rsc, PropsDefaults, EscapeTexts, Context).


%% @doc Delete a resource
%% @spec delete(Props, Context) -> ok | {error, Reason}
delete(Id, Context) when is_integer(Id), Id /= 1 ->
    case z_acl:rsc_deletable(Id, Context) of
        true ->
            case m_rsc:is_a(Id, category, Context) of
                true ->
                    m_category:delete(Id, undefined, Context);
                false ->
                    delete_nocheck(Id, Context)
            end;
        false ->
            throw({error, eacces})
    end.


%% @doc Delete a resource, no check on rights etc is made. This is called by m_category:delete/3
%% @spec delete_nocheck(Id, Context) -> ok
%% @throws {error, Reason}
delete_nocheck(Id, Context) ->
    Referrers = m_edge:subjects(Id, Context),
    CatList = m_rsc:is_a(Id, Context),
    Props = m_rsc:get(Id, Context),
    
    F = fun(Ctx) ->
        z_notifier:notify(#rsc_delete{id=Id}, Ctx),
        z_db:delete(rsc, Id, Ctx)
    end,

    {ok, _RowsDeleted} = z_db:transaction(F, Context),
    %% After inserting a category we need to renumber the categories
    case lists:member(category, CatList) of
        true ->  m_category:renumber(Context);
        false -> nop
    end,
    % Sync the caches
    [ z_depcache:flush(SubjectId, Context) || SubjectId <- Referrers ],
    flush(Id, CatList, Context),
    %% Notify all modules that the rsc has been deleted
    z_notifier:notify(#rsc_update_done{
                        action=delete,
                        id=Id,
                        pre_is_a=CatList,
                        post_is_a=[],
                        pre_props=Props,
                        post_props=[]
                    }, Context),
    ok.


%% Flush all cached entries depending on this entry, one of its subjects or its categories.
flush(Id, Context) ->
    CatList = m_rsc:is_a(Id, Context),
    flush(Id, CatList, Context).
    
flush(Id, CatList, Context) ->
    z_depcache:flush(Id, Context),
    [ z_depcache:flush(Cat, Context) || Cat <- CatList ],
    ok.


%% @doc Duplicate a resource, creating a new resource with the given title.
%% @spec duplicate(int(), PropList, Context) -> {ok, int()}
%% @throws {error, Reason}
%% @todo Also duplicate the attached medium.
duplicate(Id, DupProps, Context) ->
    case z_acl:rsc_visible(Id, Context) of
        true ->
            Props = m_rsc:get_raw(Id, Context),
            FilteredProps = props_filter_protected(Props),
            SafeDupProps = z_html:escape_props(DupProps, Context),
            InsProps = lists:foldl(
                            fun({Key, Value}, Acc) ->
                                z_utils:prop_replace(Key, Value, Acc)
                            end,
                            FilteredProps,
                            SafeDupProps ++ [
                                {name,undefined}, {uri,undefined}, {page_path,undefined},
                                {is_authoritative,true}, {is_protected,false},
                                {slug,undefined}
                            ]),
            {ok, NewId} = insert(InsProps, false, Context),
            %% Duplicate all edges
            m_edge:duplicate(Id, NewId, Context),
            {ok, NewId};
        false ->
            throw({error, eacces})
    end.


%% @doc Update a resource
%% @spec update(Id, Props, Context) -> {ok, Id}
%% @throws {error, Reason}
update(Id, Props, Context) ->
    update(Id, Props, [], Context).

%% @doc Backward comp.
update(Id, Props, false, Context) ->
    update(Id, Props, [{escape_texts, false}], Context);
update(Id, Props, true, Context) ->
    update(Id, Props, [{escape_texts, true}], Context);

%% @doc Resource updater function
%% @spec update(Id, Props, Options, Context)
%% [Options]: {escape_texts, true|false (default: true}, {acl_check: true|false (default: true)}
update(Id, Props, Options, Context) when is_integer(Id) orelse Id == insert_rsc ->
    EscapeTexts = proplists:get_value(escape_texts, Options, true),
    AclCheck = proplists:get_value(acl_check, Options, true),
    IsImport = proplists:is_defined(is_import, Options),
    % IsTransMerge = proplists:is_defined(is_trans_merge, Options, false),
    IsEditable = Id == insert_rsc orelse z_acl:rsc_editable(Id, Context) orelse not(AclCheck),

    case IsEditable of
        true ->
            DateProps = recombine_dates(Props),
            TextProps = recombine_languages(DateProps, Context),
            BlockProps = recombine_blocks(TextProps),
            AtomProps = [ {map_property_name(P), V} || {P, V} <- BlockProps ],
            FilteredProps = props_filter(props_trim(AtomProps), [], Context),
            EditableProps = props_filter_protected(FilteredProps),
            AclCheckedProps = case z_acl:rsc_update_check(Id, EditableProps, Context) of
                L when is_list(L) -> L;
                {error, Reason} -> throw({error, Reason})
            end,
            AutogeneratedProps = props_autogenerate(Id, AclCheckedProps, Context),
            SafeProps = case EscapeTexts of
                true -> z_html:escape_props(AutogeneratedProps, Context);
                false -> AutogeneratedProps
            end,
            ok = preflight_check(Id, SafeProps, Context),

            %% This function will be executed in a transaction
            TransactionF = fun(Ctx) ->
                {RscId, UpdateProps, BeforeProps, BeforeCatList, RenumberCats} = case Id of
                    insert_rsc ->
                        CategoryId = z_convert:to_integer(proplists:get_value(category_id, SafeProps)),
                        CategoryName = m_category:id_to_name(CategoryId, Ctx),
                        InsProps = [{category_id, CategoryId}, {version, 0}],
                         % Allow the insertion props to be modified.
                        InsPropsN = z_notifier:foldr(#rsc_insert{}, InsProps, Ctx),

                        % Check if the user is allowed to create the resource
                        case z_acl:is_allowed(insert, #acl_rsc{category=CategoryName}, Ctx) orelse not(AclCheck) of
                            true ->
                                InsertId = case proplists:get_value(creator_id, Props) of
                                            self ->
                                                {ok, InsId} = z_db:insert(rsc, [{creator_id, undefined} | InsPropsN], Ctx),
                                                z_db:q("update rsc set creator_id = id where id = $1", [InsId], Ctx),
                                                InsId;
                                            CreatorId when is_integer(CreatorId) ->
                                                true = z_acl:is_admin(Ctx),
                                                {ok, InsId} = z_db:insert(rsc, [{creator_id, CreatorId} | InsPropsN], Ctx),
                                                InsId;
                                            undefined ->
                                                {ok, InsId} = z_db:insert(rsc, [{creator_id, z_acl:user(Ctx)} | InsPropsN], Ctx),
                                                InsId
                                        end,

                                % Insert a category record for categories. Categories are so low level that we want
                                % to make sure that all categories always have a category record attached.
                                InsertCatList = [ CategoryId | m_category:get_path(CategoryId, Ctx) ],
                                IsACat = case lists:member(m_category:name_to_id_check(category, Ctx), InsertCatList) of
                                    true ->
                                        1 = z_db:q("insert into category (id, seq) values ($1, 1)", [InsertId], Ctx),
                                        true;
                                    false ->
                                        false
                                end,
                                 % Place the inserted properties over the update properties, replacing duplicates.
                                SafePropsN = lists:foldl(
                                                        fun
                                                            ({version, _}, Acc) -> Acc;
                                                            ({P,V}, Acc) -> z_utils:prop_replace(P, V, Acc)
                                                        end,
                                                        SafeProps,
                                                        InsPropsN),
                                {InsertId, SafePropsN, InsPropsN, [], IsACat};
                            false ->
                                throw({error, eacces})
                        end;
                    _ ->
                        SafeProps1 = case z_acl:is_admin(Context) of
                            true ->
                                case proplists:get_value(creator_id, Props) of
                                    self ->
                                        [{creator_id, Id} | SafeProps];
                                    CreatorId when is_integer(CreatorId) ->
                                        [{creator_id, CreatorId} | SafeProps];
                                    undefined -> 
                                        SafeProps
                                end;
                            false ->
                                SafeProps
                        end,
                        {Id, SafeProps1, m_rsc:get(Id, Ctx), m_rsc:is_a(Id, Ctx), false}
                end,

                UpdateProps1 = [
                    {version, z_db:q1("select version+1 from rsc where id = $1", [RscId], Ctx)},
                    {modifier_id, z_acl:user(Ctx)}
                    | UpdateProps
                ],
                
                % Optionally fetch the created date if this is an import of a resource
                UpdateProps2 = case imported_prop(IsImport, created, AtomProps, undefined) of
                                   undefined -> UpdateProps1;
                                   CreatedDT -> [{created, CreatedDT}|UpdateProps1]
                               end,
                
                UpdateProps3 = case IsImport of
                                  false ->
                                      [{modified, calendar:local_time()} | UpdateProps2 ];
                                  true ->
                                      case imported_prop(IsImport, created, AtomProps, undefined) of
                                          undefined -> UpdateProps2;
                                          ModifiedDT -> [{modified, ModifiedDT}|UpdateProps2]
                                      end
                              end,

                % Allow the update props to be modified.
                {Changed, UpdatePropsN} = z_notifier:foldr(#rsc_update{
                                                        action=case Id of insert_rsc -> insert; _ -> update end,
                                                        id=RscId, 
                                                        props=BeforeProps
                                                    }, 
                                                    {false, UpdateProps3},
                                                    Ctx),
                UpdatePropsN1 = case proplists:get_value(category_id, UpdatePropsN) of
                    undefined ->
                        UpdatePropsN;
                    CatId ->
                        CatNr = z_db:q1("select nr from category where id = $1", [CatId], Ctx),
                        [ {pivot_category_nr, CatNr} | UpdatePropsN]
                end,

                RawProps = case Id of 
                              insert_rsc -> [];
                              _ -> m_rsc:get_raw(Id, Context)
                           end,
                case Id =:= insert_rsc orelse Changed orelse is_changed(RawProps, UpdatePropsN1) of
                    true ->
                        UpdatePropsPrePivoted = z_pivot_rsc:pivot_resource_update(RscId, UpdatePropsN1, RawProps, Context),
                        {ok, _RowsModified} = z_db:update(rsc, RscId, UpdatePropsPrePivoted, Ctx),
                        {ok, RscId, BeforeProps, UpdatePropsN, BeforeCatList, RenumberCats};
                    false ->
                        {ok, RscId, notchanged}
                end
            end,
            % End of transaction function

            case z_db:transaction(TransactionF, Context) of
                {ok, NewId, notchanged} ->
                    {ok, NewId};
                {ok, NewId, OldProps, NewProps, OldCatList, RenumberCats} ->
                    % Flush some low level caches
                    case proplists:get_value(name, NewProps) of
                        undefined -> nop;
                        Name -> z_depcache:flush({rsc_name, z_convert:to_list(Name)}, Context)
                    end,
                    case proplists:get_value(uri, NewProps) of
                        undefined -> nop;
                        Uri -> z_depcache:flush({rsc_uri, z_convert:to_list(Uri)}, Context)
                    end,

                    % After inserting a category we need to renumber the categories
                    case RenumberCats of
                        true ->  m_category:renumber(Context);
                        false -> nop
                    end,

                    % Flush all cached content that is depending on one of the updated categories
                    z_depcache:flush(NewId, Context),
                    NewCatList = m_rsc:is_a(NewId, Context),
                    [ z_depcache:flush(Cat, Context) || Cat <- lists:usort(NewCatList ++ OldCatList) ],

                     % Notify that a new resource has been inserted, or that an existing one is updated
                    Note = #rsc_update_done{
                        action= case Id of insert_rsc -> insert; _ -> update end,
                        id=NewId,
                        pre_is_a=OldCatList,
                        post_is_a=NewCatList,
                        pre_props=OldProps,
                        post_props=NewProps
                    },
                    z_notifier:notify(Note, Context),

                    % Return the updated or inserted id
                    {ok, NewId};
                {rollback, {_Why, _} = Er} ->
                    throw(Er)
            end;
        false ->
            E = case m_rsc:p(Id, is_authoritative, Context) of
                false -> {error, non_authoritative};
                true -> {error, eacces}
            end,
            throw(E)
    end.


    imported_prop(false, _, _, Default) ->
        Default;
    imported_prop(true, Prop, Props, Default) ->
        proplists:get_value(Prop, Props, Default).
        

%% @doc Check if the update will change the data in the database
%% @spec is_changed(Current, Props) -> bool()
is_changed(Current, Props) ->
    is_prop_changed(Props, Current).

    is_prop_changed([], _Current) ->
        false;
    is_prop_changed([{version, _}|Rest], Current) ->
        is_prop_changed(Rest, Current);
    is_prop_changed([{modifier_id, _}|Rest], Current) ->
        is_prop_changed(Rest, Current);
    is_prop_changed([{modified, _}|Rest], Current) ->
        is_prop_changed(Rest, Current);
    is_prop_changed([{pivot_category_nr, _}|Rest], Current) ->
        is_prop_changed(Rest, Current);
    is_prop_changed([{Prop, Value}|Rest], Current) ->
        case z_utils:are_equal(Value, proplists:get_value(Prop, Current)) of
            true -> is_prop_changed(Rest, Current);
            false -> true  % The property Prop has been changed.
        end.



%% @doc Check if all props are acceptable. Examples are unique name, uri etc.
%% @spec preflight_check(Id, Props, Context) -> ok | {error, Reason}
preflight_check(insert_rsc, Props, Context) ->
    preflight_check(-1, Props, Context);
preflight_check(_Id, [], _Context) ->
    ok;
preflight_check(Id, [{name, Name}|T], Context) when Name =/= undefined ->
    case z_db:q1("select count(*) from rsc where name = $1 and id <> $2", [Name, Id], Context) of
        0 ->  preflight_check(Id, T, Context);
        _N -> throw({error, duplicate_name})
    end;
preflight_check(Id, [{page_path, Path}|T], Context) when Path =/= undefined ->
    case z_db:q1("select count(*) from rsc where page_path = $1 and id <> $2", [Path, Id], Context) of
        0 ->  preflight_check(Id, T, Context);
        _N -> throw({error, duplicate_page_path})
    end;
preflight_check(Id, [{uri, Uri}|T], Context) when Uri =/= undefined ->
    case z_db:q1("select count(*) from rsc where uri = $1 and id <> $2", [Uri, Id], Context) of
        0 ->  preflight_check(Id, T, Context);
        _N -> throw({error, duplicate_uri})
    end;
preflight_check(Id, [{'query', Query}|T], Context) ->
    Valid = case m_rsc:is_a(Id, 'query', Context) of
                true ->
                    try
                        search_query:search(search_query:parse_query_text(Query), Context), true
                    catch
                        _: {error, {_, _}} ->
                            false
                    end;
                false -> true
            end,
    case Valid of
        true -> preflight_check(Id, T, Context);
        false -> throw({error, invalid_query})
    end;
preflight_check(Id, [_H|T], Context) ->
    preflight_check(Id, T, Context).


%% @doc Remove whitespace around some predefined fields
props_trim(Props) ->
    [
        case is_trimmable(P,V) of
            true -> {P, z_string:trim(V)};
            false -> {P,V}
        end
        || {P,V} <- Props
    ].


%% @doc Remove properties the user is not allowed to change and convert some other to the correct data type
%% @spec props_filter(Props1, Acc, Context) -> Props2
props_filter([], Acc, _Context) ->
    Acc;

props_filter([{uri, Uri}|T], Acc, Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        true ->
            case Uri of
                Empty when Empty == undefined; Empty == []; Empty == <<>> ->
                    props_filter(T, [{uri, undefined} | Acc], Context);
                _ ->
                    props_filter(T, [{uri, Uri} | Acc], Context)
            end;
        false ->
            props_filter(T, Acc, Context)
    end;

props_filter([{name, Name}|T], Acc, Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        true ->
            case Name of
                Empty when Empty == undefined; Empty == []; Empty == <<>> ->
                    props_filter(T, [{name, undefined} | Acc], Context);
                _ ->
                    props_filter(T, [{name, z_string:to_name(Name)} | Acc], Context)
            end;
        false ->
            props_filter(T, Acc, Context)
    end;

props_filter([{page_path, Path}|T], Acc, Context) ->
    case z_acl:is_allowed(use, mod_admin, Context) of
        true ->
            case Path of
                Empty when Empty == undefined; Empty == []; Empty == <<>> ->
                    props_filter(T, [{page_path, undefined} | Acc], Context);
                _ ->
                    case [$/ | string:strip(z_utils:url_path_encode(Path), both, $/)] of
                        [] -> props_filter(T, [{page_path, undefined} | Acc], Context);
                        P  -> props_filter(T, [{page_path, P} | Acc], Context)
                    end
            end;
        false ->
            props_filter(T, Acc, Context)
    end;
props_filter([{slug, undefined}|T], Acc, Context) ->
    props_filter(T, [{slug, []} | Acc], Context);
props_filter([{slug, <<>>}|T], Acc, Context) ->
    props_filter(T, [{slug, []} | Acc], Context);
props_filter([{slug, ""}|T], Acc, Context) ->
    props_filter(T, [{slug, []} | Acc], Context);
props_filter([{slug, Slug}|T], Acc, Context) ->
    props_filter(T, [{slug, z_string:to_slug(Slug)} | Acc], Context);
props_filter([{custom_slug, P}|T], Acc, Context) ->
    props_filter(T, [{custom_slug, z_convert:to_bool(P)} | Acc], Context);
props_filter([{is_published, P}|T], Acc, Context) ->
    props_filter(T, [{is_published, z_convert:to_bool(P)} | Acc], Context);
props_filter([{is_authoritative, P}|T], Acc, Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        true ->
            props_filter(T, [{is_authoritative, z_convert:to_bool(P)} | Acc], Context);
        false ->
            props_filter(T, Acc, Context)
    end;
props_filter([{is_featured, P}|T], Acc, Context) ->
    props_filter(T, [{is_featured, z_convert:to_bool(P)} | Acc], Context);
props_filter([{is_query_live, P}|T], Acc, Context) ->
    props_filter(T, [{is_query_live, z_convert:to_bool(P)} | Acc], Context);
props_filter([{visible_for, Vis}|T], Acc, Context) ->
    VisibleFor = z_convert:to_integer(Vis),
    case VisibleFor of
        N when N >= 0 ->
            props_filter(T, [{visible_for, N} | Acc], Context);
        _ ->
            props_filter(T, Acc, Context)
    end;

props_filter([{category, CatName}|T], Acc, Context) ->
    props_filter([{category_id, m_category:name_to_id_check(CatName, Context)} | T], Acc, Context);

props_filter([{Location, P}|T], Acc, Context) when Location =:= location_lat; Location =:= location_lng ->
    case catch z_convert:to_float(P) of
        X when is_float(X) -> 
            props_filter(T, [{Location, X} | Acc], Context);
        _ ->
            props_filter(T, [{Location, undefined} | Acc], Context)
    end;

props_filter([{_Prop, _V}=H|T], Acc, Context) ->
    props_filter(T, [H|Acc], Context).


%% @doc Automatically modify some props on update.
%% @spec props_autogenerate(Id, Props1, Context) -> Props2
props_autogenerate(Id, Props, Context) ->
    %% When title is updating, check the rsc 'custom_slug' field to see if we need to update the slug or not.
    Props1 = case proplists:get_value(title, Props) of
                 undefined -> Props;
                 Title ->
                     case {proplists:get_value(custom_slug, Props), m_rsc:p(custom_slug, Id, Context)} of
                         {true, _} -> Props;
                         {_, true} -> Props;
                         _X ->
                             %% Determine the slug from the title.
                             [{slug, z_string:to_slug(Title)} | proplists:delete(slug, Props)]
                     end
             end,
    Props1.


%% @doc Fill in some defaults for empty props on insert.
%% @spec props_defaults(Props1, Context) -> Props2
props_defaults(Props, _Context) ->
    % Generate slug from the title (when there is a title)
    Props1 = case proplists:get_value(slug, Props) of
                undefined ->
                    case proplists:get_value(title, Props) of
                        undefined ->
                            Props;
                        Title ->
                            lists:keystore(slug, 1, Props, {slug, z_string:to_slug(Title)})
                    end;
                _ ->
                    Props
             end,
    % Assume content is authoritative, unless stated otherwise
    case proplists:get_value(is_authoritative, Props1) of
        undefined -> [{is_authoritative, true}|Props1];
        _ -> Props
    end.


props_filter_protected(Props) ->
    lists:filter(fun({K,_V}) -> not is_protected(K) end, Props).


%% @doc Map property names to an atom, fold pivot and computed fields together for later filtering.
map_property_name(P) when not is_list(P) -> map_property_name(z_convert:to_list(P));
map_property_name("computed_"++_) -> computed_xxx;
map_property_name("pivot_"++_) -> pivot_xxx;
map_property_name(P) when is_list(P) -> erlang:list_to_existing_atom(P).


%% @doc Properties that can't be updated with m_rsc_update:update/3 or m_rsc_update:insert/2
is_protected(id)            -> true;
is_protected(created)       -> true;
is_protected(creator_id)    -> true;
is_protected(modified)      -> true;
is_protected(modifier_id)   -> true;
is_protected(props)         -> true;
is_protected(version)       -> true;
is_protected(page_url)      -> true;
is_protected(medium)        -> true;
is_protected(pivot_xxx)     -> true;
is_protected(computed_xxx)  -> true;
is_protected(_)             -> false.


is_trimmable(_, V) when not is_binary(V) and not is_list(V) -> false;
is_trimmable(title, _)       -> true;
is_trimmable(title_short, _) -> true;
is_trimmable(summary, _)     -> true;
is_trimmable(chapeau, _)     -> true;
is_trimmable(subtitle, _)    -> true;
is_trimmable(email, _)       -> true;
is_trimmable(uri, _)         -> true;
is_trimmable(website, _)     -> true;
is_trimmable(page_path, _)   -> true;
is_trimmable(name, _)        -> true;
is_trimmable(slug, _)        -> true;
is_trimmable(custom_slug, _) -> true;
is_trimmable(category, _)    -> true;
is_trimmable(_, _)           -> false.


recombine_dates(Props) ->
    Now = erlang:localtime(),
    {Dates, Props1} = recombine_dates(Props, [], []),
    {Dates1, DateGroups} = group_dates(Dates),
    {DateGroups1, DatesNull} = collect_empty_date_groups(DateGroups, [], []),
    {Dates2, DatesNull1} = collect_empty_dates(Dates1, [], DatesNull),
    Dates3 = [ {Name, date_from_default(Now, D)} || {Name, D} <- Dates2 ],
    DateGroups2 = [ {Name, dategroup_fill_parts(date_from_default(Now, S), E)} || {Name, {S,E}} <- DateGroups1 ],
    Dates4 = lists:foldl(fun({Name, {S, E}}, Acc) -> [{Name++"_start", S}, {Name++"_end", E} | Acc] end, Dates3, DateGroups2),
    Dates4 ++ DatesNull1 ++ Props1.


collect_empty_date_groups([], Acc, Null) ->
    {Acc, Null};
collect_empty_date_groups([{"publication", _} = R|T], Acc, Null) ->
    collect_empty_date_groups(T, [R|Acc], Null);
collect_empty_date_groups([{Name, {
                            {{undefined, undefined, undefined}, {undefined, undefined, undefined}},
                            {{undefined, undefined, undefined}, {undefined, undefined, undefined}}
                            }}|T], Acc, Null) ->
    collect_empty_date_groups(T, Acc, [{Name++"_start", undefined}, {Name++"_end", undefined} | Null]);
collect_empty_date_groups([H|T], Acc, Null) ->
    collect_empty_date_groups(T, [H|Acc], Null).



collect_empty_dates([], Acc, Null) ->
    {Acc, Null};
collect_empty_dates([{Name, {{undefined, undefined, undefined}, {undefined, undefined, undefined}}}|T], Acc, Null) ->
    collect_empty_dates(T, Acc, [{Name, undefined}|Null]);
collect_empty_dates([H|T], Acc, Null) ->
    collect_empty_dates(T, [H|Acc], Null).



recombine_dates([], Dates, Acc) ->
    {Dates, Acc};
recombine_dates([{"dt:"++K,V}|T], Dates, Acc) ->
    [Part, End, Name] = string:tokens(K, ":"),
    Dates1 = recombine_date(Part, End, Name, V, Dates),
    recombine_dates(T, Dates1, Acc);
recombine_dates([H|T], Dates, Acc) ->
    recombine_dates(T, Dates, [H|Acc]).

    recombine_date(Part, End, Name, undefined, Dates) ->
        recombine_date(Part, End, Name, "", Dates);
    recombine_date(Part, _End, Name, V, Dates) ->
        Date = case proplists:get_value(Name, Dates) of
            undefined ->
                {{undefined, undefined, undefined}, {undefined, undefined, undefined}};
            D ->
                D
        end,
        Date1 = recombine_date_part(Date, Part, to_date_value(Part, string:strip(V))),
        lists:keystore(Name, 1, Dates, {Name, Date1}).

    recombine_date_part({{_Y,M,D},{H,I,S}}, "y", V) -> {{V,M,D},{H,I,S}};
    recombine_date_part({{Y,_M,D},{H,I,S}}, "m", V) -> {{Y,V,D},{H,I,S}};
    recombine_date_part({{Y,M,_D},{H,I,S}}, "d", V) -> {{Y,M,V},{H,I,S}};
    recombine_date_part({{Y,M,D},{_H,I,S}}, "h", V) -> {{Y,M,D},{V,I,S}};
    recombine_date_part({{Y,M,D},{H,_I,S}}, "i", V) -> {{Y,M,D},{H,V,S}};
    recombine_date_part({{Y,M,D},{H,I,_S}}, "s", V) -> {{Y,M,D},{H,I,V}};
    recombine_date_part({{Y,M,D},{_H,_I,S}}, "hi", {H,I,_S}) -> {{Y,M,D},{H,I,S}};
    recombine_date_part({{Y,M,D},_Time}, "his", {_,_,_} = V) -> {{Y,M,D},V};
    recombine_date_part({_Date,{H,I,S}}, "ymd", {_,_,_} = V) -> {V,{H,I,S}}.

    to_date_value(Part, V) when Part == "ymd" orelse Part == "his"->
        case string:tokens(V, "-/: ") of
            [] -> {undefined, undefined, undefined};
            [Y,M,D] -> {to_int(Y), to_int(M), to_int(D)}
        end;
    to_date_value("hi", V) ->
        case string:tokens(V, "-/: ") of
            [] -> {undefined, undefined, undefined};
            [H] -> {to_int(H), 0, undefined};
            [H,I] -> {to_int(H), to_int(I), undefined}
        end;
    to_date_value(_, V) ->
        to_int(V).

group_dates(Dates) ->
    group_dates(Dates, [], []).

    group_dates([], Groups, Acc) ->
        {Acc, Groups};
    group_dates([{Name,D}|T], Groups, Acc) ->
        case lists:suffix("_start", Name) of
            true ->
                Base = lists:sublist(Name, length(Name) - 6),
                Range = case proplists:get_value(Base, Groups) of
                    {_Start, End} ->
                        { D, End };
                    undefined ->
                        { D, {{undefined, undefined, undefined}, {undefined, undefined, undefined}} }
                end,
                Groups1 = lists:keystore(Base, 1, Groups, {Base, Range}),
                group_dates(T, Groups1, Acc);

            false ->
                case lists:suffix("_end", Name) of
                    true ->
                        Base = lists:sublist(Name, length(Name) - 4),
                        Range = case proplists:get_value(Base, Groups) of
                            {Start, _End} ->
                                { Start, D };
                            undefined ->
                                { {{undefined, undefined, undefined}, {0, 0, 0}}, D }
                        end,
                        Groups1 = lists:keystore(Base, 1, Groups, {Base, Range}),
                        group_dates(T, Groups1, Acc);

                    false ->
                        group_dates(T, Groups, [T|Acc])
                end
        end.


dategroup_fill_parts( S, {{undefined,undefined,undefined},{undefined,undefined,undefined}} ) ->
    {S, ?ST_JUTTEMIS};
dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{undefined,Me,De},{He,Ie,Se}} ) ->
    dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ys,Me,De},{He,Ie,Se}} );
dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,undefined,De},{He,Ie,Se}} ) ->
    dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}} ,{{Ye,Ms,De},{He,Ie,Se}} );
dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,undefined},{He,Ie,Se}} ) ->
    dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,Ds},{He,Ie,Se}} );
dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{undefined,Ie,Se}} ) ->
    dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{23,Ie,Se}} );
dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{He,undefined,Se}} ) ->
    dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{He,59,Se}} );
dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{He,Ie,undefined}} ) ->
    dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{He,Ie,59}} );
dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{He,Ie,Se}} ) ->
    {{{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{He,Ie,Se}}}.


date_from_default( S, {{undefined,undefined,undefined},{undefined,undefined,undefined}} ) ->
    S;
date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{undefined,Me,De},{He,Ie,Se}} ) ->
    date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ys,Me,De},{He,Ie,Se}} );
date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,undefined,De},{He,Ie,Se}} ) ->
    date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Ms,De},{He,Ie,Se}} );
date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,undefined},{He,Ie,Se}} ) ->
    date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,Ds},{He,Ie,Se}} );
date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{undefined,Ie,Se}} ) ->
    date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{0,Ie,Se}} );
date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{He,undefined,Se}} ) ->
    date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{He,0,Se}} );
date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{He,Ie,undefined}} ) ->
    date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{He,Ie,0}} );
date_from_default( _S, {{Ye,Me,De},{He,Ie,Se}} ) ->
    {{Ye,Me,De},{He,Ie,Se}}.

to_int("") ->
    undefined;
to_int(A) ->
    try
        list_to_integer(A)
    catch
        _:_ -> undefined
    end.


%% @doc get all languages encoded in proplists' keys.
%% e.g. m_rsc_update:props_languages([{"foo$en", x}, {"bar$nl", x}]) -> ["en", "nl"]
props_languages(Props) ->
    lists:foldr(fun({Key, _}, Acc) ->
                        case string:tokens(z_convert:to_list(Key), [$$]) of
                            [_, Lang] ->
                                case lists:member(Lang, Acc) of
                                    true -> Acc;
                                    false -> [Lang|Acc]
                                end;
                            _ -> Acc
                        end
                end, [], Props).


%% @doc Combine language versions of texts. Assume we edit all texts or none.
recombine_languages(Props, Context) ->
    case props_languages(Props) of
        [] ->
            Props;
        L -> 
            Cfg = [ atom_to_list(Code) || Code <- config_langs(Context) ],
            L1 = filter_langs(edited_languages(Props, L), Cfg),
            {LangProps, OtherProps} = comb_lang(Props, L1, [], []),
            LangProps ++ [{language, [list_to_atom(Lang) || Lang <- L1]}|proplists:delete("language", OtherProps)]
    end.

    edited_languages(Props, PropLangs) ->
        case proplists:is_defined("language", Props) of
            true -> proplists:get_all_values("language", Props);
            false -> PropLangs
        end.

    comb_lang([], _L1, LAcc, OAcc) ->
        {LAcc, OAcc};
    comb_lang([{P,V}|Ps], L1, LAcc, OAcc) when is_list(P) ->
        case string:tokens(P, "$") of
            [P1,Lang] ->
                case lists:member(Lang, L1) of
                    true -> comb_lang(Ps, L1, append_langprop(P1, Lang, V, LAcc), OAcc);
                    false -> comb_lang(Ps, L1, LAcc, OAcc)
                end;
            _ ->
                comb_lang(Ps, L1, LAcc, [{P,V}|OAcc])
        end;
    comb_lang([PV|Ps], L1, LAcc, OAcc) ->
        comb_lang(Ps, L1, LAcc, [PV|OAcc]).


    append_langprop(P, Lang, V, Acc) ->
        Lang1 = list_to_atom(Lang),
        case proplists:get_value(P, Acc) of
            {trans, Tr} ->
                Tr1 = [{Lang1, z_convert:to_binary(V)}|Tr],
                [{P, {trans, Tr1}} | proplists:delete(P, Acc)];
            undefined ->
                [{P, {trans, [{Lang1,z_convert:to_binary(V)}]}}|Acc]
        end.


recombine_blocks(Props) ->
    {BPs, Ps} = lists:partition(fun({"block-"++ _, _}) -> true; (_) -> false end, Props),
    case BPs of
        [] ->
            Props;
        _ ->
            {Dict,Keys} = lists:foldr(
                            fun ({"block-", _}, Acc) -> 
                                    Acc;
                                ({Name, Val}, {D,Ks}) ->
                                    Ts = string:tokens(Name, "-"),
                                    BlockId = iolist_to_binary(tl(lists:reverse(Ts))),
                                    BlockField = list_to_existing_atom(lists:last(Ts)),
                                    Ks1 = case lists:member(BlockId, Ks) of
                                            true -> Ks;
                                            false -> [BlockId|Ks]
                                          end,
                                    {
                                        dict:append(BlockId, {BlockField, Val}, D),
                                        Ks1
                                    }
                            end,
                            {dict:new(), []},
                            BPs),
            [{blocks, [ dict:fetch(K, Dict) || K <- Keys ]} | Ps ]
    end.


%% @doc Accept only configured languages
filter_langs(L, Cfg) ->
    filter_langs1(L, Cfg, []).
    
    filter_langs1([], _Cfg, Acc) ->
        Acc;
    filter_langs1([L|R], Cfg, Acc) ->
        case lists:member(L, Cfg) of
            true -> filter_langs1(R, Cfg, [L|Acc]);
            false -> filter_langs1(R, Cfg, Acc)
        end.

    

config_langs(Context) ->
    case m_config:get(i18n, language_list, Context) of
        undefined -> [en];
        Cfg -> [ Code || {Code, _} <- proplists:get_value(list, Cfg, [{en,[]}]) ]
    end.


test() ->
    [{"publication_start",{{2009,7,9},{0,0,0}}},
          {"publication_end",?ST_JUTTEMIS},
          {"plop","hello"}]
     = recombine_dates([
        {"dt:y:0:publication_start", "2009"},
        {"dt:m:0:publication_start", "7"},
        {"dt:d:0:publication_start", "9"},
        {"dt:y:1:publication_end", ""},
        {"dt:m:1:publication_end", ""},
        {"dt:d:1:publication_end", ""},
        {"plop", "hello"}
    ]),
    ok.

