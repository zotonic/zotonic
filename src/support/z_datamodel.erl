%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% Date: 2009-11-08
%% @doc Installing parts of the zotonic datamodel. Installs
%% predicates, categories and default resources.

%% Copyright 2009 Arjan Scherpenisse
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

-module(z_datamodel).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([manage/3, manage/4, reset_deleted/2]).

-type datamodel_options() :: [datamodel_option()].
-type datamodel_option() :: force_update.


%% The datamodel manages parts of your datamodel. This includes
%% categories and predicates, but also "default data" like resources.
%%
%% The data model maintains a special property on this installed data,
%% called 'installed_by'. This decides whether it can touch it.
%%

-include_lib("zotonic.hrl").


%% @doc Reset the state of an imported datamodel, causing all deleted resources to be reimported
reset_deleted(Module, Context) ->
    m_config:delete(Module, datamodel, Context).

%% @doc Install / update a set of named, predefined resources, categories, predicates, media and edges.
-spec manage(atom(), #datamodel{}, #context{}) -> ok.
manage(Module, Datamodel, Context) when is_list(Datamodel) ->
    %% Backwards compatibility with old datamodel notation.
    manage(Module,
           #datamodel{categories=proplists:get_value(categories, Datamodel, []),
                      predicates=proplists:get_value(predicates, Datamodel, []),
                      resources=proplists:get_value(resources, Datamodel, []),
                      media=proplists:get_value(media, Datamodel, []),
                      edges=proplists:get_value(edges, Datamodel, []),
                      data=proplists:get_value(data, Datamodel, [])
                     },
           Context);

manage(Module, Datamodel, Context) ->
    manage(Module, Datamodel, [], Context).

%% @doc Install / update a set of named, predefined resources, categories, predicates, media and edges.
-spec manage(atom(), #datamodel{}, datamodel_options(), #context{}) -> ok.
manage(Module, Datamodel, Options, Context) ->
    AdminContext = z_acl:sudo(Context),
    [manage_category(Module, Cat, Options, AdminContext) || Cat <- Datamodel#datamodel.categories],
    [manage_predicate(Module, Pred, Options, AdminContext) || Pred <- Datamodel#datamodel.predicates],
    [manage_resource(Module, R, Options, AdminContext) || R <- Datamodel#datamodel.resources],
    [manage_medium(Module, Medium, Options, AdminContext) || Medium <- Datamodel#datamodel.media],
    [manage_edge(Module, Edge, Options, AdminContext) || Edge <- Datamodel#datamodel.edges],
    [manage_data(Module, Data, AdminContext) || Data <- Datamodel#datamodel.data],
    ok.

manage_medium(Module, {Name, Props}, Options, Context) ->
    manage_resource(Module, {Name, media, Props}, Options, Context);

manage_medium(Module, {Name, {EmbedService, EmbedCode}, Props}, Options, Context) ->
    case manage_resource(Module, {Name, media, Props}, Options, Context) of
        ok ->
            ok;
        {ok, Id} ->
            MediaProps = [{mime, "text/html-video-embed"}, 
                          {video_embed_service, EmbedService}, 
                          {video_embed_code, EmbedCode}
                         ],
            m_media:replace(Id, MediaProps, Context),
            {ok, Id}
    end;

manage_medium(Module, {Name, Filename, Props}, Options, Context) ->
    case manage_resource(Module, {Name, media, Props}, Options, Context) of
        ok ->
            ok;
        {ok, Id} ->
            m_media:replace_file(Filename, Id, Context),
            {ok, Id}
    end.


manage_category(Module, {Name, ParentCategory, Props}, Options, Context) ->
    case manage_resource(Module, {Name, category, Props}, Options, Context) of
        ok ->
            ok;
        {ok, Id} ->
            case ParentCategory of
                undefined ->
                    ok;
                _ ->
                    case m_category:name_to_id(ParentCategory, Context) of
                        {ok, PId} ->
                            m_category:move_below(Id, PId, Context);
                        _ ->
                            throw({error, {nonexisting_parent_category, ParentCategory}})
                    end
            end
    end.


manage_predicate(Module, {Name, Uri, Props, ValidFor}, Options, Context) ->
    manage_predicate(Module, {Name, [{uri,Uri}|Props], ValidFor}, Options, Context);

manage_predicate(Module, {Name, Props, ValidFor}, Options, Context) ->
    Category = proplists:get_value(category, Props, predicate),
    case manage_resource(Module, {Name, Category, lists:keydelete(category, 1, Props)}, Options, Context) of
        ok ->
            ok;
        {ok, Id} ->
            ok = manage_predicate_validfor(Id, ValidFor, Options, Context),
            {ok, Id}
    end.


manage_resource(Module, {Name, Category, Props0}, Options, Context) ->
    case m_category:name_to_id(Category, Context) of
        {ok, CatId} -> 
            Props = map_props(Props0, Context),
            case m_rsc:name_to_id(Name, Context) of
                {ok, Id} ->
                    case m_rsc:p(Id, installed_by, Context) of
                        Module ->
                            NewProps = update_new_props(Module, Id, Props, Options, Context),
                            m_rsc_update:update(Id, [{managed_props, z_html:escape_props(Props)} | NewProps],
                                                [{is_import, true}], Context),
                            ok;
                        _ ->
                            %% Resource exists but is not installed by us.
                            ?zInfo(io_lib:format("Resource '~p' (~p) exists but is not managed by ~p.", [Name, Id, Module]), Context),
                            ok
                    end;
                {error, {unknown_rsc, _}} ->
                    %% new resource, or old resource
                    Props1 = [{name, Name}, {category_id, CatId},
                              {installed_by, Module}, {managed_props, z_html:escape_props(Props)}] ++ Props,
                    Props2 = case proplists:get_value(is_published, Props1) of
                                 undefined -> [{is_published, true} | Props1];
                                 _ -> Props1
                             end,
                    Props3 = case proplists:get_value(visible_for, Props2) of
                                 undefined -> [{visible_for, ?ACL_VIS_PUBLIC} | Props2];
                                 _ -> Props2
                             end,
                    Props4 = case proplists:get_value(is_protected, Props3) of
                                 undefined -> [{is_protected, true} | Props3];
                                 _ -> Props3
                             end,
                    Props5 = case proplists:get_value(is_dependent, Props4) of
                                 undefined -> [{is_dependent, false} | Props4];
                                 _ -> Props4
                             end,
                    ?zInfo(io_lib:format("Creating new ~p '~p'", [Category, Name]), Context),
                    {ok, Id} = m_rsc_update:update(insert_rsc, Props5, [{is_import, true}], Context),
                    case proplists:get_value(media_url, Props5) of
                        undefined ->
                            nop;
                        Url ->
                            m_media:replace_url(Url, Id, [], Context)
                    end,
                    case proplists:get_value(media_file, Props5) of
                        undefined ->
                            nop;
                        File ->
                            m_media:replace_file(File, Id, Context)
                    end,
                    {ok, Id}
            end;
        {error, _} ->
            Msg = io_lib:format("Resource '~p' could not be handled because the category ~p does not exist.", [Name, Category]),
            ?zWarning(Msg, Context),
            ok
    end.

manage_data(Module, Data, AdminContext) ->
    z_notifier:first(#manage_data{module = Module, props = Data}, AdminContext).

update_new_props(Module, Id, NewProps, Options, Context) ->
    case m_rsc:p(Id, managed_props, Context) of
        undefined ->
            NewProps;
        PreviousProps ->
            lists:foldl(fun({K, V}, Props) ->
                                case m_rsc:p(Id, K, Context) of
                                    V ->
                                        %% New value == current value
                                        Props;
                                    DbVal ->
                                        case proplists:get_value(K, PreviousProps) of 
                                            DbVal ->
                                                %% New value in NewProps, unchanged in DB
                                                [{K,V} | Props];
                                            _PrevVal when is_binary(DbVal) ->
                                                %% Compare with converted to list value
                                                case z_convert:to_list(DbVal) of
                                                    V ->
                                                        Props;
                                                    _X ->
                                                        %% Changed by someone else
                                                        maybe_force_update(K, V, Props, Module, Id, Options, Context)
                                                end;
                                            _PrevVal2 ->
                                                %% Changed by someone else
                                                maybe_force_update(K, V, Props, Module, Id, Options, Context)
                                        end
                                end
                        end, [], NewProps)
    end.


maybe_force_update(K, V, Props, Module, Id, Options, Context) ->
    case lists:member(force_update, Options) of
        true ->
            ?zInfo(io_lib:format("~p: ~p of ~p changed in database, forced update.", [Module, K, Id]), Context),
            z_utils:prop_replace(K, V, Props);
        false ->
            ?zInfo(io_lib:format("~p: ~p of ~p changed in database, not updating.", [Module, K, Id]), Context),
            Props
    end.


manage_predicate_validfor(_Id, [], _Options, _Context) ->
    ok;
manage_predicate_validfor(Id, [{SubjectCat, ObjectCat} | Rest], Options, Context) ->
    F = fun(S, I, C) ->
                case z_db:q("SELECT 1 FROM predicate_category WHERE predicate_id = $1 AND is_subject = $2 AND category_id = $3", [S, I, C], Context) of
                    [{1}] ->
                        ok;
                    _ ->
                        z_db:q("insert into predicate_category (predicate_id, is_subject, category_id) values ($1, $2, $3)", [S, I, C], Context),
                        ok
                end
        end,
    case SubjectCat of
        undefined -> nop;
        _ -> 
            F(Id, true, m_rsc:name_to_id_check(SubjectCat, Context))
    end,
    case ObjectCat of
        undefined -> nop;
        _ ->
            F(Id, false, m_rsc:name_to_id_check(ObjectCat, Context))
    end,
    manage_predicate_validfor(Id, Rest, Options, Context).



map_props(Props, Context) ->
    map_props(Props, Context, []).

map_props([], _Context, Acc) ->
    Acc;
map_props([{Key, Value}|Rest], Context, Acc) ->
    Value2 = map_prop(Value, Context),
    map_props(Rest, Context, [{Key, Value2}|Acc]).


map_prop({file, Filepath}, _Context) ->
    {ok, Txt} = file:read_file(Filepath),
    Txt;
map_prop({to_id, Name}, Context) ->
    case m_rsc:name_to_id(Name, Context) of
             {ok, Id} -> Id;
             _ -> undefined
    end;
map_prop(Value, _Context) ->
    Value.

manage_edge(_Module, {SubjectName, PredicateName, ObjectName}, _Options, Context) ->
    manage_edge(_Module, {SubjectName, PredicateName, ObjectName, []}, _Options, Context);
manage_edge(_Module, {SubjectName, PredicateName, ObjectName, EdgeOptions}, _Options, Context) ->
    Subject = m_rsc:name_to_id(SubjectName, Context),
    Predicate = m_predicate:name_to_id(PredicateName, Context),
    Object = m_rsc:name_to_id(ObjectName, Context),
    case {Subject, Predicate, Object} of
        {{ok, SubjectId}, {ok, PredicateId}, {ok,ObjectId}} ->
            m_edge:insert(SubjectId, PredicateId, ObjectId, EdgeOptions, Context);
        _ ->
            skip %% One part of the triple was MIA
    end.
