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

-export([manage/3, reset_deleted/2]).


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



manage(Module, Datamodel, Context) when is_list(Datamodel) ->
    %% Backwards compatibility with old datamodel notation.
    manage(Module,
           #datamodel{categories=proplists:get_value(categories, Datamodel, []),
                      predicates=proplists:get_value(predicates, Datamodel, []),
                      resources=proplists:get_value(resources, Datamodel, []),
                      media=proplists:get_value(media, Datamodel, []),
                      edges=proplists:get_value(edges, Datamodel, [])
                     },
           Context);

manage(Module, Datamodel, Context) ->
    AdminContext = z_acl:sudo(Context),
    [manage_category(Module, Cat, AdminContext) || Cat <- Datamodel#datamodel.categories],
    [manage_predicate(Module, Pred, AdminContext) || Pred <- Datamodel#datamodel.predicates],
    [manage_resource(Module, R, AdminContext) || R <- Datamodel#datamodel.resources],
    [manage_medium(Module, Medium, AdminContext) || Medium <- Datamodel#datamodel.media],
    [manage_edge(Module, Edge, AdminContext) || Edge <- Datamodel#datamodel.edges],
    ok.


manage_medium(Module, {Name, {EmbedService, EmbedCode}, Props}, Context) ->
    case manage_resource(Module, {Name, media, Props}, Context) of
        {ok} ->
            {ok};
        {ok, Id} ->
            MediaProps = [{mime, "text/html-video-embed"}, 
                          {video_embed_service, EmbedService}, 
                          {video_embed_code, EmbedCode}
                         ],
            m_media:replace(Id, MediaProps, Context),
            {ok, Id}
    end;

manage_medium(Module, {Name, Filename, Props}, Context) ->
    case manage_resource(Module, {Name, media, Props}, Context) of
        {ok} ->
            {ok};
        {ok, Id} ->
            m_media:replace_file(Filename, Id, Context),
            {ok, Id}
    end.


manage_category(Module, {Name, ParentCategory, Props}, Context) ->
    case manage_resource(Module, {Name, category, Props}, Context) of
        {ok} ->
            {ok};
        {ok, Id} ->
            case ParentCategory of
                undefined ->
                    {ok, Id};
                _ ->
                    case m_category:name_to_id(ParentCategory, Context) of
                        {ok, PId} ->
                            m_category:move_below(Id, PId, Context),
                            {ok, Id};
                        _ -> throw({error, {nonexisting_parent_category, ParentCategory}})
                    end
            end
    end.


manage_predicate(Module, {Name, Uri, Props, ValidFor}, Context) ->
    manage_predicate(Module, {Name, [{uri,Uri}|Props], ValidFor}, Context);

manage_predicate(Module, {Name, Props, ValidFor}, Context) ->
    case manage_resource(Module, {Name, predicate, Props}, Context) of
        {ok} ->
            {ok};
        {ok, Id} ->
            ok = manage_predicate_validfor(Id, ValidFor, Context),
            {ok, Id}
    end.


manage_resource(Module, {Name, Category, Props0}, Context) ->
    case m_category:name_to_id(Category, Context) of
        {ok, CatId} -> 
            Props = map_props(Props0, Context),
            case m_rsc:name_to_id(Name, Context) of
                {ok, Id} ->
                    case m_rsc:p(Id, installed_by, Context) of
                        Module ->
                            NewProps = update_new_props(Module, Id, Props, Context),
                            m_rsc:update(Id, [{managed_props, z_html:escape_props(Props)} | NewProps], Context),
                            {ok};
                        _ ->
                            %% Resource exists but is not installed by us.
                            ?zInfo(io_lib:format("Resource '~p' (~p) exists but is not managed by ~p.", [Name, Id, Module]), Context),
                            {ok}
                    end;
                {error, {unknown_rsc, _}} ->
                    %% new resource, or old resource
                    Props1 = [{name, Name}, {category_id, CatId},
                              {is_protected, true},
                              {installed_by, Module}, {managed_props, z_html:escape_props(Props)}] ++ Props,
                    Props2 = case proplists:get_value(is_published, Props1) of
                                 undefined -> [{is_published, true} | Props1];
                                 _ -> Props1
                             end,
                    Props3 = case proplists:get_value(visible_for, Props2) of
                                 undefined -> [{visible_for, ?ACL_VIS_PUBLIC} | Props2];
                                 _ -> Props2
                             end,
                    ?zInfo(io_lib:format("Creating new resource '~p'", [Name]), Context),
                    {ok, Id} = m_rsc:insert(Props3, Context),
                    case proplists:get_value(media_url, Props3) of
                        undefined ->
                            nop;
                        Url ->
                            m_media:replace_url(Url, Id, [], Context)
                    end,
                    case proplists:get_value(media_file, Props3) of
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
            {ok}
    end.

update_new_props(Module, Id, NewProps, Context) ->
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
                                                    _ ->
                                                        %% Changed by someone else
                                                        ?zInfo(io_lib:format("~p: ~p of ~p changed in database, not updating.", [Module, K, Id]), Context),
                                                        Props
                                                end;
                                            _PrevVal2 ->

                                                %% Changed by someone else
                                                ?zInfo(io_lib:format("~p: ~p of ~p changed in database, not updating.", [Module, K, Id]), Context),
                                                Props
                                        end
                                end
                        end, [], NewProps)
    end.


manage_predicate_validfor(_Id, [], _Context) ->
    ok;
manage_predicate_validfor(Id, [{SubjectCat, ObjectCat} | Rest], Context) ->
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
    manage_predicate_validfor(Id, Rest, Context).



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


manage_edge(_Module, {SubjectName, PredicateName, ObjectName}, Context) ->
    Subject = m_rsc:name_to_id(SubjectName, Context),
    Predicate = m_predicate:name_to_id(PredicateName, Context),
    Object = m_rsc:name_to_id(ObjectName, Context),
    case {Subject, Predicate, Object} of
        {{ok, SubjectId}, {ok, PredicateId}, {ok,ObjectId}} ->
            m_edge:insert(SubjectId, PredicateId, ObjectId, Context);
        _ ->
            skip %% One part of the triple was MIA
    end.


