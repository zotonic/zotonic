%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% @date 2009-11-08
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

-export([manage/3]).


%% The datamodel manages parts of your datamodel. This includes
%% categories and predicates, but also "default data" like resources.
%%
%% The data model maintains a special property on this installed data,
%% called 'installed_by'. This decides whether it can touch it.
%%

-include_lib("zotonic.hrl").



manage(Module, Datamodel, Context) ->
	AdminContext = z_acl:sudo(Context),
    case proplists:get_value(categories, Datamodel) of
        undefined  -> ok;
        Categories -> [manage_category(Module, Cat, AdminContext) || Cat <- Categories]
    end,
    case proplists:get_value(predicates, Datamodel) of
        undefined -> ok;
        Preds     -> [manage_predicate(Module, Pred, AdminContext) || Pred <- Preds]
    end,
    case proplists:get_value(resources, Datamodel) of
        undefined -> ok;
        Rs        -> [manage_resource(Module, R, AdminContext) || R <- Rs]
    end,
    case proplists:get_value(media, Datamodel) of
        undefined -> ok;
        Ms        -> [manage_medium(Module, Medium, AdminContext) || Medium <- Ms]
    end,
    case proplists:get_value(edges, Datamodel) of
        undefined -> ok;
        Edges     -> [manage_edge(Module, Edge, AdminContext) || Edge <- Edges]
    end,
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
                            {ok};
                        _ ->
                            %% Resource exists but is not installed by us.
                            Msg = io_lib:format("Resource '~p' (~p) exists but is not managed by Zotonic.", [Name, Id]),
                            ?zInfo(Msg, Context),
                            {ok}
                    end;
                {error, {unknown_rsc, _}} ->
                    %% new resource, or old resource

                    %% Check if name was previously inserted.
                    ManagedNames = case m_config:get(Module, datamodel, Context) of
                                       undefined ->
                                           [];
                                       Cfg ->
                                           case proplists:get_value(managed_resources, Cfg) of
                                               undefined ->
                                                   [];
                                               N -> N
                                           end
                                   end,

                    {Result, NewNames} = case lists:member(Name, ManagedNames) of
                                             false ->
                                                 Props1 = [{name, Name}, {category_id, CatId},
                                                           {installed_by, Module}, {managed_props, Props}] ++ Props,
                                                 Props2 = case proplists:get_value(is_published, Props1) of
                                                              undefined -> [{is_published, true} | Props1];
                                                              _ -> Props1
                                                          end,
                                                 Props3 = case proplists:get_value(visible_for, Props2) of
                                                              undefined -> [{visible_for, ?ACL_VIS_PUBLIC} | Props2];
                                                              _ -> Props2
                                                          end,
                                                 ?zInfo(io_lib:format("Creating new resource '~p'", [Name]), Context),
                                                 {m_rsc:insert(Props3, Context), [Name | ManagedNames ]};
                                             true ->
                                                 ?zInfo(io_lib:format("Resource '~p' was deleted", [Name]), Context),
                                                 {{ok}, ManagedNames}
                                         end,
                    m_config:set_prop(Module, datamodel, managed_resources, NewNames, Context),
                    Result
            end;
        {error, _} ->
            Msg = io_lib:format("Resource '~p' could not be handled because the category ~p does not exist.", [Name, Category]),
            ?zWarning(Msg, Context),
            {ok}
    end.


manage_predicate_validfor(_Id, [], _Context) ->
    ok;
manage_predicate_validfor(Id, [{SubjectCat, ObjectCat} | Rest], Context) ->
    SubjectId = m_rsc:name_to_id_check(SubjectCat, Context),
    ObjectId  = m_rsc:name_to_id_check(ObjectCat, Context),

    F = fun(S, I, C) ->
                case z_db:q("SELECT 1 FROM predicate_category WHERE predicate_id = $1 AND is_subject = $2 AND category_id = $3", [S, I, C], Context) of
                    [{1}] ->
                        ok;
                    _ ->
                        z_db:q("insert into predicate_category (predicate_id, is_subject, category_id) values ($1, $2, $3)", [S, I, C], Context),
                        ok
                end
        end,
    F(Id, true, SubjectId),
    F(Id, false, ObjectId),

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


