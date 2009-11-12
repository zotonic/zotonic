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


-include_lib("zotonic.hrl").



manage(Module, Datamodel, Context) ->
    CatNames = case proplists:get_value(categories, Datamodel) of
                   undefined ->
                       [];
                   Categories ->
                       {ok, N} = manage_categories(Module, Categories, Context),
                       N
               end,
    PredNames = case proplists:get_value(predicates, Datamodel) of
                    undefined ->
                        [];
                    Preds ->
                        {ok, N2} = manage_predicates(Module, Preds, Context),
                        N2
                end,
    ?DEBUG(CatNames ++ PredNames),
    ok.


manage_categories(Module, Cats, Context) ->
    manage_categories(Module, Cats, Context, []).

manage_categories(_Module, [] , _C, Acc) ->
    {ok, Acc};
manage_categories(Module, [Category|Rest], Context, Acc) ->
    case manage_category(Module, Category, Context) of
        {ok} ->
            manage_categories(Module, Rest, Context, Acc);
        {ok, Id} ->
            manage_categories(Module, Rest, Context, [Id|Acc])
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



manage_predicates(Module, Cats, Context) ->
    manage_predicates(Module, Cats, Context, []).

manage_predicates(_Module, [] , _C, Acc) ->
    {ok, Acc};
manage_predicates(Module, [Predicate|Rest], Context, Acc) ->
    case manage_predicate(Module, Predicate, Context) of
        {ok} ->
            manage_predicates(Module, Rest, Context, Acc);
        {ok, Id} ->
            manage_predicates(Module, Rest, Context, [Id|Acc])
    end.

manage_predicate(Module, {Name, Props, ValidFor}, Context) ->
    case manage_resource(Module, {Name, predicate, Props}, Context) of
        {ok} ->
            {ok};
        {ok, Id} ->
            ok = manage_predicate_validfor(Id, ValidFor, Context),
            {ok, Id}
    end.



manage_resource(Module, {Name, Category, Props}, Context) ->

    CatId = case m_category:name_to_id(Category, Context) of
                {ok, CId} -> CId;
                _ -> throw({error, {nonexisting_category, Category}})
            end,

    case m_rsc:name_to_id(Name, Context) of
        {ok, Id} ->
            case m_rsc:p(Id, installed_by, Context) of
                Module ->
                    %% Resource exists and has been installed by us.
                    %% FIXME Check if it needs updating!
                    ?DEBUG("Updating"),
                    ?DEBUG(Name),
                    m_rsc:update(Id, Props, Context),
                    {ok, Id};
                _ ->
                    %% Resource exists but is not installed by us!
                    ?DEBUG("manage_resource: resource exists but is not managed by us."),
                    ?DEBUG(Id),
                    ?DEBUG(Name),
                    {ok}
            end;
        {error, {unknown_rsc, _}} ->
            %% new resource
            Props1 = [{name, Name}, {group_id, m_group:name_to_id_check(admins, Context)}, {category_id, CatId},
                      {installed_by, Module}] ++ Props,
            Props2 = case proplists:get_value(is_published, Props1) of
                         undefined ->
                             [{is_published, true} | Props1];
                         _ -> Props1
                     end,
            ?DEBUG("New resource"),
            ?DEBUG(Props2),
            m_rsc:insert(Props2, Context)
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
    
