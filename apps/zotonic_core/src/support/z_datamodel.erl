%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009-2023 Arjan Scherpenisse
%% @doc Installing parts of the zotonic datamodel. Installs
%% predicates, categories and default resources.

%% Copyright 2009-2023 Arjan Scherpenisse
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

-include("../../include/zotonic.hrl").


%% @doc Reset the state of an imported datamodel, causing all deleted resources to be reimported
-spec reset_deleted(Module, Context) -> ok when
    Module :: atom(),
    Context :: z:context().
reset_deleted(Module, Context) ->
    m_config:delete(Module, datamodel, Context).

%% @doc Install / update a set of named, predefined resources, categories, predicates, media and edges.
-spec manage(Module, Datamodel, Context) -> ok when
    Module :: atom(),
    Datamodel :: #datamodel{},
    Context :: z:context().
manage(Module, Datamodel, Context) ->
    manage(Module, Datamodel, [], Context).

%% @doc Install / update a set of named, predefined resources, categories, predicates, media and edges.
-spec manage(Module, Datamodel, Options, Context) -> ok when
    Module :: atom(),
    Datamodel :: #datamodel{},
    Options :: datamodel_options(),
    Context :: z:context().
manage(Module, Datamodel, Options, Context) ->
    ?LOG_INFO(#{
        text => <<"Installing datamodel for module">>,
        in => zotonic_core,
        module => Module
    }),
    AdminContext = z_acl:sudo(Context),
    jobs:run(manage_module_jobs, fun() ->
        [ manage_category(Module, Cat, Options, AdminContext)   || Cat    <- Datamodel#datamodel.categories ],
        [ manage_predicate(Module, Pred, Options, AdminContext) || Pred   <- Datamodel#datamodel.predicates ],
        [ manage_resource(Module, R, Options, AdminContext)     || R      <- Datamodel#datamodel.resources ],
        [ manage_medium(Module, Medium, Options, AdminContext)  || Medium <- Datamodel#datamodel.media ],
        [ manage_edge(Module, Edge, Options, AdminContext)      || Edge   <- Datamodel#datamodel.edges ],
        [ manage_data(Module, Data, AdminContext)               || Data   <- Datamodel#datamodel.data ]
    end),
    ok.

manage_medium(Module, {Name, Props}, Options, Context) ->
    manage_resource(Module, {Name, media, Props}, Options, Context);

manage_medium(Module, {Name, {EmbedService, EmbedCode}, Props}, Options, Context) when is_list(Props) ->
    manage_medium(Module, {Name, {EmbedService, EmbedCode}, z_props:from_props(Props)}, Options, Context);
manage_medium(Module, {Name, {EmbedService, EmbedCode}, Props}, Options, Context) ->
    case manage_resource(Module, {Name, media, Props}, Options, Context) of
        ok ->
            ok;
        {ok, Id} ->
            MediaProps = #{
                <<"mime">> => <<"text/html-video-embed">>,
                <<"video_embed_service">> => EmbedService,
                <<"video_embed_code">> => EmbedCode
            },
            m_media:replace(Id, MediaProps, Context),
            {ok, Id}
    end;
manage_medium(Module, {Name, Filename, Props}, Options, Context) when is_list(Props) ->
    manage_medium(Module, {Name, Filename, z_props:from_props(Props)}, Options, Context);
manage_medium(Module, {Name, Filename, Props}, Options, Context) ->
    case manage_resource(Module, {Name, media, Props}, Options, Context) of
        ok ->
            Id = m_rsc:rid(Name, Context),
            case m_media:get(Id, Context) of
                undefined ->
                    insert_medium(Id, Filename, Context);
                _Medium ->
                    ok
            end;
        {ok, Id} ->
            insert_medium(Id, Filename, Context)
    end.

insert_medium(Id, Filename, Context) ->
    case is_http_url(Filename) of
        true ->
            z_media_import:update(Id, Filename, Context);
        false ->
            m_media:replace_file(path(Filename, Context), Id, Context)
    end,
    {ok, Id}.


manage_category(Module, {Name, ParentCategory, Props}, Options, Context) when is_list(Props) ->
    manage_category(Module, {Name, ParentCategory, z_props:from_props(Props)}, Options, Context);
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
            end;
        {error, _} = Error ->
            Error
    end.

manage_predicate(Module, {Name, Uri, Props, ValidFor}, Options, Context) when is_list(Props) ->
    manage_predicate(Module, {Name, Uri, z_props:from_props(Props), ValidFor}, Options, Context);
manage_predicate(Module, {Name, Uri, Props, ValidFor}, Options, Context) ->
    manage_predicate(Module, {Name, Props#{ <<"uri">> => Uri }, ValidFor}, Options, Context);

manage_predicate(Module, {Name, Props, ValidFor}, Options, Context) when is_list(Props) ->
    manage_predicate(Module, {Name, z_props:from_props(Props), ValidFor}, Options, Context);
manage_predicate(Module, {Name, Props, ValidFor}, Options, Context) ->
    Category = maps:get(<<"category_id">>, Props, predicate),
    case manage_resource(Module, {Name, Category, maps:remove(<<"category_id">>, Props)}, Options, Context) of
        ok ->
            ok;
        {ok, Id} ->
            ok = manage_predicate_validfor(Id, ValidFor, Options, Context),
            {ok, Id};
        {error, _} = Error ->
            Error
    end.


manage_resource(Module, {Name, Category, Props0}, Options, Context) when is_list(Props0) ->
    manage_resource(Module, {Name, Category, z_props:from_props(Props0)}, Options, Context);
manage_resource(Module, {Name, Category, Props0}, Options, Context) ->
    case m_category:name_to_id(Category, Context) of
        {ok, CatId} ->
            ModuleB = atom_to_binary(Module, utf8),
            Props = map_props(Props0, Context),
            case m_rsc:name_to_id(Name, Context) of
                {ok, Id} ->
                    case m_rsc:p_no_acl(Id, installed_by, Context) of
                        ModuleB ->
                            NewProps = update_new_props(Module, Id, Props, Options, Context),
                            m_rsc_update:update(
                                    Id,
                                    NewProps#{ <<"managed_props">> => z_html:escape_props(Props) },
                                    [{is_import, true}],
                                    Context),
                            ok;
                        OtherModule ->
                            %% Resource exists but is not installed by us.
                            ?LOG_NOTICE(#{
                                text => <<"Resource exists but is managed by another module.">>,
                                in => zotonic_core,
                                name => Name,
                                rsc_id => Id,
                                module => Module,
                                managing_module => OtherModule
                            }),
                            ok
                    end;
                {error, {unknown_rsc, _}} ->
                    %% new resource, or old resource
                    Props1 = Props#{
                        <<"name">> => Name,
                        <<"category_id">> => CatId,
                        <<"installed_by">> => ModuleB,
                        <<"managed_props">> => z_html:escape_props(Props)
                    },
                    Props2 = case maps:get(<<"is_published">>, Props1, undefined) of
                                 undefined -> Props1#{ <<"is_published">> => true };
                                 _ -> Props1
                             end,
                    Props4 = case maps:get(<<"is_protected">>, Props2, undefined) of
                                 undefined -> Props2#{ <<"is_protected">> => true };
                                 _ -> Props2
                             end,
                    Props5 = case maps:get(<<"is_dependent">>, Props4, undefined) of
                                 undefined -> Props4#{ <<"is_dependent">> => false };
                                 _ -> Props4
                             end,
                    ?LOG_NOTICE(#{
                        text => <<"Creating new managed resource.">>,
                        in => zotonic_core,
                        module => Module,
                        category => Category,
                        name => Name
                    }),
                    case m_rsc_update:update(insert_rsc, Props5, [{is_import, true}], Context) of
                        {ok, Id} ->
                            case maps:get(<<"media_url">>, Props5, undefined) of
                                undefined -> nop;
                                <<>> -> nop;
                                Url ->
                                    m_media:replace_url(Url, Id, [], Context)
                            end,
                            case maps:get(<<"media_file">>, Props5, undefined) of
                                undefined -> nop;
                                <<>> -> nop;
                                File ->
                                    m_media:replace_file(path(File, Context), Id, Context)
                            end,
                            {ok, Id};
                        {error, Reason} = Error ->
                            ?LOG_ERROR(#{
                                in => zotonic_core,
                                text => <<"Error creating new managed resource. Not installing.">>,
                                result => error,
                                reason => Reason,
                                module => Module,
                                category => Category,
                                name => Name,
                                page_path => maps:get(<<"page_path">>, Props5, undefined)
                            }),
                            Error
                    end
            end;
        {error, _} ->
            ?LOG_WARNING(#{
                text => <<"Managed resource could not be handled because the category does not exist.">>,
                in => zotonic_core,
                name => Name,
                category => Category,
                module => Module
            }),
            ok
    end.

manage_data(Module, Data, AdminContext) ->
    z_notifier:first(#manage_data{module = Module, props = Data}, AdminContext).

update_new_props(Module, Id, NewProps, Options, Context) ->
    case map_props( m_rsc:p_no_acl(Id, <<"managed_props">>, Context) ) of
        undefined ->
            NewProps;
        PreviousProps ->
            maps:fold(
                fun(K, V, Acc) ->
                    case m_rsc:p_no_acl(Id, K, Context) of
                        V ->
                            %% New value == current value
                            Acc;
                        DbVal ->
                            case maps:get(K, PreviousProps, undefined) of
                                DbVal ->
                                    %% New value in NewProps, unchanged in DB
                                    Acc#{ K => V };
                                _PrevVal when is_binary(DbVal) ->
                                    %% Compare with converted to binary value
                                    case z_convert:to_binary(DbVal) of
                                        V ->
                                            Acc;
                                        _X ->
                                            %% Changed by someone else
                                            maybe_force_update(K, V, Acc, Module, Id, Options, Context)
                                    end;
                                _PrevVal2 ->
                                    %% Changed by someone else
                                    maybe_force_update(K, V, Acc, Module, Id, Options, Context)
                            end
                    end
                end,
                #{},
                NewProps)
    end.

map_props(undefined) ->
    undefined;
map_props(Props) when is_map(Props) ->
    Props;
map_props(Props) when is_list(Props) ->
    z_props:from_props(Props).

maybe_force_update(K, V, Props, Module, Id, Options, _Context) ->
    case proplists:get_value(force_update, Options, false) of
        true ->
            ?LOG_NOTICE(#{
                text => <<"Managed resource property changed in database, updating.">>,
                in => zotonic_core,
                rsc_id => Id,
                property => K,
                module => Module
            }),
            Props#{ K => V };
        false ->
            ?LOG_DEBUG(#{
                text => <<"Managed resource property changed in database, not updating.">>,
                in => zotonic_core,
                rsc_id => Id,
                property => K,
                module => Module
            }),
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
            {ok, SubjectCatId} = m_rsc:name_to_id(SubjectCat, Context),
            F(Id, true, SubjectCatId)
    end,
    case ObjectCat of
        undefined -> nop;
        _ ->
            {ok, ObjectCatId} = m_rsc:name_to_id(ObjectCat, Context),
            F(Id, false, ObjectCatId)
    end,
    manage_predicate_validfor(Id, Rest, Options, Context).



map_props(Props, Context) ->
    maps:map(
        fun(_K, V) ->
            map_prop(V, Context)
        end,
        Props).

map_prop({file, Filename}, Context) ->
    {ok, Data} = file:read_file( path(Filename, Context) ),
    Data;
map_prop({to_id, Name}, Context) ->
    case m_rsc:name_to_id(Name, Context) of
        {ok, Id} -> Id;
        _ -> undefined
    end;
map_prop(Value, _Context) ->
    Value.

path({file, Filename}, _Context) ->
    Filename;
path({priv, Filename}, Context) ->
    filename:join([ z_path:site_dir(Context), "priv", Filename ]);
path({priv, App, Filename}, _Context) when is_atom(App) ->
    filename:join([ code:priv_dir(App), Filename ]);
path(Filename, Context) ->
    filename:join([ z_path:site_dir(Context), "priv", "schema_data", Filename ]).

manage_edge(_Module, {SubjectName, PredicateName, ObjectName}, _Options, Context) ->
    manage_edge(_Module, {SubjectName, PredicateName, ObjectName, []}, _Options, Context);
manage_edge(_Module, {SubjectName, PredicateName, ObjectName, EdgeOptions}, _Options, Context) ->
    Subject = m_rsc:name_to_id(SubjectName, Context),
    Predicate = m_predicate:name_to_id(PredicateName, Context),
    Object = m_rsc:name_to_id(ObjectName, Context),
    case {Subject, Predicate, Object} of
        {{ok, SubjectId}, {ok, PredicateId}, {ok,ObjectId}} ->
            {ok, _} = m_edge:insert(SubjectId, PredicateId, ObjectId, EdgeOptions, Context);
        _ ->
            skip %% One part of the triple was MIA
    end.

is_http_url("http:" ++ _) -> true;
is_http_url("https:" ++ _) -> true;
is_http_url(<<"http:", _/binary>>) -> true;
is_http_url(<<"https:", _/binary>>) -> true;
is_http_url(_) -> false.
