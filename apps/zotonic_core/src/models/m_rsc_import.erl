%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010-2021 Arjan Scherpenisse, Marc Worrell
%% @doc Importing non-authoritative things exported by m_rsc_export into the system.

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

-module(m_rsc_import).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include("../../include/zotonic.hrl").

-export([
    mark_imported/3,

    fetch_preview/2,

    is_imported/2,

    get_import_status/2,
    set_import_status/3,

    import/2,
    import/3,
    import_uri/2,
    import_uri/3,
    import_uri_recursive/3,
    import_uri_recursive_async/3,

    reimport/2,
    reimport/3,
    reimport_recursive/2,
    reimport_recursive_async/2,

    update_medium_uri/3,

    install/1
]).

-export([
    import_referred_ids_task/3
]).

-type option() :: {props_forced, map()}                 % Properties overlayed over the imported properties
                | {props_default, map()}                % Default properties
                | {import_edges, non_neg_integer()}     % Number of edge-redirections to import
                | is_import_deleted                     % Set if deleted resources must be re-imported
                | {is_import_deleted, boolean()}
                | is_authoritative                      % If set then make a local copy, do not save uri
                | {is_authoritative, boolean()}.
-type options() :: [ option() ].

-type import_result() :: {ok, {m_rsc:resource_id(), import_map()}}
                       | {error, term()}.

-type import_map() :: #{ binary() => m_rsc:resource_id() }.

-export_type([
    options/0,
    option/0,
    import_result/0,
    import_map/0
]).


%% @doc Check if a resource has been succesfully imported.
-spec is_imported( m_rsc:resource(), z:context() ) -> boolean().
is_imported( Rsc, Context ) ->
    case m_rsc:rid(Rsc, Context) of
        undefined ->
            false;
        RscId ->
            case z_db:q1("
                select last_import_status
                from rsc_import
                where id = $1",
                [ RscId ],
                Context)
            of
                <<"ok">> -> true;
                _ -> false
            end
    end.


%% @doc Fetch the import status of a resource.
-spec get_import_status( m_rsc:resource(), z:context() ) -> {ok, map()} | {error, term()}.
get_import_status( Rsc, Context ) ->
    case m_rsc:rid(Rsc, Context) of
        undefined ->
            {error, enoent};
        RscId ->
            case z_acl:rsc_visible(RscId, Context) of
                true ->
                    z_db:select(rsc_import, RscId, Context);
                false ->
                    {error, eacces}
            end
    end.

%% @doc Modify the import options of the resource.
-spec set_import_status( m_rsc:resource(), map(), z:context() ) -> {ok, m_rsc:resource_id()} | {error, term()}.
set_import_status(Rsc, Status, Context) when is_map(Status) ->
    case m_rsc:rid(Rsc, Context) of
        undefined ->
            {error, enoent};
        RscId ->
            case z_acl:rsc_editable(RscId, Context) of
                true ->
                    Status1 = maps:fold(
                        fun
                            (K, V, Acc) when is_binary(K) ->
                                Acc#{ K => V };
                            (_, _, Acc) ->
                                Acc
                        end,
                        #{},
                        Status),
                    Status2 = maps:without([ <<"id">>, <<"created">>, <<"props">> ], Status1),
                    case maps:find(<<"user_id">>, Status1) of
                        {ok, UId} ->
                            case z_acl:rsc_editable(UId, Context) of
                                true ->
                                    z_db:update(rsc_import, RscId, Status2, Context);
                                false ->
                                    {error, eacces}
                            end;
                        error ->
                            z_db:update(rsc_import, RscId, Status2, Context)
                    end;
                false ->
                    {error, eacces}
            end
    end.


%% @doc Mark a resource as imported, set the import result.
-spec mark_imported( m_rsc:resource_id(), atom() | binary() | string(), z:context() ) -> ok | {error, enoent}.
mark_imported(RscId, Status, Context) ->
    case z_db:q("
        update rsc_import
        set last_import_date = now(),
            last_import_status = $1
        where id = $2",
        [ z_convert:to_binary(Status), RscId ],
        Context)
    of
        1 -> ok;
        0 -> {error, enoent}
    end.


%% @doc Find or create a placeholder resource for later import of referred ids.
-spec maybe_create_empty( map(), map(), options(), z:context() ) -> {ok, {m_rsc:rescource_id(), map()}} | {error, term()}.
maybe_create_empty(Rsc, ImportedAcc, Options, Context) ->
    case is_imported_resource(Rsc, ImportedAcc, Options, Context) of
        false ->
            case find_category(Rsc, #{}, Context) of
                {ok, Cat} ->
                    Uri = maps:get(<<"uri">>, Rsc),
                    Props = #{
                        <<"category_id">> => Cat,
                        <<"is_published">> => false,
                        <<"title">> => maps:get(<<"title">>, Rsc, undefined),
                        <<"name">> => maps:get(<<"name">>, Rsc, undefined)
                    },
                    Props2 = case proplists:get_value(is_authoritative, Options, false) of
                        true ->
                            Props#{
                                <<"is_authoritative">> => true,
                                <<"uri">> => undefined
                            };
                        false ->
                            Props#{
                                <<"is_authoritative">> => false,
                                <<"uri">> => Uri
                            }
                    end,
                    UpdateOptions = [
                        {is_escape_texts, false},
                        is_import
                    ],
                    InsertResult = z_db:transaction(fun(Ctx) ->
                        case m_rsc:insert(Props2, UpdateOptions, Context) of
                            {ok, NewId} ->
                                Import = #{
                                    <<"id">> => NewId,
                                    <<"uri">> => Uri,
                                    <<"host">> => host(Uri),
                                    <<"user_id">> => z_acl:user(Ctx),
                                    <<"options">> => Options
                                },
                                z_db:insert(rsc_import, Import, Context);
                            {error, _} = Error ->
                                Error
                        end
                    end, Context),
                    case InsertResult of
                        {ok, LocalId} ->
                            ImportedAcc1 = ImportedAcc#{
                                Uri => LocalId
                            },
                            {ok, {LocalId, ImportedAcc1}}
                    end;
                {error, _} = Error  ->
                    % Unknown category, deny access
                    Error
            end;
        {true, RscId} ->
            {ok, {RscId, ImportedAcc}}
    end.


is_imported_resource(Rsc, ImportedAcc, Options, Context) ->
    Uri = uri(Rsc),
    case maps:find(Uri, ImportedAcc) of
        {ok, LocalId} ->
            {true, LocalId};
        error ->
            case m_rsc:rid(Rsc, Context) of
                undefined ->
                    false;
                RId ->
                    case proplists:get_value(is_authoritative, Options, false) of
                        true ->
                            % A local copy was requested but the one present is
                            % not authoritative.
                            case m_rsc:p_no_acl(RId, is_authoritative, Context) of
                                true -> {true, RId};
                                false -> false
                            end;
                        false ->
                            {true, RId}
                    end
            end
    end.


%% @doc Reimport a non-authoritative resource or placeholder using the saved import flags.
-spec reimport_recursive( m_rsc:resource_id(), z:context() ) -> import_result().
reimport_recursive(Id, Context) ->
    case reimport(Id, Context) of
        {ok, {LocalId, RefIds}} ->
            RefIds1 = import_referred_ids(RefIds, #{ LocalId => true }, Context),
            {ok, {LocalId, RefIds1}};
        {error, _} = Error ->
            Error
    end.

%% @doc Reimport a non-authoritative resource or placeholder using the saved import flags, async
%% reimport of all objects.
-spec reimport_recursive_async( m_rsc:resource_id(), z:context() ) -> import_result().
reimport_recursive_async(Id, Context) ->
    case reimport(Id, Context) of
        {ok, {LocalId, RefIds}} ->
            ContextAsync = z_context:prune_for_async(Context),
            sidejob_supervisor:spawn(
                    zotonic_sidejobs,
                    {?MODULE, import_referred_ids_task, [ RefIds, #{ LocalId => true }, ContextAsync ]}),
            {ok, {LocalId, RefIds}};
        {error, _} = Error ->
            Error
    end.

-spec import_referred_ids_task( map(), map(), z:context() ) -> ok.
import_referred_ids_task(RefIds, ImportedIds, Context) ->
    _ = import_referred_ids(RefIds, ImportedIds, Context),
    ok.

%% @doc Recursively import all resources connected to the given resources. Return a list
%% of local resource ids that are imported or referred.
import_referred_ids(RefIds, ImportedIds, Context) ->
    {NewRefIds, NewImportedIds} = maps:fold(
        fun(Uri, LocalId, {ImpAcc, ImpIdsAcc}) ->
            case maps:is_key(LocalId, ImpIdsAcc) of
                true ->
                    {ImpAcc, ImpIdsAcc};
                false ->
                    ImpAcc2 = case is_imported(LocalId, Context) of
                        true ->
                            ImpAcc#{
                                Uri => LocalId
                            };
                        false ->
                            case reimport_1(LocalId, ImpAcc, false, Context) of
                                {ok, {NewLocalId, ImpAcc1}} ->
                                    ImpAcc1#{
                                        Uri => NewLocalId
                                    };
                                {error, _} ->
                                    ImpAcc#{
                                        Uri => LocalId
                                    }
                            end
                    end,
                    {ImpAcc2, ImpIdsAcc#{ LocalId => true }}
            end
        end,
        {RefIds, ImportedIds},
        RefIds),
    OldCount = maps:size(RefIds),
    NewCount = maps:size(NewRefIds),
    case NewCount of
        OldCount -> ok;
        _ -> import_referred_ids(NewRefIds, NewImportedIds, Context)
    end.


% %% @doc Check if a resource is pending an import.
% is_import_pending(LocalId, Context) ->
%     case get_import_status(LocalId, Context) of
%         {ok, #{ <<"last_import_status">> := <<"ok">> }} ->
%             false;
%         _ ->
%             not m_rsc:p(LocalId, is_authoritative, Context)
%     end.


%% @doc Reimport a non-authoritative resource or placeholder using the saved import flags.
-spec reimport( m_rsc:resource_id(), z:context() ) -> import_result().
reimport(Id, Context) ->
    reimport_1(Id, #{}, true, Context).

reimport_1(Id, ImportedAcc, IsForceImport, Context) ->
    case z_db:select(rsc_import, Id, Context) of
        {ok, #{
            <<"options">> := Options,
            <<"uri">> := Uri,
            <<"last_import_status">> := Status
        }} ->
            case IsForceImport
                orelse (
                    not m_rsc:p(Id, is_authoritative, Context)
                    andalso Status =/= <<"ok">>
                )
            of
                true ->
                    case fetch_json(Uri, Context) of
                        {ok, JSON} ->
                            import_json(Id, Uri, JSON, ImportedAcc, Options, Context);
                        {error, _} = Error ->
                            Error
                    end;
                false ->
                    {ok, {Id, ImportedAcc}}
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Reimport a non-authoritative resource or placeholder using new import options.
-spec reimport( m_rsc:resource_id(), options(), z:context() ) -> import_result().
reimport(Id, Options, Context) ->
    case z_convert:to_bool( m_rsc:p(Id, is_authoritative, Context) ) of
        true ->
            {error, authoritative};
        false ->
            Uri = case z_db:select(rsc_import, Id, Context) of
                {ok, #{ <<"uri">> := ImportUri }} when is_binary(ImportUri), ImportUri /= <<>> ->
                    ImportUri;
                _ ->
                    m_rsc:p(Id, uri_raw, Context)
            end,
            case fetch_json(Uri, Context) of
                {ok, JSON} ->
                    import_json(Id, Uri, JSON, #{}, Options, Context);
                {error, _} = Error ->
                    Error
            end
    end.

-spec update_medium_uri( m_rsc:resource_id(), string() | binary(), z:context() ) -> {ok, m_rsc:resource_id()}.
update_medium_uri(LocalId, Uri, Context) ->
    case z_acl:rsc_editable(LocalId, Context) of
        true ->
            case fetch_json(Uri, Context) of
                {ok, JSON} ->
                    maybe_import_medium(LocalId, JSON, Context);
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, eacces}
    end.


fetch_json(undefined, _Context) ->
    {error, uri};
fetch_json(Url, Context) ->
    Site = z_context:site(Context),
    case z_sites_dispatcher:get_site_for_url(Url) of
        {ok, Site} ->
            {error, local};
        _ ->
            Options = [
                {accept, "application/json"},
                {user_agent, "Zotonic"}
            ],
            case z_fetch:fetch(Url, Options, Context) of
                {ok, {_FinalUrl, _Hs, _Size, Body}} ->
                    JSON = jsxrecord:decode(Body),
                    {ok, JSON};
                {error, _} = Error ->
                    lager:warning("Error fetching ~p: ~p", [Url, Error]),
                    Error
            end
    end.


%% @doc Fetch a sanitized version of the resource at the Url. Without edges, mapping of embedded
%% ids etc. This is to be used as a simple and quick preview of the resource at the given Uri.
-spec fetch_preview( string() | binary(), z:context() ) -> {ok, m_rsc:props()} | {error, term()}.
fetch_preview(Url, Context) ->
    case fetch_json(Url, Context) of
        {ok, #{
            <<"status">> := <<"ok">>,
            <<"result">> := #{
                <<"id">> := _RemoteId,
                <<"is_a">> := IsA,
                <<"resource">> := Rsc,
                <<"uri">> := Uri
            } = JSON
        }} when is_map(Rsc) ->
            case is_local_site(Uri, Context) of
                true ->
                    {error, local};
                false ->
                    RId = #{
                        <<"is_a">> => IsA
                    },
                    case find_category(RId, Rsc, Context) of
                        {ok, Category} ->
                            Rsc1 = z_sanitize:escape_props_check(Rsc, Context),
                            Rsc2 = Rsc1#{
                                <<"is_authoritative">> => false,
                                <<"uri">> => z_sanitize:uri(Uri),
                                <<"category_id">> => Category
                            },
                            % The URL might need to be fetched as a data: url, as the remote
                            % resource might have non-anonymous access permissions.
                            Result = #{
                                <<"rsc">> => Rsc2,
                                <<"depiction_url">> => maps:get(<<"depiction_url">>, JSON, undefined)
                            },
                            {ok, Result};
                        {error, _} = Error ->
                            Error
                    end
            end;
        {error, _} = Error ->
            Error
    end.


import_json(Id, _Url, #{ <<"status">> := <<"ok">>, <<"result">> := JSON }, ImportedAcc, Options, Context) when is_map(JSON) ->
    import(Id, JSON, ImportedAcc, Options, Context);
import_json(_Id, Url, #{ <<"status">> := <<"error">> } = JSON, _ImportedAcc, _Options, _Context) ->
    lager:warning("Remote returned error ~p: ~p", [Url, JSON]),
    {error, remote};
import_json(_Id, Url, JSON, _ImportedAcc, _Options, _Context) ->
    lager:warning("JSON with unknown structure ~p: ~p", [Url, JSON]),
    {error, status}.


%% @doc Import a non-authoritative resource from a remote URI using default import options.
-spec import_uri( string() | binary(), z:context() ) -> import_result().
import_uri(Uri, Context) ->
    import_uri(Uri, [], Context).

%% @doc Import a non-authoritative resource from a remote URI.
-spec import_uri( string() | binary(), options(), z:context() ) -> import_result().
import_uri(Uri, Options, Context) ->
    case fetch_json(Uri, Context) of
        {ok, JSON} ->
            import_json(undefined, Uri, JSON, #{}, Options, Context);
        {error, _} = Error ->
            Error
    end.

%% @doc Recursive import of resources.
-spec import_uri_recursive( string() | binary(), options(), z:context() ) -> import_result().
import_uri_recursive(Uri, Options, Context) ->
    case import_uri(Uri, Options, Context) of
        {ok, {LocalId, RefIds}} ->
            RefIds1 = import_referred_ids(RefIds, [], Context),
            {ok, {LocalId, RefIds1}};
        {error, _} = Error ->
            Error
    end.

%% @doc Recursive import of resources, async import of all referred ids.
-spec import_uri_recursive_async( string() | binary(), options(), z:context() ) -> import_result().
import_uri_recursive_async(Uri, Options, Context) ->
    case import_uri(Uri, Options, Context) of
        {ok, {LocalId, RefIds}} ->
            ContextAsync = z_context:prune_for_async(Context),
            sidejob_supervisor:spawn(
                    zotonic_sidejobs,
                    {?MODULE, import_referred_ids_task, [ RefIds, #{ LocalId => true }, ContextAsync ]}),
            {ok, {LocalId, RefIds}};
        {error, _} = Error ->
            Error
    end.

%% @doc Import a resource using default import options.
-spec import( map(), z:context() ) -> import_result().
import(JSON, Context) ->
    import(JSON, [], Context).

%% @doc Import a resource. If the resource already exists then it must be non-authoritative
%%      and have a matching URI. The resource to be updated is looked up by matching either the
%%      URI or the unique name. If the unique name matches then the category of the existing
%%      resource must have an overlap with the category of the imported resource.
-spec import( map(), options(), z:context() ) -> import_result().
import(JSON, Options, Context) ->
    import(undefined, JSON, #{}, Options, Context).

import(OptLocalId, #{
        <<"id">> := _RemoteId,
        <<"resource">> := Rsc,
        <<"uri">> := Uri,
        <<"uri_template">> := UriTemplate
    } = JSON, ImportedAcc, Options, Context) ->

    lager:info("Importing ~s (~p)", [ Uri, OptLocalId ]),

    RemoteRId = #{
        <<"uri">> => Uri,
        <<"name">> => maps:get(<<"name">>, JSON, undefined),
        <<"is_a">> => maps:get(<<"is_a">>, JSON, [])
    },
    case is_local_site(Uri, Context) of
        true ->
            case m_rsc:rid(Uri, Context) of
                undefined -> {error, enoent};
                Id -> {ok, Id}
            end;
        false ->
            case find_category(RemoteRId, Rsc, Context) of
                {ok, Category} ->
                    {Rsc1, ImportedAcc1} = cleanup_map_ids(RemoteRId, Rsc, UriTemplate, ImportedAcc, Options, Context),
                    Rsc2 = Rsc1#{
                        <<"category_id">> => Category
                    },
                    case update_rsc(OptLocalId, RemoteRId, Rsc2, ImportedAcc1, Options, Context) of
                        {ok, LocalId} ->
                            ImportedAcc2 = ImportedAcc1#{
                                Uri => LocalId
                            },
                            _ = maybe_import_medium(LocalId, JSON, Context),
                            ImportedAcc3 = case proplists:get_value(import_edges, Options, 0) of
                                N when is_integer(N), N > 0 ->
                                    EdgeOptions = [
                                        {import_edges, N - 1} | proplists:delete(import_edges, Options)
                                    ],
                                    import_edges(LocalId, JSON, ImportedAcc2, EdgeOptions, Context);
                                _ ->
                                    ImportedAcc2
                            end,
                            case mark_imported(LocalId, ok, Context) of
                                ok ->
                                    ok;
                                {error, enoent} ->
                                    Import = #{
                                        <<"id">> => LocalId,
                                        <<"uri">> => Uri,
                                        <<"host">> => host(Uri),
                                        <<"user_id">> => z_acl:user(Context),
                                        <<"options">> => Options,
                                        <<"last_import_date">> => calendar:universal_time(),
                                        <<"last_import_status">> => <<"ok">>
                                    },
                                    z_db:insert(rsc_import, Import, Context)
                            end,
                            {ok, {LocalId, ImportedAcc3}};
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end
    end;
import(_OptLocalId, JSON, _ImportedAcc, _Options, _Context) ->
    lager:warning("Import of JSON without required fields id, resource, uri and uri_template: ~p", [JSON]),
    {error, status}.


update_rsc(undefined, RemoteRId, Rsc, ImportedAcc, Options, Context) ->
    UpdateOptions = [
        {is_escape_texts, false},
        is_import
    ],
    Uri = maps:get(<<"uri">>, Rsc),
    IsImportDeleted = proplists:get_value(is_import_deleted, Options, false),
    case is_imported_resource(RemoteRId, ImportedAcc, Options, Context) of
        false when IsImportDeleted ->
            m_rsc:insert(Rsc, UpdateOptions, Context);
        false when not IsImportDeleted ->
            case m_rsc_gone:is_gone_uri(Uri, Context) of
                true ->
                    {error, deleted};
                false ->
                    m_rsc:insert(Rsc, UpdateOptions, Context)
            end;
        {true, LocalId} ->
            case not is_imported(LocalId, Context)
                or not m_rsc:p_no_acl(LocalId, is_authoritative, Context)
            of
                true ->
                    m_rsc:update(LocalId, Rsc, UpdateOptions, Context);
                false ->
                    {error, authoritative}
            end
    end;
update_rsc(LocalId, _RemoteRId, Rsc, _ImportedAcc, _Options, Context) when is_integer(LocalId) ->
    UpdateOptions = [
        {is_escape_texts, false},
        is_import
    ],
    m_rsc:update(LocalId, Rsc, UpdateOptions, Context).


cleanup_map_ids(RemoteRId, Rsc, UriTemplate, ImportedAcc, Options, Context) ->
    PropsForced = proplists:get_value(props_forced, Options, #{}),
    PropsDefault = proplists:get_value(props_default, Options, #{}),

    IsAuthCopy = proplists:get_value(is_authoritative, Options, false),

    OptionsWithoutEdges = proplists:delete(import_edges, Options),

    % Remove or map modifier_id, creator_id, etc
    {Rsc1, ImportedAcc1} = maps:fold(
        fun
            % If we make an authoritative copy then the creator/modifier is the current user
            (<<"creator_id">>, _, {Acc, ImpAcc}) when IsAuthCopy ->
                {Acc#{ <<"creator_id">> => z_acl:user(Context) }, ImpAcc};
            (<<"modifier_id">>, _, {Acc, ImpAcc}) when IsAuthCopy ->
                {Acc#{ <<"modifier_id">> => z_acl:user(Context) }, ImpAcc};

            % Be specific about the category id - handled by caller
            (<<"category_id">>, _, {Acc, ImpAcc}) -> {Acc, ImpAcc};
            (<<"category">>, _, {Acc, ImpAcc}) -> {Acc, ImpAcc};

            % Importing into a content-group depends on the import Options
            (<<"content_group_id">>, CGRId, {Acc, ImpAcc}) ->
                case maps:is_key(<<"content_group_id">>, PropsForced) of
                    false ->
                        case find_content_group(CGRId, Context) of
                            {ok, CGId} ->
                                {Acc#{ <<"content_group_id">> => CGId }, ImpAcc};
                            {error, _} ->
                                {Acc, ImpAcc}
                        end;
                    true ->
                        {Acc, ImpAcc}
                end;
            (<<"content_group">>, _, {Acc, ImpAcc}) -> {Acc, ImpAcc};

            % Other ids are mapped to placeholders or local ids
            (<<"menu">>, Menu, {Acc, ImpAcc}) when is_list(Menu) ->
                % map ids in menu to local ids
                {Menu1, ImpAcc1} = map_menu(Menu, UriTemplate, [], ImpAcc, Options, Context),
                {Acc#{ <<"menu">> => Menu1 }, ImpAcc1};
            (<<"blocks">>, Blocks, {Acc, ImpAcc}) when is_list(Blocks) ->
                % map ids in blocks and content to local ids
                {Blocks1, ImpAcc1} = map_blocks(Blocks, UriTemplate, [], ImpAcc, Options, Context),
                {Acc#{ <<"blocks">> => Blocks1 }, ImpAcc1};
            (K, V, {Acc, ImpAcc}) ->
                case m_rsc_export:is_id_prop(K) of
                    true ->
                        case map_id(V, UriTemplate, ImpAcc, OptionsWithoutEdges, Context) of
                            {ok, {LocalId, ImpAcc1}} ->
                                ImpAcc2 = ImpAcc1#{
                                    uri(V) => LocalId
                                },
                                {Acc#{ K => LocalId }, ImpAcc2};
                            {error, _} ->
                                {Acc#{ K => undefined }, ImpAcc}
                        end;
                    false ->
                        {V1, ImpAcc1} = map_html(K, V, UriTemplate, ImpAcc, OptionsWithoutEdges, Context),
                        {Acc#{ K => V1 }, ImpAcc1}
                end
        end,
        {#{}, ImportedAcc},
        Rsc),

    % Set forced and default props
    Rsc2 = maps:merge(Rsc1, PropsForced),
    Rsc3 = maps:merge(PropsDefault, Rsc2),
    Rsc4 = case maps:find(<<"is_authoritative">>, PropsForced) of
        {ok, true} ->
            Rsc3#{
                <<"is_authoritative">> => true,
                <<"uri">> => undefined
            };
        _ when IsAuthCopy ->
            Rsc3#{
                <<"is_authoritative">> => true,
                <<"uri">> => undefined
            };
        _ ->
            Rsc3#{
                <<"uri">> => maps:get(<<"uri">>, RemoteRId),
                <<"is_authoritative">> => false
            }
    end,

    % Ensure that the is_published flag is present, defaults to true
    Rsc5 = case maps:find(<<"is_published">>, Rsc4) of
        {ok, true} -> Rsc4;
        _ -> Rsc4#{ <<"is_published">> => true }
    end,
    {Rsc5, ImportedAcc1}.


%% @doc Map all ids in a menu to local (stub) resources. Skip all tree entries
%% that could not be mapped to local ids.
map_menu([ #rsc_tree{ id = RemoteId, tree = Sub } | Rest ], UriTemplate, Acc, ImpAcc, Options, Context) ->
    case map_id(RemoteId, UriTemplate, ImpAcc, Options, Context) of
        {ok, {LocalId, ImpAcc1}} ->
            {Sub1, ImpAcc2} = map_menu(Sub, UriTemplate, [], ImpAcc1, Options, Context),
            Acc1 = [ #rsc_tree{ id = LocalId, tree = Sub1 } | Acc ],
            map_menu(Rest, UriTemplate, Acc1, ImpAcc2, Options, Context);
        {error, _} ->
            % Skip the tree entry if the remote id could not be mapped
            map_menu(Rest, UriTemplate, Acc, ImpAcc, Options, Context)
    end;
map_menu([ _ | Rest ], UriTemplate, Acc, ImpAcc, Options, Context) ->
    map_menu(Rest, UriTemplate, Acc, ImpAcc, Options, Context);
map_menu([], _UriTemplate, Acc, ImpAcc, _Options, _Context) ->
    {lists:reverse(Acc), ImpAcc}.


%% @doc Map all ids in blocks to local (stub) resources. Remove blocks that can not
%% have their ids mapped.
map_blocks([ B | Rest ], UriTemplate, Acc, ImpAcc, Options, Context) ->
    OptionsWithoutEdges = proplists:delete(import_edges, Options),
    {B1, ImpAcc1} = maps:fold(
        fun(K, V, {BAcc, BImpAcc}) ->
            case m_rsc_export:is_id_prop(K) of
                true ->
                    case map_id(V, UriTemplate, BImpAcc, Options, Context) of
                        {ok, {LocalId, BImpAcc1}} ->
                            {BAcc#{ K => LocalId }, BImpAcc1};
                        {error, _} ->
                            {BAcc#{ K => undefined }, BImpAcc}
                    end;
                false ->
                    {V1, BImpAcc1} = map_html(K, V, UriTemplate, BImpAcc, OptionsWithoutEdges, Context),
                    {BAcc#{ K => V1}, BImpAcc1}
            end
        end,
        {#{}, ImpAcc},
        B),
    map_blocks(Rest, UriTemplate, [ B1 | Acc ], ImpAcc1, Options, Context);
map_blocks([], _UriTemplate, Acc, ImpAcc, _Options, _Context) ->
    {lists:reverse(Acc), ImpAcc}.



%% @doc Map a remote id to a local id, optionally creating a new (stub) resource.
map_id(RemoteId, UriTemplate, ImpAcc, Options, Context) when is_integer(RemoteId) ->
    case is_url(UriTemplate) of
        true ->
            URL = binary:replace(UriTemplate, <<":id">>, z_convert:to_binary(RemoteId)),
            Rsc = #{
                <<"uri">> => URL
            },
            maybe_create_empty(Rsc, ImpAcc, Options, Context);
        false ->
            {error, enoent}
    end;
map_id(Remote, _UriTemplate, ImpAcc, Options, Context) when is_map(Remote) ->
    maybe_create_empty(Remote, ImpAcc, Options, Context);
map_id(Remote, _UriTemplate, ImpAcc, Options, Context) when is_binary(Remote) ->
    case is_url(Remote) of
        true ->
            Rsc = #{
                <<"uri">> => Remote
            },
            maybe_create_empty(Rsc, ImpAcc, Options, Context);
        false ->
            {error, enoent}
    end;
map_id(_, _, _ImpAcc, _Options, _Context) ->
    {error, enoent}.


%% @doc Map texts in html properties.
map_html(Key, Value, UriTemplate, ImportAcc, Options, Context) ->
    case is_html_prop(Key) of
        true ->
            map_html_1(Value, UriTemplate, ImportAcc, Options, Context);
        false ->
            {Value, ImportAcc}
    end.

map_html_1(#trans{ tr = Tr }, UriTemplate, ImportAcc, Options, Context) ->
    {Tr1, AccIds1} = lists:foldr(
        fun({Lang, Text}, {TAcc, TAccIds}) ->
            {Text1, TAccIds2} = map_html_1(Text, UriTemplate, TAccIds, Options, Context),
            {[ {Lang, Text1} | TAcc ], TAccIds2}
        end,
        {[], ImportAcc},
        Tr),
    {#trans{ tr = Tr1 }, AccIds1};
map_html_1(Text, UriTemplate, ImportAcc, Options, Context) when is_binary(Text) ->
    case filter_embedded_media:embedded_media(Text, Context) of
        [] ->
            {Text, ImportAcc};
        EmbeddedIds ->
            {Text1, ImportAcc1} = lists:foldl(
                fun(RemoteId, {TextAcc, ImpAcc}) ->
                    case map_id(RemoteId, UriTemplate, ImpAcc, Options, Context) of
                        {ok, {LocalId, ImpAcc1}} ->
                            From = <<"<!-- z-media ", (integer_to_binary(RemoteId))/binary, " ">>,
                            To = <<"<!-- z-media-local ", (integer_to_binary(LocalId))/binary, " ">>,
                            {binary:replace(TextAcc, From, To, [ global ]), ImpAcc1};
                        {error, _} ->
                            From = <<"<!-- z-media ", (integer_to_binary(RemoteId))/binary, " ">>,
                            To = <<"<!-- z-media-temp 0 ">>,
                            {binary:replace(TextAcc, From, To, [ global ]), ImpAcc}
                    end
                end,
                {Text, ImportAcc},
                EmbeddedIds),
            Text2 = binary:replace(Text1, <<"<!-- z-media-local ">>, <<"<!-- z-media ">>, [ global ]),
            {Text2, ImportAcc1}
    end.

is_url(<<"http:", _/binary>>) -> true;
is_url(<<"https:", _/binary>>) -> true;
is_url(_) -> false.


is_html_prop(<<"body">>) -> true;
is_html_prop(<<"body_", _/binary>>) -> true;
is_html_prop(K) ->
    binary:longest_common_suffix([ K, <<"_html">> ]) =:= 5.


maybe_import_medium(LocalId, #{ <<"medium">> := Medium, <<"medium_url">> := MediaUrl }, Context)
    when is_binary(MediaUrl), is_map(Medium) ->
    % If medium is outdated (compare with created date in medium record)
    %    - download URL
    %    - save into medium, ensure created date has been set (aka copied)
    Options = [
        {is_escape_texts, false},
        is_import,
        no_touch
    ],
    % TODO: add medium created date option (to set equal to imported medium)
    Created = maps:get(<<"created">>, Medium, calendar:universal_time()),
    RemoteMedium = #{
        <<"created">> => Created
    },
    LocalMedium = m_media:get(LocalId, Context),
    case is_newer_medium(RemoteMedium, LocalMedium) of
        true ->
            _ = m_media:replace_url(MediaUrl, LocalId, RemoteMedium, Options, Context);
        false ->
            ok
    end,
    {ok, LocalId};
maybe_import_medium(LocalId, #{ <<"medium">> := Medium }, Context)
    when is_map(Medium) ->
    % Overwrite local medium record with the imported medium record
    % [ sanitize any HTML in the medium record ]
    case z_notifier:first(#media_import_medium{ id = LocalId, medium = Medium }, Context) of
        undefined ->
            lager:info("Resource import dropped medium record for local id ~p", [ LocalId ]),
            {ok, LocalId};
        ok ->
            {ok, LocalId};
        {ok, _} ->
            {ok, LocalId};
        {error, _} = Error ->
            Error
    end;
maybe_import_medium(LocalId, #{}, Context) ->
    % If no medium:
    %    Delete local medium record (if any)
    case m_media:get(LocalId, Context) of
        undefined ->
            {ok, LocalId};
        _LocalMedium ->
            _ = m_media:delete(LocalId, Context),
            {ok, LocalId}
    end.

is_newer_medium(#{ <<"created">> := Remote }, #{ <<"created">> := Local }) when Local < Remote ->
    true;
is_newer_medium(#{ <<"created">> := _ }, undefined) ->
    true;
is_newer_medium(_, _) ->
    false.


% <<"edges">> := #{
%   <<"depiction">> => #{
%       <<"predicate">> => #{
%             <<"id">> => 304,
%             <<"is_a">> => [ meta, predicate ],
%             <<"name">> => <<"depiction">>,
%             <<"title">> => {trans,[{en,<<"Depiction">>}]},
%             <<"uri">> => <<"http://xmlns.com/foaf/0.1/depiction">>
%       },
%       <<"objects">> => [
%             #{
%                 <<"created">> => {{2020,12,23},{15,4,55}},
%                 <<"object_id">> => #{
%                      <<"id">> => 28992,
%                      <<"is_a">> => [media,image],
%                      <<"name">> => undefined,
%                      <<"title">> =>
%                         {trans,[{nl,<<"NL: a.jpg">>},{en,<<"a.jpg">>}]},
%                      <<"uri">> =>
%                          <<"https://learningstone.test:8443/id/28992">>
%                 },
%                 <<"seq">> => 1
%             },
%             ... more objects
%       ]
%   },
%   ... more predicates
% }

%% @doc Import all edges, return a list of newly created objects
import_edges(LocalId, #{ <<"edges">> := Edges }, ImportedAcc, Options, Context) when is_map(Edges) ->
    % Delete all edges with predicates not mentioned in the import
    LocalPredicates = [ z_convert:to_binary(P) || P <- m_edge:object_predicates(LocalId, Context) ],
    ImportPredicates = maps:keys(Edges),

    lists:foreach(
        fun
            (<<"hasusergroup">>) ->
                % Local ACL predicate - keep as is
                ok;
            (P) ->
                % Delete all edges for predicate P
                m_edge:set_sequence(LocalId, P, [], Context)
        end,
        LocalPredicates -- ImportPredicates),

    % Sync predicates present in the import data.
    maps:fold(
        fun
            (Name, #{ <<"predicate">> := Pred, <<"objects">> := Os }, Acc) ->
                case find_predicate(Name, Pred, Context) of
                    {ok, PredId} ->
                        replace_edges(LocalId, PredId, Os, Acc, Options, Context);
                    {error, _} ->
                        Acc
                end;
            (Name, V, Acc) ->
                lager:warning("Unknown import predicate ~p => ~p", [ Name, V]),
                Acc
        end,
        ImportedAcc,
        Edges).

replace_edges(LocalId, PredId, Os, ImportedAcc, Options, Context) ->
    % Keep order of edges
    {ObjectIds, ImportedAcc1} = lists:foldr(
        fun(Edge, {Acc, ImpAcc}) ->
            Object = maps:get(<<"object_id">>, Edge),
            case is_imported_resource(Object, ImpAcc, Options, Context) of
                false ->
                    case maybe_create_empty(Object, ImpAcc, Options, Context) of
                        {ok, {ObjectId, ImpAcc1}} ->
                            {[ ObjectId | Acc ], ImpAcc1};
                        {error, Reason} ->
                            lager:debug("Skipping object ~p: ~p", [ Object, Reason ]),
                            {Acc, ImpAcc}
                    end;
                {true, ObjectId} ->
                    ImpAcc1 = ImpAcc#{
                        uri(Object) => ObjectId
                    },
                    {[ ObjectId | Acc ], ImpAcc1}
            end
        end,
        {[], ImportedAcc},
        Os),
    m_edge:set_sequence(LocalId, PredId, ObjectIds, Context),
    ImportedAcc1.


uri(Uri) when is_binary(Uri) -> Uri;
uri(#{ <<"uri">> := Uri }) -> Uri.

%% @doc Find the local predicate to be imported.
%% For now we only import if we know the predicate.
%% An option could be added to insert the predicate if it is was unknown.
find_predicate(Name, Pred, Context) ->
    PredId = case m_rsc:rid(Name, Context) of
        undefined -> m_rsc:rid(Pred, Context);
        Id -> Id
    end,
    case m_rsc:is_a(PredId, predicate, Context) of
        true ->
            case m_rsc:p_no_acl(PredId, name, Context) of
                <<"hasusergroup">> ->
                    % Do not import user group memberships
                    % TODO: make this configurable.
                    {error, eacces};
                _ ->
                    {ok, PredId}
            end;
        false ->
            {error, enoent}
    end.


%% @doc Find a content group, prefer matching on name before URI.
find_content_group(RId, Context) ->
    Name = maps:get(<<"name">>, RId, undefined),
    CGId = case m_rsc:rid(Name, Context) of
        undefined -> m_rsc:rid(RId, Context);
        Id -> Id
    end,
    case m_rsc:is_a(CGId, content_group, Context) of
        true ->
            {ok, CGId};
        false ->
            {error, enoent}
    end.

%% @doc Find the category to be imported. This tries to map the 'is_a'
%% and the uri of the category. Importing resources into the 'meta' category
%% is not allowed.
-spec find_category( RId::map(),  Rsc::map(), z:context() ) -> {ok, m_rsc:resource_id()} | {error, term()}.
find_category(RId, Rsc, Context) ->
    RscCatId = case maps:get(<<"category_id">>, Rsc, undefined) of
        #{ <<"name">> := Name, <<"uri">> := CatUri } ->
            case m_rsc:rid(CatUri, Context) of
                undefined -> m_rsc:rid(Name, Context);
                UriId -> UriId
            end;
        #{ <<"uri">> := CatUri } ->
            m_rsc:rid(CatUri, Context);
        _ ->
            undefined
    end,
    case m_rsc:is_a(RscCatId, category, Context) of
        true ->
            check_allowed_category(RscCatId, Context);
        false ->
            case maps:get(<<"is_a">>, RId, undefined) of
                IsA when is_list(IsA) ->
                    case first_category(lists:reverse(IsA), Context) of
                        {ok, IsACatId} ->
                            check_allowed_category(IsACatId, Context);
                        error ->
                            {ok, other}
                    end;
                _ ->
                    {ok, other}
            end
    end.

check_allowed_category(CatId, Context) ->
    case m_category:is_a(CatId, meta, Context) of
        true ->
            % Do not import resources in the 'meta' category.
            % TODO: make this configurable.
            {error, eacces};
        false ->
            {ok, CatId}
    end.


first_category([], _Context) ->
    error;
first_category([ R | Rs ], Context) ->
    case m_category:name_to_id(R, Context) of
        {ok, CatId} ->
            {ok, CatId};
        {error, _} ->
            first_category(Rs, Context)
    end.

%% @doc Return the host part of an URI
-spec host( binary() | string() ) -> binary().
host(Uri) ->
    #{
        host := Host
    } = uri_string:parse(Uri),
    z_convert:to_binary(Host).


%% @doc Check if the site is the local site
is_local_site(Uri, Context ) ->
    Site = z_context:site(Context),
    case z_sites_dispatcher:get_site_for_url(Uri) of
        {ok, Site} -> true;
        {ok, _} -> false;
        undefined -> false
    end.

% Install the datamodel for managing resource imports, especially the rights and options for the imports.
-spec install( z:context() ) -> ok.
install(Context) ->
    case z_db:table_exists(rsc_import, Context) of
        true ->
            ok;
        false ->
            [] = z_db:q("
                create table rsc_import (
                    id int not null,
                    user_id int,
                    host character varying(128) not null,
                    uri character varying(2048) not null,
                    props bytea,
                    created timestamp with time zone not null default now(),
                    next_import_check timestamp with time zone,
                    last_import_check timestamp with time zone,
                    last_import_date timestamp with time zone,
                    last_import_status character varying(20) not null default '',

                    constraint rsc_import_pkey primary key (id),
                    constraint fk_rsc_import_id foreign key (id)
                        references rsc(id)
                        on delete cascade on update cascade,
                    constraint fk_rsc_import_user_id foreign key (user_id)
                        references rsc(id)
                        on delete set null on update cascade
                )",
                Context),
            Indices = [
                {"fki_rsc_import_user_id", "user_id"},
                {"import_rsc_uri", "uri"},
                {"import_rsc_host", "host"},
                {"import_rsc_created_key", "created"},
                {"import_rsc_last_import_check_key", "last_import_check"},
                {"import_rsc_next_import_check_key", "next_import_check"}
            ],
            [ z_db:q("create index "++Name++" on rsc_import ("++Cols++")", Context) || {Name, Cols} <- Indices ],
            z_db:flush(Context)
    end.

