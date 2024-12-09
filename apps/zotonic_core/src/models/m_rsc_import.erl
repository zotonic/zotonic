%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010-2024 Arjan Scherpenisse, Marc Worrell
%% @doc Importing non-authoritative things exported by m_rsc_export into the system.
%% @end

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

-behaviour(zotonic_model).

-include("../../include/zotonic.hrl").

-export([
    m_get/3,

    mark_imported/3,

    fetch_preview/2,

    is_imported/2,

    get_import_status/2,
    set_import_status/3,

    import/2,
    import/3,
    import/4,
    import_uri/2,
    import_uri/3,
    import_uri_recursive/3,
    import_uri_recursive_async/3,

    reimport/2,
    reimport/4,
    reimport_recursive/2,
    reimport_recursive/4,
    reimport_recursive_async/2,

    update_medium_uri/4,

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
                | {is_authoritative, boolean()}
                | {allow_category, [ binary() ]}
                | {allow_predicate, [ binary() ]}
                | {deny_category, [ binary() ]}
                | {deny_predicate, [ binary() ]}
                | {fetch_options, z_url_fetch:options()}
                | {uri_template, binary()}
                | {is_forced_update, boolean()}.               % Set to true on forced medium imports
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


-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"fetch_raw">> | Rest ], #{ payload := Uri }, Context) ->
    case z_auth:is_auth(Context) of
        true ->
            case fetch_raw(Uri, Context) of
                {ok, JSON} ->
                    {ok, {JSON, Rest}};
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, eacces}
    end.


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
-spec maybe_create_empty( map(), map(), options(), z:context() ) -> {ok, {m_rsc:resource_id(), map()}} | {error, term()}.
maybe_create_empty(Rsc, ImportedAcc, Options, Context) ->
    case is_known_resource(Rsc, ImportedAcc, Options, Context) of
        false ->
            Uri = maps:get(<<"uri">>, Rsc),
            case find_allowed_category(Rsc, #{}, Options, Context) of
                {ok, Cat} ->
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
                            {ok, {LocalId, ImportedAcc1}};
                        {error, duplicate_page_path} ->
                            PagePath = unique_page_path(undefined, maps:get(<<"page_path">>, Rsc), Context ),
                            ?LOG_WARNING(#{
                                text => <<"Import of duplicate page_path">>,
                                in => zotonic_core,
                                result => error,
                                reason => duplicate_page_path,
                                uri => Uri,
                                page_path => PagePath
                            }),
                            Rsc1 = Rsc#{ <<"page_path">> => PagePath },
                            maybe_create_empty(Rsc1, ImportedAcc, Options, Context);
                        {error, duplicate_name} ->
                            Name = unique_name(undefined, maps:get(<<"name">>, Rsc), Context ),
                            ?LOG_WARNING(#{
                                text => <<"Import of duplicate name">>,
                                in => zotonic_core,
                                result => error,
                                reason => duplicate_name,
                                uri => Uri,
                                old_name => maps:get(<<"name">>, Props2, undefined),
                                new_name => Name
                            }),
                            Rsc1 = Rsc#{ <<"name">> => Name },
                            maybe_create_empty(Rsc1, ImportedAcc, Options, Context);
                        {error, Reason} = Error ->
                            ?LOG_NOTICE(#{
                                text => <<"Not importing menu entry from remote">>,
                                in => zotonic_core,
                                result => error,
                                reason => Reason,
                                uri => Uri,
                                category => Cat
                            }),
                            Error
                    end;
                {error, Reason} = Error  ->
                    % Unknown category, deny access
                    ?LOG_INFO(#{
                        text => <<"Not importing menu entry from remote, category disallowed">>,
                        in => zotonic_core,
                        result => error,
                        reason => Reason,
                        uri => Uri,
                        rsc => Rsc
                    }),
                    Error
            end;
        {true, RscId} ->
            {ok, {RscId, ImportedAcc}}
    end.

is_known_resource(Rsc, ImportedAcc, Options, Context) ->
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
                            case m_rsc:p_no_acl(RId, is_authoritative, Context) of
                                true ->
                                    {true, RId};
                                false ->
                                    % A local authoritative copy was requested but the one
                                    % present is not authoritative - in this case we will
                                    % force a new copy that is authoritative without changing
                                    % the current non-authoritative resource.
                                    false
                            end;
                        false ->
                            {true, RId}
                    end
            end
    end.


%% @doc Reimport a non-authoritative resource or placeholder using the saved import flags.
-spec reimport_recursive( m_rsc:resource_id(), z:context() ) -> import_result().
reimport_recursive(Id, Context) ->
    reimport_recursive(Id, #{}, saved, Context).

%% @doc Reimport a non-authoritative resource or placeholder using the saved import flags.
-spec reimport_recursive( m_rsc:resource_id(), map(), options() | saved, z:context() ) -> import_result().
reimport_recursive(Id, RefIds, Options, Context) ->
    case reimport(Id, RefIds, Options, Context) of
        {ok, {LocalId, RefIds1}} ->
            Imported = maps:fold(
                fun(_Uri, LId, ImpAcc) ->
                    ImpAcc#{ LId => true }
                end,
                #{ LocalId => true },
                RefIds),
            RefIds2 = import_referred_ids(RefIds1, Imported, Context),
            {ok, {LocalId, RefIds2}};
        {error, _} = Error ->
            Error
    end.

%% @doc Reimport a non-authoritative resource or placeholder using the saved import flags, async
%% reimport of all objects.
-spec reimport_recursive_async( m_rsc:resource_id(), z:context() ) -> import_result().
reimport_recursive_async(Id, Context) ->
    case reimport(Id, Context) of
        {ok, {LocalId, RefIds}} ->
            Args = [ RefIds, #{ LocalId => true } ],
            case z_sidejob:start(?MODULE, import_referred_ids_task, Args, Context) of
                {ok, _} ->
                    {ok, {LocalId, RefIds}};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

-spec import_referred_ids_task( map(), map(), z:context() ) -> ok.
import_referred_ids_task(RefIds, ImportedIds, Context) ->
    _ = import_referred_ids(RefIds, ImportedIds, Context),
    ok.

%% @doc Recursively import all resources connected to the given resources. Return a map
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
        OldCount -> NewRefIds;
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
                    orelse Status =/= <<"ok">>
                )
            of
                true ->
                    case fetch_json(Uri, Context) of
                        {ok, {RscId, ImportMap}} when is_integer(RscId) ->
                            {ok, {RscId, maps:merge(ImportMap, ImportedAcc)}};
                        {ok, JSON} ->
                            import_data(Id, Uri, JSON, ImportedAcc, Options, Context);
                        {error, _} = Error ->
                            Error
                    end;
                false ->
                    {ok, {Id, ImportedAcc}}
            end;
        {error, enoent} ->
            reimport_nonauth(Id, ImportedAcc, Context);
        {error, _} = Error ->
            Error
    end.

reimport_nonauth(Id, ImportedAcc, Context) ->
    case m_rsc:p_no_acl(Id, is_authoritative, Context) of
        false ->
            case m_rsc:p_no_acl(Id, uri_raw, Context) of
                <<>> ->
                    {error, uri};
                Uri ->
                    case fetch_json(Uri, Context) of
                        {ok, {RscId, ImportMap}} when is_integer(RscId) ->
                            {ok, {RscId, maps:merge(ImportMap, ImportedAcc)}};
                        {ok, JSON} ->
                            import_data(Id, Uri, JSON, ImportedAcc, [], Context);
                        {error, _} = Error ->
                            Error
                    end
            end;
        true ->
            {error, authoritative}
    end.

%% @doc Reimport a non-authoritative resource or placeholder using new import options.
-spec reimport( m_rsc:resource_id(), map(), options() | saved, z:context() ) -> import_result().
reimport(Id, RefIds, Options, Context) ->
    {Uri, Options1} = case z_db:select(rsc_import, Id, Context) of
        {ok, #{
            <<"uri">> := ImportUri,
            <<"options">> := SavedOptions
        }} when Options =:= saved ->
            {ImportUri, SavedOptions};
        {ok, #{
            <<"uri">> := ImportUri
        }} ->
            {ImportUri, Options};
        {error, _} ->
            m_rsc:p(Id, uri_raw, Context)
    end,
    case fetch_json(Uri, Context) of
        {ok, {RscId, ImportMap}} when is_integer(RscId) ->
            {ok, {RscId, maps:merge(ImportMap, RefIds)}};
        {ok, JSON} ->
            import_data(Id, Uri, JSON, RefIds, Options1, Context);
        {error, _} = Error ->
            Error
    end.


-spec update_medium_uri( m_rsc:resource_id(), string() | binary(), options(), z:context() ) -> {ok, m_rsc:resource_id()}.
update_medium_uri(LocalId, Uri, Options, Context) ->
    case z_acl:rsc_editable(LocalId, Context) of
        true ->
            case fetch_json(Uri, Context) of
                {ok, {RscId, ImportMap}} when is_integer(RscId) ->
                    {ok, {RscId, ImportMap}};
                {ok, #{ <<"result">> := JSON, <<"status">> := <<"ok">> }} ->
                    maybe_import_medium(LocalId, JSON, Options, Context);
                {ok, JSON} ->
                    maybe_import_medium(LocalId, JSON, Options, Context);
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, eacces}
    end.


fetch_json(undefined, _Context) ->
    {error, uri};
fetch_json(Uri, Context) ->
    Site = z_context:site(Context),
    case z_sites_dispatcher:get_site_for_url(Uri) of
        {ok, Site} ->
            {error, local};
        _ ->
            % Check other modules if they can return the JSON for the uri.
            case z_notifier:first(#rsc_import_fetch{ uri = Uri }, Context) of
                {ok, #{
                    <<"uri">> := U,
                    <<"resource">> := Rsc
                }} = Data when is_binary(U), is_map(Rsc) ->
                    {ok, #{
                        <<"status">> => <<"ok">>,
                        <<"result">> => Data
                    }};
                {ok, RscId} when is_integer(RscId) ->
                    {ok, {RscId, #{ Uri => RscId }}};
                {ok, {RscId, ImportMap}} when is_integer(RscId) ->
                    {ok, {RscId, ImportMap#{ Uri => RscId }}};
                {ok, Data} when is_map(Data); is_list(Data) ->
                    {ok, Data};
                {error, Reason} = Error ->
                    ?LOG_WARNING(#{
                        text => <<"Error fetching resource">>,
                        in => zotonic_core,
                        result => error,
                        reason => Reason,
                        uri => Uri
                    }),
                    Error;
                undefined ->
                    Options = [
                        {accept, "application/json"},
                        {user_agent, "Zotonic"}
                    ],
                    case z_fetch:fetch(Uri, Options, Context) of
                        {ok, {_FinalUrl, _Hs, _Size, Body}} ->
                            JSON = jsxrecord:decode(Body),
                            {ok, JSON};
                        {error, Reason} = Error ->
                            ?LOG_WARNING(#{
                                text => <<"Error fetching resource for import">>,
                                in => zotonic_core,
                                result => error,
                                reason => Reason,
                                uri => Uri
                            }),
                            Error
                    end
            end
    end.


%% @doc Fetch a raw version of the resource at the Url. No sanitization.
-spec fetch_raw(Uri :: binary() | map(), z:context()) -> {ok, map()} | {error, term()}.
fetch_raw(Uri, Context) when is_binary(Uri) ->
    case fetch_json(Uri, Context) of
        {ok, #{
            <<"status">> := <<"ok">>,
            <<"result">> := #{
                <<"id">> := _RemoteId,
                <<"is_a">> := _IsA,
                <<"resource">> := Rsc,
                <<"uri">> := _Uri
            } = Result
        }} when is_map(Rsc) ->
            {ok, Result};
        {ok, _} ->
            {error, format};
        {error, _} = Error ->
            Error
    end;
fetch_raw(#{ <<"uri">> := Uri }, Context) ->
    fetch_raw(Uri, Context);
fetch_raw(_, _Context) ->
    {error, enoent}.


%% @doc Fetch a sanitized version of the resource at the Url. Without edges, mapping of embedded
%% ids etc. This is to be used as a simple and quick preview of the resource at the given Uri.
-spec fetch_preview( string() | binary(), z:context() ) -> {ok, m_rsc:props()} | {error, term()}.
fetch_preview(Url, Context) ->
    case fetch_raw(Url, Context) of
        {ok, #{
            <<"is_a">> := IsA,
            <<"resource">> := Rsc,
            <<"uri">> := Uri
        } = JSON} when is_map(Rsc) ->
            case is_local_site(Uri, Context) of
                true ->
                    {error, local};
                false ->
                    RId = #{
                        <<"is_a">> => IsA
                    },
                    Category = find_category(RId, Rsc, Context),
                    Rsc1 = z_sanitize:escape_props_check(Rsc, Context),
                    Rsc2 = Rsc1#{
                        <<"is_authoritative">> => false,
                        <<"uri">> => z_sanitize:uri(Uri),
                        <<"category_id">> => Category
                    },
                    % The URL might need to be fetched as a data: url, as the remote
                    % resource might have non-anonymous access permissions.
                    Result = #{
                        <<"resource">> => Rsc2,
                        <<"depiction_url">> => maps:get(<<"depiction_url">>, JSON, undefined)
                    },
                    {ok, Result}
            end;
        {ok, _} ->
            {error, format};
        {error, _} = Error ->
            Error
    end.


import_data(Id, _Url, #{ <<"status">> := <<"ok">>, <<"result">> := JSON }, ImportedAcc, Options, Context) when is_map(JSON) ->
    import(Id, JSON, ImportedAcc, Options, Context);
import_data(_Id, Url, #{ <<"status">> := <<"error">> } = JSON, _ImportedAcc, _Options, _Context) ->
    ?LOG_WARNING(#{
        text => <<"Remote returned error on import">>,
        in => zotonic_core,
        uri => Url,
        json => JSON
    }),
    {error, remote};
import_data(_Id, _Url, #{ <<"rdf_triples">> := [] }, _ImportedAcc, _Options, _Context) ->
    {error, nodoc};
import_data(Id, Url, #{ <<"rdf_triples">> := _ } = Data, ImportedAcc, Options, Context) ->
    import_rdf(Id, Url, Data, ImportedAcc, Options, Context);
import_data(_Id, Url, JSON, _ImportedAcc, _Options, _Context) ->
    ?LOG_WARNING(#{
        text => <<"Import of JSON with unknown structure">>,
        in => zotonic_core,
        uri => Url,
        json => JSON
    }),
    {error, status}.

import_rdf(OptLocalId, OptUri, #{ <<"rdf_triples">> := Triples } = Data, ImportedAcc, Options, Context) ->
    DataUri = maps:get(<<"uri">>, Data, OptUri),
    Docs = zotonic_rdf:triples_to_docs(Triples),
    case lists:map(fun zotonic_rdf:compact/1, Docs) of
        [] ->
            {error, enoent};
        CDocs ->
            UriDocs = lists:filter(fun(Doc) -> maps:get(<<"@id">>, Doc) =:= DataUri end, CDocs),
            {MainDoc, OtherDocs} = case UriDocs of
                [D] -> {D, CDocs -- [D]};
                [] -> {hd(CDocs), tl(CDocs)}
            end,
            MainUri = maps:get(<<"@id">>, MainDoc),
            case import_doc(OptLocalId, MainDoc, ImportedAcc, Options, Context) of
                {ok, {LocalId, Acc1}} ->
                    ImportedAcc1 = Acc1#{ MainUri => LocalId },
                    ImportedAcc2 = lists:foldl(
                        fun(D, ImpAcc) ->
                            case import_doc(undefined, D, ImpAcc, Options, Context) of
                                {ok, {Id, ImpAcc1}} ->
                                    DUri = maps:get(<<"@id">>, D),
                                    ImpAcc1#{ DUri => Id };
                                {error, _} ->
                                    ImpAcc
                            end
                        end,
                        ImportedAcc1,
                        OtherDocs),
                    {ok, {maps:get(MainUri, ImportedAcc2, undefined), ImportedAcc2}};
                {error, _} = Error ->
                    Error
            end
    end.

import_doc(OptLocalId, Doc, ImpAcc, Options, Context) ->
    case z_rdf_props:extract_resource(Doc, Context) of
        {ok, Rsc} when is_map(Rsc) ->
            Uri = maps:get(<<"uri">>, Rsc),
            OptId1 = case OptLocalId of
                undefined -> m_rsc:rid(Uri, Context);
                _ -> OptLocalId
            end,
            import(OptId1, Rsc, ImpAcc, Options, Context);
        {error, _} = Error ->
            Error
    end.

%% @doc Import a non-authoritative resource from a remote URI using default import options.
-spec import_uri( string() | binary(), z:context() ) -> import_result().
import_uri(Uri, Context) ->
    import_uri(Uri, [], Context).

%% @doc Import a non-authoritative resource from a remote URI.
-spec import_uri( string() | binary(), options(), z:context() ) -> import_result().
import_uri(Uri, Options, Context) ->
    case fetch_json(Uri, Context) of
        {ok, JSON} ->
            import_data(undefined, Uri, JSON, #{}, Options, Context);
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
            Args = [ RefIds, #{ LocalId => true } ],
            case z_sidejob:start(?MODULE, import_referred_ids_task, Args, Context) of
                {ok, _} ->
                    {ok, {LocalId, RefIds}};
                {error, _} = Error ->
                    Error
            end;
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

%% @doc Import a resource. Overwrites the given resource.
-spec import( OptLocalId, JSON, options(), z:context() ) -> import_result() when
    OptLocalId :: m_rsc:resource() | undefined,
    JSON :: map().
import(OptLocalId, JSON, Options, Context) ->
    import(OptLocalId, JSON, #{}, Options, Context).

import(OptLocalId, #{ <<"status">> := <<"ok">>, <<"result">> := JSON }, ImportedAcc, Options, Context) ->
    import(OptLocalId, JSON, ImportedAcc, Options, Context);
import(OptLocalId, #{
        <<"resource">> := Rsc,
        <<"uri">> := Uri
    } = JSON, ImportedAcc, Options, Context) ->

    ?LOG_INFO(#{
        text => <<"Importing resource">>,
        in => zotonic_core,
        uri => Uri,
        local_id => OptLocalId
    }),
    RemoteRId = #{
        <<"uri">> => Uri,
        <<"is_a">> => maps:get(<<"is_a">>, JSON, [])
    },
    case is_local_site(Uri, Context) of
        true ->
            case m_rsc:rid(Uri, Context) of
                undefined -> {error, enoent};
                Id -> {ok, {Id, ImportedAcc}}
            end;
        false ->
            case find_allowed_category(RemoteRId, Rsc, Options, Context) of
                {ok, Category} ->
                    UriTemplate = maps:get(<<"uri_template">>, JSON, proplists:get_value(uri_template, Options)),
                    {Rsc1, ImportedAcc1} = cleanup_map_ids(RemoteRId, Rsc, UriTemplate, ImportedAcc, Options, Context),
                    Rsc2 = Rsc1#{
                        <<"category_id">> => Category
                    },
                    case update_rsc(OptLocalId, RemoteRId, Rsc2, ImportedAcc1, Options, Context) of
                        {ok, LocalId} ->
                            ImportedAcc2 = ImportedAcc1#{
                                Uri => LocalId
                            },
                            _ = maybe_import_medium(LocalId, JSON, Options, Context),
                            ImportedAcc3 = case proplists:get_value(import_edges, Options, 0) of
                                N when is_integer(N), N > 0 ->
                                    PropsForced = proplists:get_value(props_forced, Options, #{}),
                                    EdgeOptions = [
                                        {import_edges, N - 1},
                                        {props_forced, maps:remove(<<"category_id">>, PropsForced)}
                                        | proplists:delete(import_edges,
                                            proplists:delete(props_forced, Options))
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
                        {error, Reason} = Error ->
                            ?LOG_INFO(#{
                                text => <<"Importing resource returned error">>,
                                in => zotonic_core,
                                result => error,
                                reason => Reason,
                                uri => Uri,
                                rsc_id => OptLocalId
                            }),
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end
    end;
import(OptLocalId, #{ <<"rdf_triples">> := _ } = Data, ImportedAcc, Options, Context) ->
    Uri = maps:get(<<"uri">>, Data, m_rsc:p(OptLocalId, uri_raw, Context)),
    import_rdf(OptLocalId, Uri, Data, ImportedAcc, Options, Context);
import(_OptLocalId, JSON, _ImportedAcc, _Options, _Context) ->
    ?LOG_WARNING(#{
        text => <<"Import of JSON without required fields resource and uri">>,
        in => zotonic_core,
        json => JSON
    }),
    {error, status}.


update_rsc(OptLocalId, RemoteRId, Rsc, ImportedAcc, Options, Context) ->
    case update_rsc_1(OptLocalId, RemoteRId, Rsc, ImportedAcc, Options, Context) of
        {ok, _} = OK ->
            OK;
        {error, duplicate_page_path} ->
            OldPath = maps:get(<<"page_path">>, Rsc),
            PagePath = unique_page_path(OptLocalId, OldPath, Context),
            ?LOG_WARNING(#{
                text => <<"Import of duplicate page_path">>,
                in => zotonic_core,
                result => error,
                reason => duplicate_page_path,
                rsc_id => OptLocalId,
                remote_id => RemoteRId,
                old_page_path => OldPath,
                new_page_path => PagePath,
                uri => maps:get(<<"uri">>, Rsc, undefined)
            }),
            Rsc1 = Rsc#{ <<"page_path">> => PagePath },
            update_rsc(OptLocalId, RemoteRId, Rsc1, ImportedAcc, Options, Context);
        {error, duplicate_name} ->
            OldName = maps:get(<<"name">>, Rsc),
            Name = unique_name(OptLocalId, OldName, Context),
            ?LOG_WARNING(#{
                text => <<"Import of duplicate name">>,
                in => zotonic_core,
                result => error,
                reason => duplicate_name,
                rsc_id => OptLocalId,
                remote_id => RemoteRId,
                old_name => OldName,
                new_name => Name,
                uri => maps:get(<<"uri">>, Rsc, undefined)
            }),
            Rsc1 = Rsc#{ <<"name">> => Name },
            update_rsc(OptLocalId, RemoteRId, Rsc1, ImportedAcc, Options, Context);
        {error, _} = Error ->
            Error
    end.

update_rsc_1(undefined, RemoteRId, Rsc, ImportedAcc, Options, Context) ->
    RscLang = ensure_language_prop(Rsc),
    UpdateOptions = [
        {is_escape_texts, false},
        is_import
    ],
    Uri = maps:get(<<"uri">>, RscLang),
    IsImportDeleted = proplists:get_value(is_import_deleted, Options, false),
    case is_known_resource(RemoteRId, ImportedAcc, Options, Context) of
        false when IsImportDeleted ->
            m_rsc:insert(Rsc, UpdateOptions, Context);
        false when not IsImportDeleted ->
            case m_rsc_gone:is_gone_uri(Uri, Context) of
                true ->
                    {error, deleted};
                false ->
                    m_rsc:insert(RscLang, UpdateOptions, Context)
            end;
        {true, LocalId} ->
            case m_rsc:p_no_acl(LocalId, is_authoritative, Context) of
                true ->
                    {error, authoritative};
                false ->
                    m_rsc:update(LocalId, RscLang, UpdateOptions, Context)
            end
    end;
update_rsc_1(LocalId, _RemoteRId, Rsc, _ImportedAcc, _Options, Context) when is_integer(LocalId) ->
    RscLang = ensure_language_prop(Rsc),
    UpdateOptions = [
        {is_escape_texts, false},
        is_import
    ],
    m_rsc:update(LocalId, RscLang, UpdateOptions, Context).


ensure_language_prop(#{ <<"language">> := _ } = Rsc) ->
    Rsc;
ensure_language_prop(Rsc) ->
    Langs = maps:fold(
        fun
            (_K, #trans{ tr = Tr }, Acc) ->
                Langs = [ Iso || {Iso, _} <- Tr ],
                Acc ++ Langs;
            (_, _, Acc) ->
                Acc
        end,
        [],
        Rsc),
    case Langs of
        [_|_] -> Rsc#{ <<"language">> => lists:usort(Langs) };
        [] -> Rsc
    end.


cleanup_map_ids(RemoteRId, Rsc, UriTemplate, ImportedAcc, Options, Context) ->
    PropsForced = proplists:get_value(props_forced, Options, #{}),
    PropsDefault = proplists:get_value(props_default, Options, #{}),

    IsAuthCopy = proplists:get_value(is_authoritative, Options, false),

    % Import options for resources that are directly referred.
    % For example menus and the 'rsc_id' in blocks.
    N = proplists:get_value(import_edges, Options, 0),
    ReferredOptions = [
        {import_edges, erlang:max(0, N - 1)},
        {props_forced, maps:remove(<<"category_id">>, PropsForced)}
        | proplists:delete(import_edges,
            proplists:delete(props_forced, Options))
    ],

    % Remove or map modifier_id, creator_id, etc
    {Rsc1, ImportedAcc1} = maps:fold(
        fun
            (K, V, {Acc, ImpAcc}) ->
                case maps:is_key(K, PropsForced) of
                    true ->
                        % Forced props are assumed to use local ids, no mapping needed.
                        {Acc, ImpAcc};
                    false ->
                        case K of
                            % If we make an authoritative copy then the creator/modifier is the current user
                            <<"creator_id">> when IsAuthCopy ->
                                {Acc#{ <<"creator_id">> => z_acl:user(Context) }, ImpAcc};
                            <<"modifier_id">> when IsAuthCopy ->
                                {Acc#{ <<"modifier_id">> => z_acl:user(Context) }, ImpAcc};

                            % Be specific about the category id - handled by caller
                            <<"category_id">> -> {Acc, ImpAcc};
                            <<"category">> -> {Acc, ImpAcc};

                            % Importing into a content-group depends on the import Options
                            <<"content_group_id">> ->
                                case find_content_group(V, Context) of
                                    {ok, CGId} ->
                                        {Acc#{ <<"content_group_id">> => CGId }, ImpAcc};
                                    {error, _} ->
                                        {Acc, ImpAcc}
                                end;
                            <<"content_group">> -> {Acc, ImpAcc};

                            % Other ids are mapped to placeholders or local ids
                            <<"menu">> when is_list(V) ->
                                % map ids in menu to local ids
                                {Menu1, ImpAcc1} = map_menu(V, UriTemplate, [], ImpAcc, ReferredOptions, Context),
                                {Acc#{ <<"menu">> => Menu1 }, ImpAcc1};

                            % All other values are mapped, considering the key and value to see if
                            % it is a resource reference.
                            _ ->
                                {V1, ImpAcc1} = map_key_value(K, V, UriTemplate, ImpAcc, ReferredOptions, Context),
                                {Acc#{ K => V1 }, ImpAcc1}
                        end
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

%% @doc Check if the value looks like an exported id description.
is_id_map(#{
        <<"id">> := Id,
        <<"uri">> := Uri,
        <<"is_a">> := [ Cat |_ ]
    }) when is_integer(Id), is_binary(Uri), is_binary(Cat) ->
    true;
is_id_map(_) ->
    false.

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


%% @doc Map all ids in a list to local (stub) resources.
map_list([{K, V} | Rest], UriTemplate, Acc, ImpAcc, Options, Context) when is_binary(K) ->
    {V1, ImpAcc1} = map_key_value(K, V, UriTemplate, ImpAcc, Options, Context),
    map_list(Rest, UriTemplate, [{K, V1} | Acc], ImpAcc1, Options, Context);
map_list([V | Rest], UriTemplate, Acc, ImpAcc, Options, Context) ->
    {V1, ImpAcc1} = map_value(V, UriTemplate, ImpAcc, Options, Context),
    map_list(Rest, UriTemplate, [V1 | Acc], ImpAcc1, Options, Context);
map_list([], _UriTemplate, Acc, ImpAcc, _Options, _Context) ->
    {lists:reverse(Acc), ImpAcc}.

%% @doc Map all ids in a map. The map itself could be an id reference, if so then
%% replace the map with the newly referred local id.
map_map(Map, UriTemplate, ImpAcc, Options, Context) when is_map(Map) ->
    case is_id_map(Map) of
        true ->
            case map_id(Map, UriTemplate, ImpAcc, Options, Context) of
                {ok, {LocalId, ImpAcc1}} ->
                    {LocalId, ImpAcc1};
                {error, _} ->
                    {Map, ImpAcc}
            end;
        false ->
            maps:fold(
                fun(K, V, {BAcc, BImpAcc}) ->
                    {V1, BImpAcc1} = map_key_value(K, V, UriTemplate, BImpAcc, Options, Context),
                    {BAcc#{ K => V1 }, BImpAcc1}
                end,
                {#{}, ImpAcc},
                Map)
    end.

%% @doc Map ids in the value, irrespective of the name of the key.
map_value(V, UriTemplate, ImpAcc, Options, Context) ->
    map_key_value(<<>>, V, UriTemplate, ImpAcc, Options, Context).

%% @doc Map ids in the value, knowing the name and the semantics of the key.
map_key_value(K, V, UriTemplate, ImpAcc, Options, Context) ->
    case m_rsc_export:is_id_prop(K) orelse is_id_map(V) of
        true ->
            case map_id(V, UriTemplate, ImpAcc, Options, Context) of
                {ok, {LocalId, ImpAcc1}} ->
                    ImpAcc2 = ImpAcc1#{
                        uri(V) => LocalId
                    },
                    {LocalId, ImpAcc2};
                {error, _} ->
                    {undefined, ImpAcc}
            end;
        false when is_list(V) ->
            map_list(V, UriTemplate, [], ImpAcc, Options, Context);
        false when is_map(V) ->
            map_map(V, UriTemplate, ImpAcc, Options, Context);
        false ->
            map_html(K, V, UriTemplate, ImpAcc, Options, Context)
    end.


%% @doc Map a remote id to a local id, optionally creating a new (stub) resource.
map_id(RemoteId, UriTemplate, ImpAcc, Options, Context) when is_integer(RemoteId) ->
    case is_uri(UriTemplate) of
        true ->
            Uri = binary:replace(UriTemplate, <<":id">>, z_convert:to_binary(RemoteId)),
            Rsc = #{
                <<"uri">> => Uri
            },
            maybe_create_empty(Rsc, ImpAcc, Options, Context);
        false ->
            {error, enoent}
    end;
map_id(Remote, _UriTemplate, ImpAcc, Options, Context) when is_map(Remote) ->
    maybe_create_empty(Remote, ImpAcc, Options, Context);
map_id(Remote, _UriTemplate, ImpAcc, Options, Context) when is_binary(Remote) ->
    case is_uri(Remote) of
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
map_html(_Key, #trans{} = Value, UriTemplate, ImportAcc, Options, Context) ->
    map_html_1(Value, UriTemplate, ImportAcc, Options, Context);
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
    % Import options for resources that are embedded.
    % For example images and links in HTML texts.
    EmbeddedOptions = proplists:delete(import_edges, Options),
    case filter_embedded_media:embedded_media(Text, Context) of
        [] ->
            {Text, ImportAcc};
        EmbeddedIds ->
            {Text1, ImportAcc1} = lists:foldl(
                fun(RemoteId, {TextAcc, ImpAcc}) ->
                    case map_id(RemoteId, UriTemplate, ImpAcc, EmbeddedOptions, Context) of
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

is_uri(undefined) -> false;
is_uri(<<"http:", _/binary>>) -> true;
is_uri(<<"https:", _/binary>>) -> true;
is_uri(<<C, _/binary>> = Uri) when C >= $a, C =< $z -> is_schemed(Uri);
is_uri(<<C, _/binary>> = Uri) when C >= $A, C =< $Z -> is_schemed(Uri);
is_uri(_) -> false.

is_schemed(<<C, R/binary>>) when C >= $a, C =< $z -> is_schemed(R);
is_schemed(<<C, R/binary>>) when C >= $A, C =< $Z -> is_schemed(R);
is_schemed(<<$:, _/binary>>) -> true;
is_schemed(<<>>) -> false;
is_schemed(_) -> false.


is_html_prop(<<"body">>) -> true;
is_html_prop(<<"body_", _/binary>>) -> true;
is_html_prop(K) ->
    binary:longest_common_suffix([ K, <<"_html">> ]) =:= 5.


maybe_import_medium(LocalId, #{ <<"medium">> := Medium, <<"medium_url">> := MediaUrl } = JSON, Options, Context)
    when is_binary(MediaUrl), MediaUrl =/= <<>>, is_map(Medium) ->
    % If medium is outdated (compare with created date in medium record)
    %    - download URL
    %    - save into medium, ensure medium created date has been set (aka copied)
    % TODO: add medium created date option (to set equal to imported medium)
    Created = maps:get(<<"created">>, Medium, calendar:universal_time()),
    RemoteMedium = #{
        <<"created">> => Created
    },
    IsForcedUpdate = z_convert:to_bool( proplists:get_value(is_forced_update, Options, Context) ),
    LocalMedium = m_media:get(LocalId, Context),
    case IsForcedUpdate orelse is_newer_medium(RemoteMedium, LocalMedium) of
        true ->
            MediaOptions = [
                {is_escape_texts, false},
                is_import,
                no_touch,
                {fetch_options, proplists:get_value(fetch_options, Options, [])}
            ],
            RscProps = #{
                <<"original_filename">> => maps:get(<<"original_filename">>, Medium, undefined)
            },
            RscProps1 = case JSON of
                #{ <<"resource">> := Rsc } when is_map(Rsc) ->
                    RscProps#{
                        <<"medium_language">> => maps:get(<<"medium_language">>, Rsc, undefined),
                        <<"medium_edit_settings">> => maps:get(<<"medium_edit_settings">>, Rsc, undefined)
                    };
                _ ->
                    RscProps#{
                        <<"medium_language">> => undefined,
                        <<"medium_edit_settings">> => undefined
                    }
            end,
            _ = m_media:replace_url(MediaUrl, LocalId, RscProps1, MediaOptions, Context);
        false ->
            ok
    end,
    {ok, LocalId};
maybe_import_medium(LocalId, #{ <<"medium">> := Medium }, _Options, Context)
    when is_map(Medium) ->
    % Overwrite local medium record with the imported medium record
    % [ sanitize any HTML in the medium record ]
    case z_notifier:first(#media_import_medium{ id = LocalId, medium = Medium }, Context) of
        undefined ->
            ?LOG_NOTICE(#{
                text => <<"Resource import dropped medium record">>,
                in => zotonic_core,
                rsc_id => LocalId
            }),
            {ok, LocalId};
        ok ->
            {ok, LocalId};
        {ok, _} ->
            {ok, LocalId};
        {error, _} = Error ->
            Error
    end;
maybe_import_medium(LocalId, #{}, _Options, Context) ->
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
                case find_allowed_predicate(Name, Pred, Options, Context) of
                    {ok, PredId} ->
                        replace_edges(LocalId, PredId, Os, Acc, Options, Context);
                    {error, _} ->
                        Acc
                end;
            (Name, V, Acc) ->
                ?LOG_WARNING(#{
                    text => <<"Import of unknown predicate">>,
                    in => zotonic_core,
                    name => Name,
                    props => V
                }),
                Acc
        end,
        ImportedAcc,
        Edges).

replace_edges(LocalId, PredId, Os, ImportedAcc, Options, Context) ->
    % Keep order of edges
    {ObjectIds, ImportedAcc1} = lists:foldr(
        fun(Edge, {Acc, ImpAcc}) ->
            Object = maps:get(<<"object_id">>, Edge),
            case is_known_resource(Object, ImpAcc, Options, Context) of
                false ->
                    case maybe_create_empty(Object, ImpAcc, Options, Context) of
                        {ok, {ObjectId, ImpAcc1}} ->
                            {[ ObjectId | Acc ], ImpAcc1};
                        {error, Reason} ->
                            ?LOG_DEBUG(#{
                                text => <<"Skipping import of object">>,
                                in => zotonic_core,
                                result => error,
                                reason => Reason,
                                object => Object
                            }),
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


%% @doc Find the predicate to be imported, check against the allow/deny lists
%% of predicates.
find_allowed_predicate(Name, Pred, Options, Context) ->
    case find_predicate(Name, Pred, Context) of
        {ok, PredId} ->
            PredName = m_rsc:p_no_acl(PredId, name, Context),
            Allow = proplists:get_value(allow_predicate, Options),
            Deny = proplists:get_value(deny_predicate, Options, [ <<"hasusergroup">> ]),
            case (Allow =:= undefined orelse lists:member(PredName, Allow))
                andalso not lists:member(PredName, Deny)
            of
                true ->
                    {ok, PredId};
                false ->
                    ?LOG_NOTICE(#{
                        text => <<"Not importing edges because predicate is not allowed.">>,
                        in => zotonic_core,
                        result => error,
                        reason => eacces,
                        predicate => PredName
                    }),
                    {error, eacces}
            end;
        {error, _} = Error ->
            Error
    end.

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
            {ok, PredId};
        false ->
            {error, enoent}
    end.


%% @doc Find a content group, prefer matching on name before URI.
find_content_group(undefined, _Context) ->
    {error, enoent};
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

%% @doc Find the category to be imported, check against the allow/deny lists
%% of categories. Per default importing resources into the 'meta' category
%% is not allowed.
-spec find_allowed_category( RId::map(),  Rsc::map(), options(), z:context() ) -> {ok, m_rsc:resource_id()} | {error, term()}.
find_allowed_category(RId, Rsc, Options, Context) ->
    CatId = find_category(RId, Rsc, Context),
    CatName = m_rsc:p_no_acl(CatId, name, Context),
    Allow = proplists:get_value(allow_category, Options, undefined),
    Deny = proplists:get_value(deny_category, Options, [ <<"meta">> ]),
    case
        (Allow =:= undefined orelse matching_category(CatName, Allow, Context))
        andalso not matching_category(CatName, Deny, Context)
    of
        true ->
            {ok, CatId};
        false ->
            ?LOG_NOTICE(#{
                text => <<"Not importing resource because category is disallowed">>,
                in => zotonic_core,
                result => error,
                reason => eacces,
                category => CatName,
                category_id => CatId,
                rsc_id => RId
            }),
            {error, eacces}
    end.

matching_category(Name, Cats, Context) ->
    lists:any(
        fun(Cat) ->
            m_category:is_a(Name, Cat, Context)
        end,
        Cats).


%% @doc Find the category to be imported. This tries to map the 'is_a'
%% and the uri of the category.
-spec find_category( RId::map(),  Rsc::map(), z:context() ) -> m_rsc:resource_id().
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
            RscCatId;
        false ->
            case maps:get(<<"is_a">>, RId, undefined) of
                IsA when is_list(IsA) ->
                    case first_category(lists:reverse(IsA), Context) of
                        {ok, IsACatId} ->
                            IsACatId;
                        error ->
                            m_rsc:rid(other, Context)
                    end;
                _ ->
                    m_rsc:rid(other, Context)
            end
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
    case uri_string:parse(Uri) of
        #{ host := Host } ->
            z_convert:to_binary(Host);
        #{ scheme := Scheme } when Scheme =/= <<"http">>, Scheme =/= <<"https">> ->
            Scheme
    end.


%% @doc Check if the site is the local site
is_local_site(Uri, Context ) ->
    Site = z_context:site(Context),
    case z_sites_dispatcher:get_site_for_url(Uri) of
        {ok, Site} -> true;
        {ok, _} -> false;
        undefined -> false
    end.

%% @doc Generate a new page path by appending a number.
-spec unique_page_path(OptRscId, Path, Context) -> NewPath | undefined when
    OptRscId :: m_rsc:resource_id() | undefined,
    Path :: binary(),
    Context :: z:context(),
    NewPath :: binary().
unique_page_path(OptRscId, Path, Context) ->
    unique_page_path(OptRscId, Path, 1, Context).

unique_page_path(OptRscId, Path, N, Context) ->
    B = integer_to_binary(N),
    Path1 = <<Path/binary, $-, B/binary>>,
    case m_rsc:page_path_to_id(Path1, Context) of
        {ok, OptRscId} ->
            Path1;
        {ok, _} ->
            unique_page_path(OptRscId, Path, N+1, Context);
        {redirect, _} ->
            Path1;
        {error, {unknown_page_path, _}} ->
            Path1;
        {error, _} ->
            undefined
    end.

%% @doc Generate a new name by appending a number.
-spec unique_name(OptRscId, Name, Context) -> NewName | undefined when
    OptRscId :: m_rsc:resource_id() | undefined,
    Name :: binary(),
    Context :: z:context(),
    NewName :: binary().
unique_name(OptRscId, Name, Context) ->
    unique_name(OptRscId, Name, 1, Context).

unique_name(OptRscId, Name, N, Context) ->
    B = integer_to_binary(N),
    Name1 = <<Name/binary, $_, B/binary>>,
    case m_rsc:name_to_id(Name1, Context) of
        {ok, OptRscId} ->
            Name1;
        {ok, _} ->
            unique_page_path(OptRscId, Name, N+1, Context);
        {error, _} ->
            Name1
    end.

% Install the datamodel for managing resource imports, especially the rights and options for the imports.
-spec install( z:context() ) -> ok.
install(Context) ->
    case z_db:table_exists(rsc_import, Context) of
        true ->
            ok;
        false ->
            [] = z_db:q("
                create table if not exists rsc_import (
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
            [ z_db:q("create index if not exists "++Name++" on rsc_import ("++Cols++")", Context) || {Name, Cols} <- Indices ],
            z_db:flush(Context)
    end.

