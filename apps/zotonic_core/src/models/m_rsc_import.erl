%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010-2021 Arjan Scherpenisse
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

% TODO:
% - handle ids embedded in texts
% - map ids in rsc blocks
% - import embedded HTML medium records
%
% - store import options and user in m_rsc_import table.

-module(m_rsc_import).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include("../../include/zotonic.hrl").

-export([
    mark_imported/3,

    create_empty/2,
    create_empty/3,

    get_import_status/2,
    set_import_status/3,

    import/2,
    import/3,
    import_uri_recursive/3,
    import_uri/2,
    import_uri/3,
    reimport_recursive/2,
    reimport/2,
    reimport/3,

    install/1
]).


-type option() :: {props_forced, map()}                 % Properties overlayed over the imported properties
                | {props_default, map()}                % Default properties
                | {import_edges, non_neg_integer()}     % Number of edge-redirections to import
                | is_import_deleted                     % Set if deleted resources must be re-imported
                | {is_import_deleted, boolean()}.
-type options() :: [ option() ].

-type import_result() :: {ok, {m_rsc:resource_id(), [ m_rsc:resource_id() ]}}
                       | {error, term()}.

-export_type([
    options/0,
    option/0,
    import_result/0
]).


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



%% @doc Create an empty, non-authoritative resource, with the given uri.
-spec create_empty( string() | binary(), z:context()) -> {ok, m_rsc:resource_id()} | {error, duplicate_uri | term()}.
create_empty(Uri, Context) ->
    create_empty(Uri, #{}, Context).

-spec create_empty( string() | binary(), m_rsc:props_all(), z:context()) -> {ok, m_rsc:resource_id()} | {error, duplicate_uri | term()}.
create_empty(Uri, Props, Context) ->
    create_empty(Uri, Props, [], Context).

-spec create_empty( string() | binary(), m_rsc:props_all(), options(), z:context()) -> {ok, m_rsc:resource_id()} | {error, duplicate_uri | term()}.
create_empty(Uri, Props, Options, Context) when is_list(Props) ->
    {ok, RscMap} = z_props:from_list(Props),
    create_empty(Uri, RscMap, Options, Context);
create_empty(Uri, Props, Options, Context) ->
    case m_rsc:uri_lookup(Uri, Context) of
        undefined ->
            Props1 = case maps:is_key(<<"category_id">>, Props) of
                false -> Props#{ <<"category_id">> => placeholder };
                true -> Props
            end,
            Props2 = Props1#{
                <<"is_authoritative">> => false,
                <<"is_published">> => false,
                <<"uri">> => Uri
            },
            z_db:transaction(fun(Ctx) ->
                case m_rsc:insert(Props2, Ctx) of
                    {ok, NewId} ->
                        Import = #{
                            <<"id">> => NewId,
                            <<"uri">> => Uri,
                            <<"host">> => host(Uri),
                            <<"user_id">> => z_acl:user(Ctx),
                            <<"options">> => Options
                        },
                        z_db:insert(rsc_import, Import, Ctx);
                    {error, _} = Error ->
                        {rollback, Error}
                end
            end, Context);
        RscId ->
            lager:info("Imported resource of \"~s\" already exists as rsc id ~p",
                       [ Uri, RscId ]),
            {error, duplicate_uri}
    end.


-spec maybe_create_empty( map(), options(), z:context() ) -> {ok, m_rsc:rescource_id()} | {error, term()}.
maybe_create_empty(Rsc, Options, Context) ->
    case valid_category(Rsc, Context) of
        {true, Cat} ->
            case m_rsc:rid(Rsc, Context) of
                undefined ->
                    Uri = maps:get(<<"uri">>, Rsc),
                    Props = #{
                        <<"category_id">> => Cat,
                        <<"is_authoritative">> => false,
                        <<"is_published">> => false,
                        <<"uri">> => Uri,
                        <<"title">> => maps:get(<<"title">>, Rsc, undefined),
                        <<"name">> => maps:get(<<"name">>, Rsc, undefined)
                    },
                    UpdateOptions = [
                        {is_escape_texts, false},
                        is_import
                    ],
                    z_db:transaction(fun(Ctx) ->
                        case m_rsc:insert(Props, UpdateOptions, Context) of
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
                    end, Context);
                RscId ->
                    {ok, RscId}
            end;
        false ->
            % Unknown category, deny access
            {error, eacces}
    end.

%% @doc Be strict for meta-category inserts - do not add categories or other meta
%% data by importing references.
valid_category(#{ <<"is_a">> := [ <<"meta">> | _ ] = IsA } = Rsc, Context) ->
    RemoteCat = lists:last(IsA),
    case m_rsc:rid(Rsc, Context) of
        undefined ->
            false;
        LocalId ->
            LocalIsA = [ z_convert:to_binary(C) || C <- m_rsc:is_a(LocalId, Context) ],
            case lists:last(LocalIsA) of
                RemoteCat -> {true, LocalId};
                _ -> false
            end
    end;
valid_category(_, _Context) ->
    {true, placeholder}.


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


%% @doc Recursively import all resources connected to the given resources. Return a list
%% of local resource ids that are imported or referred.
import_referred_ids([], Acc, _Context) ->
    Acc;
import_referred_ids([ LocalId | Ids ], Acc, Context) ->
    case reimport(LocalId, Context) of
        {ok, {NewLocalId, RefIds}} ->
            Acc1 = import_referred_ids(RefIds, Acc, Context),
            Acc2 = [ NewLocalId | Acc1 ],
            import_referred_ids(Ids, Acc2, Context);
        {error, _} ->
            import_referred_ids(Ids, Acc, Context)
    end.


%% @doc Reimport a non-authoritative resource using the saved import flags.
-spec reimport_recursive( m_rsc:resource_id(), z:context() ) -> import_result().
reimport_recursive(Id, Context) ->
    case reimport(Id, Context) of
        {ok, {LocalId, RefIds}} ->
            RefIds1 = import_referred_ids(RefIds, [], Context),
            {ok, {LocalId, RefIds1}};
        {error, _} = Error ->
            Error
    end.


%% @doc Reimport a non-authoritative resource using the saved import flags.
-spec reimport( m_rsc:resource_id(), z:context() ) -> import_result().
reimport(Id, Context) ->
    case z_db:select(rsc_import, Id, Context) of
        {ok, #{ <<"options">> := Options, <<"uri">> := Uri }} ->
            case m_rsc:p(Id, is_authoritative, Context) of
                true ->
                    {error, authoritative};
                false ->
                    case fetch_json(Uri) of
                        {ok, JSON} ->
                            import_json(Uri, JSON, Options, Context);
                        {error, _} = Error ->
                            Error
                    end
            end;
        {error, _} = Error ->
            Error
    end.

-spec reimport( m_rsc:resource_id(), options(), z:context() ) -> import_result().
reimport(Id, Options, Context) ->
    case m_rsc:p(Id, is_authoritative, Context) of
        true ->
            {error, authoritative};
        false ->
            Uri = m_rsc:uri(Id, Context),
            case fetch_json(Uri) of
                {ok, JSON} ->
                    import_json(Uri, JSON, Options, Context);
                {error, _} = Error ->
                    Error
            end
    end.

fetch_json(Url) ->
    Options = [
        {accept, "application/json"},
        {user_agent, "Zotonic"},
        insecure
    ],
    case z_url_fetch:fetch(Url, Options) of
        {ok, {_FinalUrl, _Hs, _Size, Body}} ->
            JSON = jsxrecord:decode(Body),
            {ok, JSON};
        {error, _} = Error ->
            lager:warning("Error fetching ~p: ~p", [Url, Error]),
            Error
    end.

import_json(_Url, #{ <<"status">> := <<"ok">>, <<"result">> := JSON }, Options, Context) ->
    import(JSON, Options, Context);
import_json(Url, #{ <<"status">> := <<"error">> } = JSON, _Options, _Context) ->
    lager:warning("Remote returned error ~p: ~p", [Url, JSON]),
    {error, remote};
import_json(Url, JSON, _Options, _Context) ->
    lager:warning("JSON with unknown structure ~p: ~p", [Url, JSON]),
    {error, status}.


%% @doc Import a non-authoritative resource from a remote URI using default import options.
-spec import_uri( string() | binary(), z:context() ) -> import_result().
import_uri(Uri, Context) ->
    import_uri(Uri, [], Context).

%% @doc Import a non-authoritative resource from a remote URI.
-spec import_uri( string() | binary(), options(), z:context() ) -> import_result().
import_uri(Uri, Options, Context) ->
    case fetch_json(Uri) of
        {ok, JSON} ->
            import_json(Uri, JSON, Options, Context);
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
import(#{
        <<"id">> := _RemoteId,
        <<"resource">> := Rsc,
        <<"uri">> := Uri,
        <<"uri_template">> := UriTemplate
    } = JSON, Options, Context) ->

    RemoteRId = #{
        <<"uri">> => Uri,
        <<"name">> => maps:get(<<"name">>, JSON, undefined),
        <<"is_a">> => maps:get(<<"is_a">>, JSON, undefined)
    },
    case Rsc of
        #{ <<"is_authoritative">> := false } ->
            {error, remote_not_authoritative};
        _ ->
            {Rsc1, EmbeddedIds} = cleanup_map_ids(RemoteRId, Rsc, UriTemplate, Options, Context),
            case update_rsc(RemoteRId, Rsc1, Options, Context) of
                {ok, LocalId} ->
                    _ = maybe_import_medium(LocalId, JSON, Context),
                    NewObjects = case proplists:get_value(import_edges, Options, 0) of
                        N when is_integer(N), N > 0 ->
                            EdgeOptions = [
                                {import_edges, N - 1} | proplists:delete(import_edges, Options)
                            ],
                            import_edges(LocalId, JSON, EdgeOptions, Context);
                        _ ->
                            []
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
                    {ok, {LocalId, lists:usort(EmbeddedIds ++ NewObjects)}};
                {error, _} = Error ->
                    Error
            end
    end;
import(JSON, _Options, _Context) ->
    lager:warning("Import of JSON without required fields id, resource, uri and uri_template: ~p", [JSON]),
    {error, status}.


update_rsc(RemoteRId, Rsc, Options, Context) ->
    UpdateOptions = [
        {is_escape_texts, false},
        is_import
    ],
    IsImportDeleted = proplists:get_value(is_import_deleted, Options, false),
    case m_rsc:rid(RemoteRId, Context) of
        undefined when IsImportDeleted ->
            m_rsc:insert(Rsc, UpdateOptions, Context);
        undefined when not IsImportDeleted ->
            Uri = maps:get(<<"uri">>, Rsc),
            case m_rsc_gone:is_gone_uri(Uri, Context) of
                true ->
                    {error, deleted};
                false ->
                    m_rsc:insert(Rsc, UpdateOptions, Context)
            end;
        LocalId ->
            case m_rsc:p_no_acl(LocalId, <<"is_authoritative">>, Context) of
                false ->
                    m_rsc:update(LocalId, Rsc, UpdateOptions, Context);
                true ->
                    {error, authoritative}
            end
    end.

cleanup_map_ids(RemoteRId, Rsc, UriTemplate, Options, Context) ->
    PropsForced = proplists:get_value(props_forced, Options, #{}),
    PropsDefault = proplists:get_value(props_default, Options, #{}),

    % Remove or map modifier_id, creator_id, etc
    {Rsc1, EmbeddedIds} = maps:fold(
        fun
            (<<"menu">>, Menu, {Acc, AccIds}) when is_list(Menu) ->
                % map ids in menu to local ids
                Menu1 = map_menu(Menu, UriTemplate, [], Options, Context),
                AccIds1 = menu_ids(Menu1, []) ++ AccIds,
                {Acc#{ <<"menu">> => Menu1 }, AccIds1};
            (<<"blocks">>, Blocks, {Acc, AccIds}) when is_list(Blocks) ->
                % TODO: map ids in blocks and content to local ids
                {Blocks1, AccIds1} = map_blocks(Blocks, UriTemplate, [], AccIds, Options, Context),
                {Acc#{ <<"blocks">> => Blocks1 }, AccIds1};
            (K, V, {Acc, AccIds}) ->
                case m_rsc_export:is_id_prop(K) of
                    true ->
                        case map_id(V, UriTemplate, Options, Context) of
                            {ok, LocalId} ->
                                {Acc#{ K => LocalId }, [ LocalId | AccIds ]};
                            {error, _} ->
                                {Acc#{ K => undefined }, AccIds}
                        end;
                    false ->
                        {V1, AccIds1} = map_html(K, V, UriTemplate, AccIds, Options, Context),
                        {Acc#{ K => V1 }, AccIds1}
                end
        end,
        {#{}, []},
        Rsc),

    % Set forced and default props
    Rsc2 = maps:merge(Rsc1, PropsForced),
    Rsc3 = maps:merge(PropsDefault, Rsc2),
    Rsc4 = Rsc3#{
        <<"uri">> => maps:get(<<"uri">>, RemoteRId),
        <<"is_authoritative">> => false
    },

    % Ensure that the is_published flag is present, defaults to true
    Rsc5 = case maps:find(<<"is_published">>, Rsc4) of
        {ok, true} -> Rsc4;
        _ -> Rsc4#{ <<"is_published">> => true }
    end,
    {Rsc5, lists:usort(EmbeddedIds)}.


%% @doc Map all ids in a menu to local (stub) resources. Skip all tree entries
%% that could not be mapped to local ids.
map_menu([ #rsc_tree{ id = RemoteId, tree = Sub } | Rest ], UriTemplate, Acc, Options, Context) ->
    case map_id(RemoteId, UriTemplate, Options, Context) of
        {ok, LocalId} ->
            Sub1 = map_menu(Sub, UriTemplate, [], Options, Context),
            Acc1 = [ #rsc_tree{ id = LocalId, tree = Sub1 } | Acc ],
            map_menu(Rest, UriTemplate, Acc1, Options, Context);
        {error, _} ->
            % Skip the tree entry if the remote id could not be mapped
            map_menu(Rest, UriTemplate, Acc, Options, Context)
    end;
map_menu([ _ | Rest ], UriTemplate, Acc, Options, Context) ->
    map_menu(Rest, UriTemplate, Acc, Options, Context);
map_menu([], _UriTemplate, Acc, _Options, _Context) ->
    lists:reverse(Acc).


%% @doc Map all ids in blocks to local (stub) resources. Remove blocks that can not
%% have their ids mapped.
map_blocks([ B | Rest ], UriTemplate, Acc, AccIds, Options, Context) ->
    {B1, AccIds1} = maps:fold(
        fun(K, V, {BAcc, BAccIds}) ->
            case m_rsc_export:is_id_prop(K) of
                true ->
                    case map_id(V, UriTemplate, Options, Context) of
                        {ok, LocalId} ->
                            {BAcc#{ K => LocalId }, [ LocalId | BAccIds ]};
                        {error, _} ->
                            {BAcc#{ K => undefined }, BAccIds}
                    end;
                false ->
                    {V1, BAccIds1} = map_html(K, V, UriTemplate, BAccIds, Options, Context),
                    {BAcc#{ K => V1}, BAccIds1 }
            end
        end,
        {#{}, AccIds},
        B),
    map_blocks(Rest, UriTemplate, [ B1 | Acc ], AccIds1, Options, Context);
map_blocks([], _UriTemplate, Acc, AccIds, _Options, _Context) ->
    {lists:reverse(Acc), AccIds}.



%% @doc Map a remote id to a local id, optionally creating a new (stub) resource.
map_id(RemoteId, UriTemplate, Options, Context) when is_integer(RemoteId) ->
    case is_url(UriTemplate) of
        true ->
            URL = binary:replace(UriTemplate, <<":id">>, z_convert:to_binary(RemoteId)),
            Rsc = #{
                <<"uri">> => URL
            },
            maybe_create_empty(Rsc, Options, Context);
        false ->
            {error, enoent}
    end;
map_id(Remote, _UriTemplate, Options, Context) when is_map(Remote) ->
    maybe_create_empty(Remote, Options, Context);
map_id(Remote, _UriTemplate, Options, Context) when is_binary(Remote) ->
    case is_url(Remote) of
        true ->
            Rsc = #{
                <<"uri">> => Remote
            },
            maybe_create_empty(Rsc, Options, Context);
        false ->
            {error, enoent}
    end;
map_id(_, _, _Options, _Context) ->
    {error, enoent}.


%% @doc Map texts in html properties.
map_html(Key, Value, UriTemplate, AccIds, Options, Context) ->
    case is_html_prop(Key) of
        true ->
            map_html_1(Value, UriTemplate, AccIds, Options, Context);
        false ->
            {Value, AccIds}
    end.

map_html_1(#trans{ tr = Tr }, UriTemplate, AccIds, Options, Context) ->
    {Tr1, AccIds1} = lists:foldr(
        fun({Lang, Text}, {TAcc, TAccIds}) ->
            Text1 = map_html_1(Text, UriTemplate, AccIds, Options, Context),
            {[ {Lang, Text1} | TAcc ], TAccIds}
        end,
        {[], AccIds},
        Tr),
    {#trans{ tr = Tr1 }, AccIds1};
map_html_1(Text, UriTemplate, AccIds, Options, Context) when is_binary(Text) ->
    case filter_embedded_media:embedded_media(Text, Context) of
        [] ->
            {Text, AccIds};
        EmbeddedIds ->
            Text1 = lists:foldl(
                fun(RemoteId, TextAcc) ->
                    case map_id(RemoteId, UriTemplate, Options, Context) of
                        {ok, LocalId} ->
                            From = <<"<!-- z-media ", (integer_to_binary(RemoteId))/binary, " ">>,
                            To = <<"<!-- z-media-local ", (integer_to_binary(LocalId))/binary, " ">>,
                            binary:replace(TextAcc, From, To, [ global ]);
                        {error, _} ->
                            From = <<"<!-- z-media ", (integer_to_binary(RemoteId))/binary, " ">>,
                            To = <<"<!-- z-media-temp 0 ">>,
                            binary:replace(TextAcc, From, To, [ global ])
                    end
                end,
                Text,
                EmbeddedIds),
            Text2 = binary:replace(Text1, <<"<!-- z-media-local ">>, <<"<!-- z-media ">>, [ global ]),
            {Text2, EmbeddedIds ++ AccIds}
    end.

is_url(<<"http:", _/binary>>) -> true;
is_url(<<"https:", _/binary>>) -> true;
is_url(_) -> false.


is_html_prop(<<"body">>) -> true;
is_html_prop(<<"body_", _/binary>>) -> true;
is_html_prop(K) ->
    binary:longest_common_suffix([ K, <<"_html">> ]) =:= 5.


menu_ids([], Acc) ->
    lists:usort(Acc);
menu_ids([ #rsc_tree{ id = Id, tree = Sub } | Rest ], Acc) ->
    Acc1 = [ Id | menu_ids(Sub, Acc) ],
    menu_ids(Rest, Acc1).


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
import_edges(LocalId, #{ <<"edges">> := Edges }, Options, Context) when is_map(Edges) ->
    % Delete edges not in 'Edges', add new edges if needed.
    maps:fold(
        fun
            (Name, #{ <<"predicate">> := Pred, <<"objects">> := Os }, Acc) ->
                case find_predicate(Name, Pred, Context) of
                    {ok, PredId} ->
                        NewObjects = replace_edges(LocalId, PredId, Os, Options, Context),
                        NewObjects ++ Acc;
                    {error, _} ->
                        Acc
                end;
            (Name, V, Acc) ->
                lager:warning("Unknown import predicate ~p => ~p", [ Name, V]),
                Acc
        end,
        [],
        Edges).

replace_edges(LocalId, PredId, Os, Options, Context) ->
    % Keep order of edges
    {ObjectIds, NewIds} = lists:foldr(
        fun(Edge, {Acc, New}) ->
            Object = maps:get(<<"object_id">>, Edge),
            case m_rsc:rid(Object, Context) of
                undefined ->
                    case maybe_create_empty(Object, Options, Context) of
                        {ok, ObjectId} ->
                            {[ ObjectId | Acc ], [ ObjectId | New ]};
                        {error, Reason} ->
                            lager:debug("Skipping object ~p: ~p", [ Object, Reason ]),
                            {Acc, New}
                    end;
                ObjectId ->
                    {[ ObjectId | Acc ], New}
            end
        end,
        {[], []},
        Os),
    m_edge:update_sequence(LocalId, PredId, ObjectIds, Context),
    NewIds.


% For now we only import if we know the predicate.
% An option could be added to insert the predicate if it is was unknown.
find_predicate(Name, Pred, Context) ->
    PredId = case m_rsc:rid(Name, Context) of
        undefined -> m_rsc:rid(Pred, Context);
        Id -> Id
    end,
    case m_rsc:is_a(PredId, predicate, Context) of
        true -> {ok, PredId};
        false -> {error, enoent}
    end.


host(Uri) ->
    #{
        host := Host
    } = uri_string:parse(Uri),
    Host.

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

