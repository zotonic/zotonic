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
% - do not import if previously deleted
% - handle ids embedded in texts
% - map ids in rsc blocks
% - keep menu structure
% - handle ids in menu structure

-module(m_rsc_import).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include("zotonic.hrl").

-export([
    create_empty/2,
    create_empty/3,
    import/3
]).


-type option() :: {props_forced, map()}
                | {props_default, map()}
                | is_import_edges
                | {is_import_edges, boolean()}.
-type options() :: [ option() ].

-export_type([
    options/0,
    option/0
]).


%% @doc Create an empty, non-authoritative resource, with the given uri.
-spec create_empty( string() | binary(), z:context()) -> {ok, m_rsc:resource_id()} | {error, duplicate_uri | term()}.
create_empty(Uri, Context) ->
    create_empty(Uri, #{}, Context).

-spec create_empty( string() | binary(), m_rsc:props_all(), z:context()) -> {ok, m_rsc:resource_id()} | {error, duplicate_uri | term()}.
create_empty(Uri, Props, Context) when is_list(Props) ->
    {ok, RscMap} = z_props:from_list(Props),
    create_empty(Uri, RscMap, Context);
create_empty(Uri, Props, Context) ->
    case m_rsc:uri_lookup(Uri, Context) of
        undefined ->
            Props1 = case maps:is_key(<<"category_id">>, Props) of
                false -> Props#{ <<"category_id">> => other };
                true -> Props
            end,
            Props2 = Props1#{
                <<"is_authoritative">> => false,
                <<"is_published">> => false,
                <<"uri">> => Uri
            },
            m_rsc:insert(Props2, Context);
        RscId ->
            lager:info("Imported resource of \"~s\" already exists as rsc id ~p",
                       [ Uri, RscId ]),
            {error, duplicate_uri}
    end.


-spec maybe_create_empty( map(), z:context() ) -> {ok, m_rsc:rescource_id()} | {error, term()}.
maybe_create_empty(Rsc, Context) ->
    case m_rsc:rid(Rsc, Context) of
        undefined ->
            Category = case maps:get(<<"is_a">>, Rsc, undefined) of
                [ _ | _ ] = IsA -> lists:last(IsA);
                _ -> other
            end,
            Props = #{
                <<"is_authoritative">> => false,
                <<"is_published">> => false,
                <<"uri">> => maps:get(<<"uri">>, Rsc),
                <<"title">> => maps:get(<<"title">>, Rsc, undefined),
                <<"name">> => maps:get(<<"name">>, Rsc, undefined),
                <<"category_id">> => Category
            },
            Options = [
                {is_escape_texts, false},
                is_import
            ],
            m_rsc:insert(Props, Options, Context);
        RscId ->
            {ok, RscId}
    end.


%% @doc Import a resource. If the resource already exists then it must be non-authoritative
%%      and have a matching URI. The resource to be updated is looked up by matching either the
%%      URI or the unique name. If the unique name matches then the category of the existing
%%      resource must have an overlap with the category of the imported resource.
-spec import( map(), options(), z:context() ) -> {ok, {m_rsc:resource_id(), [ m_rsc:resource_id() ]}} | {error, term()}.
import(#{
        <<"id">> := RemoteId,
        <<"resource">> := Rsc,
        <<"uri">> := Uri,
        <<"uri_template">> := UriTemplate
    } = JSON, Options, Context) ->

    ?DEBUG(RemoteId),

    io:format("~p~n", [ JSON ]),

    RemoteRId = #{
        <<"uri">> => Uri,
        <<"name">> => maps:get(<<"name">>, JSON, undefined),
        <<"is_a">> => maps:get(<<"is_a">>, JSON, undefined)
    },
    case update_rsc(RemoteRId, Rsc, UriTemplate, Options, Context) of
        {ok, LocalId} ->
            _ = maybe_import_medium(LocalId, JSON, Context),
            NewObjects = case proplists:get_value(is_import_edges, Options) of
                true ->
                    import_edges(LocalId, JSON, Context);
                false ->
                    []
            end,
            {ok, {LocalId, NewObjects}};
        {error, _} = Error ->
            Error
    end;
import(JSON, _Options, _Context) ->
    lager:warning("Import of JSON without required fields id, resource, uri and uri_template: ~p", [JSON]),
    {error, status}.


update_rsc(_RemoteRId, #{ <<"is_authoritative">> := false }, _UriTemplate, _Options, _Context) ->
    {error, remote_not_authoritative};
update_rsc(RemoteRId, Rsc, UriTemplate, Options, Context) ->
    Rsc1 = cleanup_rsc(RemoteRId, Rsc, UriTemplate, Options, Context),
    UpdateOptions = [
        {is_escape_texts, false},
        is_import
    ],
    case m_rsc:rid(RemoteRId, Context) of
        undefined ->
            m_rsc:insert(Rsc1, UpdateOptions, Context);
        LocalId ->
            case m_rsc:p_no_acl(LocalId, <<"is_authoritative">>, Context) of
                false ->
                    m_rsc:update(LocalId, Rsc1, UpdateOptions, Context);
                true ->
                    {error, authoritative}
            end
    end.

cleanup_rsc(RemoteRId, Rsc, UriTemplate, Options, Context) ->
    PropsForced = proplists:get_value(props_forced, Options, #{}),
    PropsDefault = proplists:get_value(props_default, Options, #{}),

    % Remove modifier_id, creator_id, etc
    Rsc1 = maps:fold(
        fun(K, V, Acc) ->
            % TODO: map ids in rsc and content to local ids
            % TODO: delete all other '..._id' not mapped and not in Options
            case m_rsc_export:is_id_prop(K) of
                true when is_map(V) ->
                    case m_rsc:rid(V, Context) of
                        undefined -> Acc;
                        VId -> Acc#{ K => VId }
                    end;
                true ->
                    Acc;
                false ->
                    Acc#{ K => V }
            end
        end,
        #{},
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
    Rsc5.


maybe_import_medium(LocalId, #{ <<"medium">> := Medium, <<"medium_url">> := MediaUrl }, Context)
    when is_binary(MediaUrl), is_map(Medium) ->
    % If medium is outdated (compare with created date in medium record)
    %    - download URL
    %    - save into medium, ensure created date has been set (aka copied)
    ?DEBUG(MediaUrl),
    Options = [
        {is_escape_texts, false},
        is_import,
        no_touch
    ],
    % TODO: add medium created date option (to set equal to imported medium)
    Created = maps:get(<<"created">>, Medium, calendar:universal_time()),
    UploadMedium = #{
        <<"created">> => Created
    },
    CurrentMedium = m_media:get(LocalId, Context),
    case is_newer_medium(UploadMedium, CurrentMedium) of
        true ->
            _ = m_media:replace_url(MediaUrl, LocalId, UploadMedium, Options, Context);
        false ->
            ok
    end,
    {ok, LocalId};
maybe_import_medium(LocalId, #{ <<"medium">> := Medium }, Context)
    when is_map(Medium) ->
    % TODO: overwrite local medium record with the imported medium record
    %       [ sanitize any HTML in the medium record ]
    {ok, LocalId};
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

is_newer_medium(#{ <<"created">> := Local }, #{ <<"created">> := Remote }) when Local < Remote ->
    true;
is_newer_medium(#{ <<"created">> := _ }, undefined) ->
    true;
is_newer_medium(_, _) ->
    false.


%%    <<"edges">> := #{
%%      %% Edges from this item to other items
%%      <<"depiction">> => #{
%%          <<"predicate">> => #{
%%                <<"id">> => 304,
%%                <<"is_a">> => [ meta, predicate ],
%%                <<"name">> => <<"depiction">>,
%%                <<"title">> => {trans,[{en,<<"Depiction">>}]},
%%                <<"uri">> => <<"http://xmlns.com/foaf/0.1/depiction">>
%%          },
%%          <<"objects">> => [
%%                #{
%%                    <<"created">> => {{2020,12,23},{15,4,55}},
%%                    <<"object_id">> => #{
%%                         <<"id">> => 28992,
%%                         <<"is_a">> => [media,image],
%%                         <<"name">> => undefined,
%%                         <<"title">> =>
%%                            {trans,[{nl,<<"NL: a.jpg">>},{en,<<"a.jpg">>}]},
%%                         <<"uri">> =>
%%                             <<"https://learningstone.test:8443/id/28992">>
%%                    },
%%                    <<"seq">> => 1
%%                },
%%                ... more objects
%%          ]
%%      },
%%      ... more predicates
%%    }

%% @doc Import all edges, return a list of newly created objects
import_edges(LocalId, #{ <<"edges">> := Edges }, Context) when is_map(Edges) ->
    % Delete edges not in 'Edges', add new edges if needed.
    maps:fold(
        fun
            (Name, #{ <<"predicate">> := Pred, <<"objects">> := Os }, Acc) ->
                case find_predicate(Name, Pred, Context) of
                    {ok, PredId} ->
                        NewObjects = replace_edges(LocalId, PredId, Os, Context),
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

replace_edges(LocalId, PredId, Os, Context) ->
    % Keep order of edges
    {ObjectIds, NewIds} = lists:foldr(
        fun(Edge, {Acc, New}) ->
            Object = maps:get(<<"object_id">>, Edge),
            case m_rsc:rid(Object, Context) of
                undefined ->
                    case maybe_create_empty(Object, Context) of
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

