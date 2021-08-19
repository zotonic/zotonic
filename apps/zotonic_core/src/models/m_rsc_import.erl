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

-module(m_rsc_import).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include("zotonic.hrl").

-export([
    create_empty/2,
    create_empty/3,
    import/3
]).


-type options() :: list().

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
                <<"note">> => <<"Pending import">>,
                <<"is_published">> => false,
                <<"uri">> => Uri,
                <<"is_authoritative">> => false
            },
            m_rsc:insert(Props2, Context);
        RscId ->
            lager:info("Imported resource of \"~s\" already exists as rsc id ~p",
                       [ Uri, RscId ]),
            {error, duplicate_uri}
    end.


%% @doc Import given resource. resource must already exist and be non-authoritative. URIs must match.
-spec import( map(), options(), z:context() ) -> {ok, m_rsc:resource_id()} | {error, term()}.
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
            _ = maybe_update_medium(LocalId, JSON, Context),
            % TODO: if import_objects > 0 then also import objects
            % decrement import_objects for every recursion
            % (max import level  then import_objects = N, where N >= 0)
            {ok, LocalId};
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
    Rsc5 = case maps:find(<<"is_published">>, Rsc4) of
        {ok, true} -> Rsc4;
        _ -> Rsc4#{ <<"is_published">> => true }
    end,
    Rsc5.


maybe_update_medium(LocalId, #{ <<"medium">> := Medium, <<"medium_url">> := MediaUrl }, Context)
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
    ?DEBUG( m_media:replace_url(MediaUrl, LocalId, UploadMedium, Options, Context) ),
    {ok, LocalId};
maybe_update_medium(LocalId, #{ <<"medium">> := Medium }, Context)
    when is_map(Medium) ->
    % - overwrite local medium record with the imported medium record
    %   [ sanitize any HTML in the medium record ]
    {ok, LocalId};
maybe_update_medium(LocalId, #{}, Context) ->
    % If no medium:
    %    Delete local medium record (if any)
    case m_media:get(LocalId, Context) of
        undefined ->
            {ok, LocalId};
        _LocalMedium ->
            _ = m_media:delete(LocalId, Context),
            {ok, LocalId}
    end.


