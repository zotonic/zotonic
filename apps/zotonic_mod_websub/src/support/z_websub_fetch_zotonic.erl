%% @doc Fetch resource from remote Zotonic site, keep in sync.
%% @copyright 2021 Marc Worrell
%% @author Marc Worrell <marc@worrell.nl>

%% Copyright 2021 Marc Worrell
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

-module(z_websub_fetch_zotonic).
-author("Marc Worrell <marc@worrell.nl>").

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    fetch/3,

    fetch_json/1
]).


%% @doc Fetch the page at the URL, keep it in sync with the page that was
%% previously synchronized. Optionally fetches all objects of the page.
-spec fetch( Url, Options, Context ) -> {ok, m_rsc:resource_id()} | {error, term()}
    when Url :: binary(),
         Options :: list(),
         Context :: z:context().
fetch(Url, Options, Context) ->
    case fetch_json(Url) of
        {ok, JSON} ->
            import_json(Url, JSON, Options, Context);
        {error, _} = Error ->
            Error
    end.


% Example url:
%
% https://test.zotonic.com/id/1"
%
fetch_json(Url) ->
    Options = [
        {accept, "application/json"},
        {user_agent, "Zotonic-WebSub"},
        insecure
    ],
    case z_url_fetch:fetch(Url, Options) of
        {ok, {_FinalUrl, _Hs, _Size, Body}} ->
            JSON = jsxrecord:decode(Body),
            {ok, JSON};
        {error, _} = Error ->
            lager:warning("WebSub: error fetching ~p: ~p", [Url, Error]),
            Error
    end.

import_json(_Url, #{
    <<"status">> := <<"ok">>,
    <<"result">> := #{
        <<"id">> := RemoteId,
        <<"rsc">> := Rsc,
        <<"uri">> := Uri,
        <<"uri_template">> := UriTemplate
    } = JSON}, Options, Context) ->

    ?DEBUG(RemoteId),

    io:format("~p~n", [ JSON ]),

    case update_rsc(Uri, Rsc, UriTemplate, Options, Context) of
        {ok, LocalId} ->
            _ = maybe_update_medium(LocalId, JSON, Context),
            % TODO: if import_objects > 0 then also import objects
            % decrement import_objects for every recursion
            % (max import level  then import_objects = N, where N >= 0)
            {ok, LocalId};
        {error, _} = Error ->
            Error
    end;
import_json(Url, JSON, _Options, _Context) ->
    lager:warning("WebSub: JSON without status ok ~p: ~p", [Url, JSON]),
    {error, status}.


maybe_update_medium(LocalId, #{ <<"medium">> := Medium, <<"medium_url">> := MediaUrl }, Context)
    when is_binary(MediaUrl), is_map(Medium) ->
    % If medium is outdated (compare with created date in medium record)
    %    - download URL
    %    - save into medium, ensure created date has been set (aka copied)
    ?DEBUG(MediaUrl),
    Options = [
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

update_rsc(_Uri, #{ <<"is_authoritative">> := false }, _UriTemplate, _Options, _Context) ->
    {error, remote_not_authoritative};
update_rsc(Uri, Rsc, UriTemplate, Options, Context) ->
    Rsc1 = cleanup_rsc(Uri, Rsc, UriTemplate, Options, Context),
    case m_rsc:uri_lookup(Uri, Context) of
        undefined ->
            m_rsc:insert(Rsc1, [ is_import ], Context);
        LocalId ->
            m_rsc:update(LocalId, Rsc1, [ is_import ], Context)
    end.

cleanup_rsc(Uri, Rsc, UriTemplate, Options, Context) ->
    PropsForced = proplists:get_value(props_forced, Options, #{}),
    PropsDefault = proplists:get_value(props_default, Options, #{}),

    % Remove modifier_id, creator_id, etc
    Rsc1 = maps:fold(
        fun(K, V, Acc) ->
            % TODO: map ids in rsc and content to local ids
            % TODO: delete all other '_id' not mapped and not in Options
            % TODO: check local import tables to see if any of the ids can be mapped
            %       using the UriTemplate and the id.
            case is_id(K) of
                true ->
                    Acc;
                false ->
                    Acc#{ K => V }
            end
        end,
        #{},
        Rsc),
    Rsc2 = maps:merge(Rsc1, PropsForced),
    Rsc3 = maps:merge(PropsDefault, Rsc2),
    Rsc3#{
        <<"uri">> => Uri
    }.


is_id(<<"id">>) -> true;
is_id(K) -> binary:longest_common_suffix([ K, <<"_id">> ]) =:= 3.
