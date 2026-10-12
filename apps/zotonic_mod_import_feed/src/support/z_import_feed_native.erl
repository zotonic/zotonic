%% @copyright 2026 Marc Worrell
%% @doc Import explicitly advertised Zotonic resource exports through m_rsc_import.

%% Copyright 2026 Marc Worrell
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

-module(z_import_feed_native).
-define(DEFAULT_IMPORT_DEPTH, 1).
-include_lib("zotonic_core/include/zotonic.hrl").
-export([prepare/5, run/3, fetch/3, options/2, filter_resource/4]).

-spec prepare(map(), map(), [map()], map(), z:context()) -> {map(), [map()]}.
prepare(#{zotonic_export := Url, uri := Uri}, Item, _, Feed, C) when is_binary(Url) ->
    case read_export(Url, Uri, C) of
        {ok, JSON} ->
            Options = options(Feed, maps:get(props, Item)),
            Edges = case proplists:get_value(import_edges, Options) of
                0 -> [];
                _ -> [{P, U} || {P, #{<<"objects">> := Os}} <- maps:to_list(maps:get(<<"edges">>, JSON, #{})),
                    #{<<"object_id">> := #{<<"uri">> := U}} <- Os]
            end,
            {true, CleanProps} = filter_resource(maps:get(remove_patterns, Feed, <<>>),
                maps:get(<<"resource">>, JSON), Options, C),
            Props = maps:merge(CleanProps, proplists:get_value(props_forced, Options)),
            Dependents = [#{uri => U, props => #{}, medium => undefined} || U <- lists:usort([U || {_, U} <- Edges])],
            {Item#{zotonic_export => JSON, native_options => Options,
                props := Props, edges := Edges ++ maps:get(connections, Feed),
                medium := undefined, default_connections => maps:get(connections, Feed)}, Dependents};
        {error, Reason} -> throw({import_error, Reason})
    end;
prepare(_, Item, Dependents, _, _) -> {Item, Dependents}.

-spec options(map(), map()) -> m_rsc_import:options().
options(Feed, Props) ->
    Forced = (maps:with([<<"content_group_id">>, <<"rights">>,
        <<"rights_attribution">>, <<"rights_year">>], Props))#{<<"feed_is_zotonic">> => true},
    [
        {import_edges, maps:get(import_depth, Feed, ?DEFAULT_IMPORT_DEPTH)},
        {fetch_options, case maps:get(use_credentials, Feed, false) of
            true -> [];
            false -> [{authorization, none}]
        end},
        {is_subscribe, maps:get(is_subscribe, Feed, false)},
        {is_subscribe_connections, maps:get(is_subscribe_connections, Feed, false)},
        {is_authoritative, maps:get(is_authoritative, Feed, false)},
        {is_no_medium_download, maps:get(media_mode, Feed) =:= <<"none">>},
        {is_import_deleted, false},
        {props_forced, Forced},
        {filtermap_rsc, {?MODULE, filter_resource, [maps:get(remove_patterns, Feed, <<>>)]}}
    ].

-spec run(map(), integer() | undefined, z:context()) -> {integer(), map()}.
run(#{uri := Uri, zotonic_export := JSON, native_options := Options}, Existing, C) ->
    NativeC = z_context:set(import_feed_native, #{Uri => JSON}, C),
    Result = case Existing of
        undefined -> m_rsc_import:import_uri_recursive(Uri, Options, NativeC);
        Id ->
            case m_rsc_import:get_import_status(Id, NativeC) of
                {ok, _} -> m_rsc_import:reimport_recursive(Id, #{}, Options, NativeC);
                _ ->
                    %% Initialize native import bookkeeping for an older feed copy.
                    case m_rsc_import:import(Id, JSON, Options, NativeC) of
                        {ok, _} -> m_rsc_import:reimport_recursive(Id, #{}, Options, NativeC);
                        Error -> Error
                    end
            end
    end,
    case Result of
        {ok, {Id1, Imported}} ->
            %% Meta resources are references, not content owned by this feed.
            Content = maps:filter(fun(U, RId) ->
                U =/= Uri andalso not m_rsc:is_a(RId, meta, C)
            end, Imported),
            {Id1, Content};
        {error, Reason} -> throw({import_error, Reason})
    end.

%% Scoped to native imports: every resource fetch uses the bounded client with
%% the feed's authentication preference. The cached root avoids a second fetch and recursive observer calls.
-spec fetch(binary(), map(), z:context()) -> {ok, map()} | {error, term()}.
fetch(Uri, Cached, C) ->
    case z_db:q1("select count(*) from import_feed_resource where uri=$1 and rsc_id is null", [Uri], C) of
        1 -> {error, deleted};
        _ ->
            case maps:find(Uri, Cached) of
                {ok, JSON} -> envelope({ok, JSON});
                error -> envelope(read_export(Uri, Uri, C))
            end
    end.

envelope({ok, JSON}) -> {ok, #{<<"status">> => <<"ok">>, <<"result">> => JSON}};
envelope(Error) -> Error.

read_export(Url, Uri, C) ->
    case z_import_feed_http:get_json(Url, C) of
        {ok, {_, _, _, Body}} ->
            try normalize(z_json:decode(Body), Uri)
            catch _:_ -> {error, invalid_zotonic_export} end;
        Error -> Error
    end.

normalize(#{<<"status">> := <<"ok">>, <<"result">> := JSON}, Uri) -> normalize(JSON, Uri);
normalize(#{<<"uri">> := Uri, <<"resource">> := Rsc} = JSON, Uri) when is_map(Rsc) -> {ok, JSON};
normalize(_, _) -> {error, invalid_zotonic_export}.

%% Native imports retain multilingual properties and Zotonic media embeds.
-spec filter_resource(binary(), map(), list(), z:context()) -> {true, map()}.
filter_resource(PatternsText, Rsc, _, _) ->
    {ok, Patterns} = z_import_feed_cleanup:compile(PatternsText),
    {true, maps:map(fun
        (<<"body">>, Value) -> clean(Value, Patterns);
        (<<"summary">>, Value) -> clean(Value, Patterns);
        (_, Value) -> Value
    end, Rsc)}.

clean(#trans{tr = Translations} = Trans, Patterns) ->
    Trans#trans{tr = [{Lang, clean(Text, Patterns)} || {Lang, Text} <- Translations]};
clean(Text, Patterns) when is_binary(Text) -> z_import_feed_cleanup:html(Text, Patterns);
clean(Value, _) -> Value.
