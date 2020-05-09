%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse
%%
%% @doc Importing non-authoritative things into the system.

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
    import/2
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


%% @doc Import given resource. resource must already exist and be
%% non-authoritative. URIs must match.
-spec import(m_rsc:props_all(), z:context()) -> {ok, m_rsc:resource_id()} | {error, atom()}.
import(RscImport, Context) when is_list(RscImport) ->
    {ok, RscMap} = z_props:from_list(RscImport),
    import(RscMap, Context);
import(#{ <<"uri">> := Uri } = RscImport, Context) ->
    case m_rsc:uri_lookup(Uri, Context) of
        undefined ->
            {error, {unknown_rsc, Uri}};
        Id ->
            import_1(Id, RscImport, Context)
    end;
import(_RscImport, _Context) ->
    {error, no_uri}.

import_1(Id, RscImport, Context) ->
    case z_acl:rsc_editable(Id, Context) of
        false ->
            {error, eacces};
        true ->
            case m_rsc:p(Id, is_authoritative, Context) of
                false ->
                    %% Import rsc
                    Props = maps:get(<<"rsc">>, RscImport),
                    Props1 = case maps:find(<<"is_published">>, Props) of
                        {ok, true} -> Props;
                        _ -> Props#{ <<"is_published">> => true }
                    end,
                    Opts = [
                        {escape_texts, false}
                    ],
                    case m_rsc_update:update(Id, Props1, Opts, Context) of
                        {ok, Id} ->
                            %% Import medium
                            {ok, Id} = case maps:find(<<"medium">>, RscImport) of
                                {ok, #{ <<"url">> := Url } = MediumProps} ->
                                    m_media:replace_url(Url, Id, MediumProps, Context);
                                _ ->
                                   {ok, Id}
                            end,
                            %% Import category
                            %% Import group
                            %% Import Edges
                            {ok, Id};
                        {error, _} = Error ->
                            Error
                    end;
                true ->
                    {error, authoritative}
            end
    end.

