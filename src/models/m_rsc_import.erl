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

-export([create_empty/2,
         create_empty/3,
         import/2
         ]).


%% @doc Create an empty, non-authoritative resource, with the given uri.
%% @spec create_empty(Uri, Context) -> {ok, Id} | {error, Reason}
create_empty(Uri, Context) ->    
    create_empty(Uri, [], Context).

create_empty(Uri, Props, Context) -> 
    Props1 = [{category_id, m_category:name_to_id_check(other, Context)},
              {note, "Pending import"},
              {is_published, false},
              {uri, Uri},
              {is_authoritative, false}] ++ Props,
    m_rsc:insert(Props1, Context).


%% @doc Import given resource. resource must already exist and be
%% non-authoritative. URIs must match.
%% @spec import(rsc_export(), Context) -> {ok, Id} | {error, Reason}
import(RscImport, Context) ->
    {uri, Uri} = proplists:lookup(uri, RscImport),
    Id = case m_rsc:uri_lookup(Uri, Context) of
             undefined -> throw({error, {unknown_rsc, Uri}});
             TheId -> TheId
         end,
	case z_acl:rsc_editable(Id, Context) of
		false -> throw({error, eacces});
		true -> ok
	end,
    case m_rsc:p(Id, is_authoritative, Context) of
        false ->
            %% Import rsc
            {rsc, Props} = proplists:lookup(rsc, RscImport),

            Props1 = case proplists:get_value(is_published, Props) of
                         true -> Props;
                         _ -> [{is_published, true}|proplists:delete(is_published, Props)]
                     end,

            Opts = [{escape_texts, false}],
            {ok, Id} = m_rsc_update:update(Id, Props1, Opts, Context),

            %% Import medium
            {ok, Id} = case proplists:get_value(medium, RscImport) of
                           undefined ->
                               {ok, Id};
                           MediumProps ->
                               Url = proplists:get_value(url, MediumProps),
                               m_media:replace_url(Url, Id, MediumProps, Context)
                       end,

            %% Import category
            %% Import group
            %% Import Edges
            {ok, Id};
        true ->
            {error, cannot_import_authoritative_rsc}
    end.
			
