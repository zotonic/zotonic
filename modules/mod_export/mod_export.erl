%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013 Marc Worrell
%% @doc Generic export routines for data sources

%% Copyright 2013 Marc Worrell
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

-module(mod_export).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Export Data").
-mod_description("Exports data as CSV and other formats.").
-mod_prio(800).
-mod_depends([mod_base]).

-export([
    observe_content_types_dispatch/3,
    observe_export_resource_data/2,

    rsc_props/1
    ]).

-include_lib("zotonic.hrl").

%% @doc Add an extra content-type to the 'id' controller.
observe_content_types_dispatch(#content_types_dispatch{}, Acc, _Context) ->
    Acc.

%% @doc Fetch all ids making up the export, handles collections and search queries.
observe_export_resource_data(#export_resource_data{id=Id}, Context) when is_integer(Id) ->
    case m_rsc:is_a(Id, 'query', Context) of
        true ->
            {ok, z_search:query_([{id, Id}], Context)};
        false ->
            case m_rsc:is_a(Id, collection, Context) of
                true -> {ok, m_edge:objects(Id, haspart, Context)};
                false -> {ok, [Id]}
            end
    end;
observe_export_resource_data(#export_resource_data{}, _Context) ->
    {ok, []}.

rsc_props(Context) ->
    m_rsc:common_properties(Context) ++ [page_url_abs].

