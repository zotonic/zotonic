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
    observe_export_resource_encode/2
    ]).

-include_lib("zotonic.hrl").

%% @doc Add an extra content-type to the 'id' controller.
observe_content_types_dispatch(#content_types_dispatch{}, Acc, _Context) ->
    [{"text/csv", export_rsc_csv} | Acc].


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


%% @doc Encode a "row" of data, according to the encoding requested
observe_export_resource_encode(#export_resource_encode{content_type="text/csv", data=Id}, Context) when is_integer(Id) ->
    Data = rsc_data(Id, Context),
    {ok, export_encode_csv:encode(Data, Context)};
observe_export_resource_encode(#export_resource_encode{content_type="text/csv", data=Data}, Context) when is_list(Data) ->
    {ok, export_encode_csv:encode(Data, Context)};
observe_export_resource_encode(#export_resource_encode{}, _Context) ->
    undefined.

rsc_data(Id, Context) ->
    [ Id | [ m_rsc:p(Id, Prop, Context) || Prop <- rsc_fields() ] ].

rsc_fields() ->
    [
        title,
        summary,
        created,
        modified,
        page_url_abs
    ].

