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
    observe_export_resource_content_disposition/2,
    rsc_props/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Add extra content-type to the 'id' controller; as fallbacks for content-types
%% the API controller can't handle.
observe_content_types_dispatch(#content_types_dispatch{id=Id}, Acc, Context) ->
    Acc ++ export_encoder:content_types_dispatch(Id, Context).


%% @doc Get the content-disposition for the export
observe_export_resource_content_disposition(
        #export_resource_content_disposition{
            dispatch = export_rsc_query,
            content_type = <<"application/atom+xml", _/binary>>
        },
        _Context) ->
    {ok, <<"inline">>};
observe_export_resource_content_disposition(#export_resource_content_disposition{}, _Context) ->
    {ok, <<"attachment">>}.

rsc_props(Context) ->
    m_rsc:common_properties(Context) ++ [page_url_abs].
