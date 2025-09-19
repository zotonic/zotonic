%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013-2025 Marc Worrell
%% @doc Generic export routines for data sources
%% @end

%% Copyright 2013-2025 Marc Worrell
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
-moduledoc("
Provides a generic framework to export [resources](/id/doc_glossary#term-resource).



Admin interface
---------------

[](../../_images/admin_view.png)When [enabled](/id/doc_developerguide_modules#activating-modules), this module adds two things to each admin edit page:

*   extra content types to the ‘View’ dropdown menu
*   an ‘Export’ block.

Both single pages and [query resources](/id/doc_developerguide_search#guide-query-resources) can be exported. For a
query, all resources matching it will be included in the export.



Customizing exports
-------------------

To customize data selection and the properties that are exported, observe one or several of the [export notifications](/id/doc_reference_notifications_import_export#export-notifications).
").
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Export Data").
-mod_description("Exports data as CSV and other formats.").
-mod_prio(800).
-mod_depends([mod_base]).

-export([
    observe_content_types_dispatch/3,
    observe_export_resource_content_disposition/2
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

