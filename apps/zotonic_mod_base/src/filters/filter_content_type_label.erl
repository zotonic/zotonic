%% @author David de Boer <david@ddeboer.nl>
%% @doc Get human-readable label for a content (MIME) type
%% @copyright 2010-2025 David de Boer
%% @end

%% Copyright 2010-2025 David de Boer
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

-module(filter_content_type_label).
-moduledoc("
Todo

Not yet documented.
").
-export([
    content_type_label/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-spec content_type_label(ContentType, Context) -> binary() | undefined when
    ContentType :: undefined | string() | binary() | Mime,
    Mime :: cow_http_hd:media_type(),
    Context :: z:context().
content_type_label(undefined, _Context) ->
    undefined;
content_type_label(ContentType, Context) when is_list(ContentType) ->
    content_type_label(z_convert:to_binary(ContentType), Context);
content_type_label(ContentType, Context) when is_tuple(ContentType) ->
    ContentType1 = cowmachine_util:format_content_type(ContentType),
    content_type_label(ContentType1, Context);
content_type_label(<<"text/html">>, Context) ->
    ?__("page", Context);
content_type_label(<<"application/ld+json">>, Context) ->
    ?__("JSON-LD", Context);
content_type_label(<<"application/json">>, Context) ->
    ?__("JSON", Context);
content_type_label(<<"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet">>, Context) ->
    ?__("Excel Workbook", Context);
content_type_label(<<"text/calendar">>, Context) ->
    ?__("iCalendar", Context);
content_type_label(<<"text/csv">>, Context) ->
    ?__("CSV", Context);
content_type_label(ContentType, Context) ->
    Extension = case z_media_identify:extension(ContentType) of
        <<".bin">> -> ContentType;
        <<".", Ext/binary>> -> Ext
    end,
    z_string:to_upper(?__(Extension, Context)).
