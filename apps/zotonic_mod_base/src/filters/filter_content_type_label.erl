%% @author David de Boer <david@ddeboer.nl>
%% @doc Get human-readable label for a content (MIME) type
-module(filter_content_type_label).
-export([
    content_type_label/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-spec content_type_label(string(), #context{}) -> string().
content_type_label("text/html", Context) ->
    ?__("page", Context);
content_type_label("application/ld+json", Context) ->
    ?__("JSON-LD", Context);
content_type_label("application/json", Context) ->
    ?__("JSON", Context);
content_type_label("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", Context) ->
    ?__("Excel Workbook", Context);
content_type_label("text/calendar", Context) ->
    ?__("iCalendar", Context);
content_type_label("text/csv", Context) ->
    ?__("CSV", Context);
content_type_label(ContentType, Context) ->
    Label = case mimetypes:mime_to_exts(ContentType) of
        undefined ->
            ContentType;
        Extension ->
            Extension
    end,
    ?__(Label, Context).
