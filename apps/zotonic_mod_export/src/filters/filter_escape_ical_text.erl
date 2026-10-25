%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Escape an iCalendar TEXT property value according to RFC 5545.
%% @end

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

-module(filter_escape_ical_text).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "frontend_developer", "template_filter",
        "export_and_syndication", "text", "serialize"
    ]
}).
-moduledoc("
Escape a value for an iCalendar TEXT property, such as SUMMARY, DESCRIPTION or
LOCATION. Backslashes, commas and semicolons are escaped with a backslash.
CRLF and carriage-return line endings are normalized to newlines, then escaped
as the iCalendar newline sequence. Colons and double quotes remain unchanged.
An undefined value produces an empty binary. Translation records use the request
language and normal translation fallback. Values are converted with
`z_convert:to_binary/2` before escaping.

For example, in an iCalendar template:

```django
SUMMARY:{{ id.title|unescape|escape_ical_text }}
```

Use `unescape` first for resource text containing HTML entities. Strip HTML tags
before escaping rich text. This filter escapes a property value; it does not
add a property name or fold content lines. The iCalendar encoder folds complete
lines at 75 octets and writes CRLF line endings.

This is TEXT-value escaping, not HTML escaping or URI/parameter encoding.
").
-export([escape_ical_text/2]).

-spec escape_ical_text(term(), z:context()) -> binary().

escape_ical_text(undefined, _) -> <<>>;
escape_ical_text(Value, Context) ->
    lists:foldl(fun({From, To}, B) -> binary:replace(B, From, To, [global]) end,
        z_convert:to_binary(Value, Context),
        [{<<"\\">>, <<"\\\\">>}, {<<"\r\n">>, <<"\n">>}, {<<"\r">>, <<"\n">>},
         {<<"\n">>, <<"\\n">>}, {<<";">>, <<"\\;">>}, {<<",">>, <<"\\,">>}]).
