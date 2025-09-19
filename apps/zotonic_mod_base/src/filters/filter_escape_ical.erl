%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'escape_ical' filter, escape a value for an ical file.

%% Copyright 2010 Marc Worrell
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

-module(filter_escape_ical).
-moduledoc("
See also

[escape](/id/doc_template_filter_filter_escape)

Escape the value according to the RFC2445 rules.

A double quote becomes `\\\"`; a comma becomes `\\,`; a colon becomes `\":\"`; a semicolon becomes `\\;`; a backslash
becomes `\\\\` and a newline becomes `\\n`.

It is also ensures that any single line is maximum 70 characters long by splitting the lines with newline/space combinations.

For example:


```django
{{ value|escape_ical }}
```

When the value is `abc:d;e` then the output is `abc\":\"d\\;e`.
").
-export([escape_ical/2]).


escape_ical(undefined, _Context) ->
	<<>>;
escape_ical(In, _Context) ->
	z_string:escape_ical(In).
