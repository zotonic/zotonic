%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2024 Marc Worrell
%% @doc 'escapejs' filter, escape a value for output in javascript
%% @end

%% Copyright 2010-2024 Marc Worrell
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

-module(filter_escapejs).
-moduledoc("
See also

[escape](/id/doc_template_filter_filter_escape), [escapejson](/id/doc_template_filter_filter_escapejson)

Escapes the value for insertion in JavaScript output.

For example:


```django
{{ value|escapejs }}
```

When the value is `he'llo` then the output is `he\\x27llo`.

Internally, this calls `z_utils:js_escape/1` to perform the escaping.

Note: when generating JSON output, be sure to use [escapejson](/id/doc_template_filter_filter_escapejson), as JSON
escaping is subtly different from JS escaping.
").
-export([escapejs/2]).

escapejs(Input, Context) when is_map(Input) ->
    JSON = z_json:encode(Input),
    escapejs(JSON, Context);
escapejs(Input, Context) ->
    z_convert:to_binary(z_utils:js_escape(Input, Context)).
