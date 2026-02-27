%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell
%% @doc Escape filter, same as 'force_escape'

%% Copyright 2016 Marc Worrell
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

-module(filter_escape).
-moduledoc("
HTML escape a text. Escapes all reserved HTML characters in the value. Escaped strings are safe to be displayed in a
HTML page. When you echo a query string argument or path variable then you must escape the value before displaying it on
a HTML page.

The following characters are replaced:

| Character | Replacement |
| --------- | ----------- |
| `\\\\>`     | `&gt;`      |
| `<`       | `&lt;`      |
| `\"`       | `&quot;`    |
| `'`       | `&#039;`    |
| `&`       | `&amp;`     |

The escaping is only applied if the filter is not within an `{% autoescape on %}` block. If you always want escaping to
be applied, use the [force_escape](/id/doc_template_filter_filter_force_escape) filter.

For example:


```django
{{ value|escape }}
```

When the value is `<hel&lo\\>` then the output is `&lt;hel&amp;lo&gt;`.

Note: this filter is not part of a module, it is built into the template compiler.

See also

[force_escape](/id/doc_template_filter_filter_force_escape), [escape_check](/id/doc_template_filter_filter_escape_check)").
-export([
    escape/2
    ]).

escape(V, Context) ->
    filter_force_escape:force_escape(V, Context).
