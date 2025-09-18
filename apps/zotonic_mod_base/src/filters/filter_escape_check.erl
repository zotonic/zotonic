%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc Ensure a value is properly escaped.

%% Copyright 2021 Marc Worrell
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

-module(filter_escape_check).
-moduledoc("
See also

[force\\_escape](/id/doc_template_filter_filter_force_escape), [escape](/id/doc_template_filter_filter_escape)

Ensures thant an HTML escaped value is properly escaped.

Checks for all reserved HTML characters if they are properly escaped.

Escaped strings are safe to be displayed in a HTML page. When you echo a query string argument or path variable then you
must escape the value before displaying it on a HTML page.

The following characters are replaced:

| Character | Replacement |
| --------- | ----------- |
| `\\\\>`     | `&gt;`      |
| `<`       | `&lt;`      |
| `\"`       | `&quot;`    |
| `'`       | `&#039;`    |
| `&`       | `&amp;`     |

If you always want escaping to be applied, use the [force\\_escape](/id/doc_template_filter_filter_force_escape) filter.

For example:


```django
{{ value|escape_check }}
```

When the value is `<hel&amp;lo\\>` then the output is `&lt;hel&amp;lo&gt;`.
").

-export([ escape_check/ 2]).

-include_lib("zotonic_core/include/zotonic.hrl").

escape_check(undefined, _Context) ->
    undefined;
escape_check(V, _Context) when is_number(V); is_boolean(V) ->
    V;
escape_check(V, Context) when is_list(V) ->
    escape_check(unicode:characters_to_binary(V, utf8), Context);
escape_check(B, _Context) when is_binary(B) ->
    z_html:escape_check(B);
escape_check(#trans{} = Tr, _Context) ->
    z_html:escape_check(Tr);
escape_check(A, _Context) when is_atom(A) ->
    z_html:escape_check(atom_to_binary(A, utf8));
escape_check(M, Context) when is_map(M) ->
    z_sanitize:escape_props_check(M, Context);
escape_check(_, _Context) ->
    undefined.
