%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2023 Marc Worrell
%% @doc 'truncate' filter, truncate a string on a certain length, taking word boundaries into account.
%% @end

%% Copyright 2010-2023 Marc Worrell
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

-module(filter_truncate).
-moduledoc("
See also

[truncatechars](/id/doc_template_filter_filter_truncatechars), [truncate\\_html](/id/doc_template_filter_filter_truncate_html)

Truncate a text to a maximum length.

The text is truncated to the maximum length specified with the argument. The text is always truncated at a word
boundary, to truncate at a character boundary use [truncatechars](/id/doc_template_filter_filter_truncatechars). If the
truncation is not after punctuation then the unicode ellipsis … character is appended.

For example:


```django
{{ value|truncate:8 }}
```

If the value is `hello world.` then the output is `hello…`.

Entities like `&amp;` are counted as a single character.

This filter is multibyte aware: Multi-byte UTF-8 characters are assumed to be non-breaking characters.



Truncating character
--------------------

An optional second argument defines which text will be added if the text is truncated:


```django
{{ value|truncate:8:\" (more)\" }}
```

If the value is `hello world.` then the output is `hello (more)`.
").
-export([truncate/2, truncate/3, truncate/4]).

-include_lib("zotonic_core/include/zotonic.hrl").

truncate(In, Context) ->
    truncate(In, 20, Context).

truncate(In, N, Context) ->
    truncate(In, N, <<"…"/utf8>>, Context).

truncate(undefined, _N, _Append, _Context) ->
    undefined;
truncate(S, N, Append, Context) when not is_integer(N) ->
    truncate(S, z_convert:to_integer(N), Append, Context);
truncate(#trans{} = Tr, N, Append, Context) ->
    truncate(z_trans:lookup_fallback(Tr, Context), N, Append, Context);
truncate(In, N, Append, _Context) when is_binary(In) ->
    z_string:truncate(In, N, z_convert:to_binary(Append));
truncate(In, N, Append, _Context) when is_list(In) ->
    z_string:truncate(iolist_to_binary(In), N, z_convert:to_binary(Append));
truncate(In, N, Append, Context) ->
    case z_template_compiler_runtime:to_simple_value(In, Context) of
        L when is_list(L) ->
            truncate(L, N, Append, Context);
        B when is_binary(B) ->
            truncate(B, N, Append, Context);
        _ ->
            undefined
    end.

