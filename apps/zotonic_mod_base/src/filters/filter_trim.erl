%% @doc 'trim' filter, removes whitespace at the start and end of a string
-module(filter_trim).
-moduledoc("
Removes whitespace at the start and end of a string.

For example:


```django
{{ value|trim }}
```

When the value is `\"   hello   \"` then the output is `\"hello\"`.

Internally, this calls `z_string:trim/1` to perform the trimming.
").

-export([trim/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

trim(undefined, _Context) ->
    <<>>;
trim(#trans{} = Tr, Context) ->
    trim(z_trans:lookup_fallback(Tr, Context), Context);
trim(Input, _Context) ->
    z_string:trim(Input).
