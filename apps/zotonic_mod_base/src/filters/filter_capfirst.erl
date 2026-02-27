%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2023 Marc Worrell
%% @doc 'capfirst' filter, capitalize the first character
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

-module(filter_capfirst).
-moduledoc("
Converts the first character of the value to uppercase.

For example:


```django
{{ value|capfirst }}
```

When value is `hello world` then the output is `Hello world`.

At the moment this only works for the characters a through z. Accented characters (like ü) are not yet supported.

See also

[upper](/id/doc_template_filter_filter_upper)").
-export([capfirst/2]).

-include_lib("zotonic_core/include/zotonic.hrl").


capfirst(undefined, _Context) ->
    undefined;
capfirst(<<Byte, Binary/binary>>, _Context) when Byte >= $a andalso Byte =< $z ->
    [<<(Byte + $A - $a)>>, Binary];
capfirst(<<Byte, _/binary>> = S, _Context) when Byte =< 127 ->
    S;
capfirst(<<C/utf8, Rest/binary>>, _Context) ->
    First = z_string:to_upper(<<C/utf8>>),
    <<First/binary, Rest/binary>>;
capfirst([H|T], _Context) when is_integer(H) ->
    First = z_string:to_upper(<<H/utf8>>),
    <<First/binary, (unicode:characters_to_binary(T))/binary>>;
capfirst(L, Context) when is_list(L) ->
    capfirst(unicode:characters_to_binary(L), Context);
capfirst(#trans{} = In, Context) ->
    capfirst(z_trans:lookup_fallback(In, Context), Context);
capfirst(A, _Context) ->
    A.
