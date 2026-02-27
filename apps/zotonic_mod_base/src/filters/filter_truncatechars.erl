%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2023 Marc Worrell
%% @doc 'truncate' filter, truncate a string on a certain length.
%% @end

%% Copyright 2023 Marc Worrell
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

-module(filter_truncatechars).
-moduledoc("
Truncate a text to a maximum length in characters.

The text is truncated to the maximum length specified with the argument. The text is truncated at a characters boundary,
use [truncate](/id/doc_template_filter_filter_truncate) to truncate at a word boundary. If the truncation is not after
punctuation then the unicode ellipsis … character is appended.

For example:


```django
{{ value|truncatechars:8 }}
```

If the value is `hello world.` then the output is `hello wo…`.

Entities like `&amp;` are counted as a single character.



Truncating character
--------------------

An optional second argument defines which text will be added if the text is truncated:


```django
{{ value|truncatechars:8:\" (more)\" }}
```

If the value is `hello world.` then the output is `hello wo (more)`.

See also

[truncate](/id/doc_template_filter_filter_truncate), [truncate_html](/id/doc_template_filter_filter_truncate_html)").
-export([truncatechars/2, truncatechars/3, truncatechars/4]).

-include_lib("zotonic_core/include/zotonic.hrl").

truncatechars(In, Context) ->
    truncatechars(In, 20, Context).

truncatechars(In, N, Context) ->
    truncatechars(In, N, <<"…"/utf8>>, Context).

truncatechars(undefined, _N, _Append, _Context) ->
    undefined;
truncatechars(S, N, Append, Context) when not is_integer(N) ->
    truncatechars(S, z_convert:to_integer(N), Append, Context);
truncatechars(#trans{} = Tr, N, Append, Context) ->
    truncatechars(z_trans:lookup_fallback(Tr, Context), N, Append, Context);
truncatechars(In, N, Append, _Context) when is_binary(In) ->
    truncate(In, N, z_convert:to_binary(Append));
truncatechars(In, N, Append, _Context) when is_list(In) ->
    truncate(unicode:characters_to_binary(In), N, z_convert:to_binary(Append));
truncatechars(In, N, Append, Context) ->
    case z_template_compiler_runtime:to_simple_value(In, Context) of
        L when is_list(L) ->
            truncatechars(unicode:characters_to_binary(L), N, Append, Context);
        B when is_binary(B) ->
            truncatechars(B, N, Append, Context);
        _ ->
            undefined
    end.

-spec truncate( String :: binary(), Length :: integer(), Append :: binary()) -> binary().
truncate(_L, N, _Append) when N =< 0 ->
    <<>>;
truncate(B, N, Append) when is_binary(B), is_binary(Append) ->
    truncate(B, N, Append, <<>>).

truncate(<<>>, _, _Append, Acc) ->
    Acc;
truncate(_, 0, Append, Acc) ->
    <<Acc/binary, Append/binary>>;
truncate(<<"&#", Rest/binary>>, N, Append, Acc) ->
    {Acc1, Rest1} = entity(Rest, <<Acc/binary, "&#">>),
    truncate(Rest1, N-1, Append, Acc1);
truncate(<<"&",Rest/binary>>, N, Append, Acc) ->
    {Acc1, Rest1} = entity(Rest, <<Acc/binary, "&">>),
    truncate(Rest1, N-1, Append, Acc1);
truncate(<<C/utf8,Rest/binary>>, N, Append, Acc) ->
    truncate(Rest, N-1, Append, <<Acc/binary,C/utf8>>);
truncate(<<_, Rest/binary>>, N, Append, Acc) ->
    % Silently drop non-utf-8 characters
    truncate(Rest, N, Append, Acc).

entity(<<";", R/binary>>, Acc) ->
    {<<Acc/binary, ";">>, R};
entity(<<>>, Acc) ->
    {Acc, <<>>};
entity(<<C, _/binary>> = R, Acc) when C =< $0 ->
    {Acc, R};
entity(<<C/utf8, R/binary>>, Acc) ->
    entity(R, <<Acc/binary, C/utf8>>).

