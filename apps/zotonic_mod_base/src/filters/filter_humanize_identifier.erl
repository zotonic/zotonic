%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Convert underscore-separated identifiers to readable labels.
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

-module(filter_humanize_identifier).
-moduledoc(#{
    zotonic_keywords => ["reference", "frontend_developer", "template_filter", "text", "format"]
}).
-moduledoc(<<"
Convert an underscore-separated identifier to a readable label.

Replaces underscores with spaces and uppercases the first Unicode character.
The remaining characters keep their original case. Accepts an atom, binary,
or string and returns a binary. An empty value or `undefined` returns `<<>>`.

For example:

```django
{{ identifier|humanize_identifier|escape }}
```

When `identifier` is `media_management`, the output is `Media management`.

This filter formats the identifier directly without translating it or looking
up a label. It does not split camelCase or replace hyphens.
The result is plain text; use `escape` when displaying it in HTML.
">>).
-author("Marc Worrell <marc@worrell.nl>").

-export([humanize_identifier/2]).

-spec humanize_identifier(Identifier, Context) -> Label when
    Identifier :: atom() | binary() | string() | undefined,
    Context :: z:context(),
    Label :: binary().
humanize_identifier(undefined, _Context) ->
    <<>>;
humanize_identifier(Identifier, _Context) ->
    Text = binary:replace(z_convert:to_binary(Identifier), <<"_">>, <<" ">>, [global]),
    case Text of
        <<First/utf8, Rest/binary>> ->
            <<(z_string:to_upper(<<First/utf8>>))/binary, Rest/binary>>;
        <<>> ->
            <<>>
    end.
