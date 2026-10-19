%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Readable labels for module topic slugs, without a taxonomy file.
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

-module(filter_module_topic).
-moduledoc(#{
    zotonic_keywords => ["reference", "frontend_developer", "template_filter", "text", "format"]
}).
-moduledoc(<<"
Convert a module topic slug to a readable label.

Replaces underscores with spaces and uppercases the first Unicode character.
The remaining characters keep their original case. Accepts an atom, binary,
or string and returns a binary. An empty value or `undefined` returns `<<>>`.

For example:

```django
{{ keyword|module_topic|escape }}
```

When `keyword` is `media_management`, the output is `Media management`.

This filter formats the slug directly: it does not translate labels or read a
topic vocabulary file, so it also works in releases without that file.
The result is plain text; use `escape` when displaying it in HTML.
">>).
-author("Marc Worrell <marc@worrell.nl>").

-export([module_topic/2]).

-spec module_topic(Topic, Context) -> Label when
    Topic :: atom() | binary() | string() | undefined,
    Context :: z:context(),
    Label :: binary().
module_topic(undefined, _Context) ->
    <<>>;
module_topic(Topic, _Context) ->
    Text = binary:replace(z_convert:to_binary(Topic), <<"_">>, <<" ">>, [global]),
    case Text of
        <<First/utf8, Rest/binary>> ->
            <<(z_string:to_upper(<<First/utf8>>))/binary, Rest/binary>>;
        <<>> ->
            <<>>
    end.
