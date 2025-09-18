%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2022 Marc Worrell
%% @doc Lookup a specific translation in a trans record. None trans record values
%% are passed as-is.
%% @end

%% Copyright 2022 Marc Worrell
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

-module(filter_translation).
-moduledoc("
See also

[translate](/id/doc_template_filter_filter_translate)

Lookup a specific translation in a translated text. If the text is not translated then the text is returned as-is.

The filter takes as input a value or other variable and as argument the language to be shown.

Example usage:


```django
{{ text|translation:\"en\" }}
```

If the variable `text` has the value:


```django
{trans, [{en, <<\"Hello\">>}, {nl,<<\"Hallo\">>}]}
```

Then this will show `Hello`, even if the current template language is set to `nl`.

This filter is especially useful for filling in forms with language specific strings to be edited.
").

-export([
    translation/3
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

translation(_V, undefined, _Context) ->
    undefined;
translation(#trans{ tr = Tr }, Lang, _Context) ->
    case z_language:to_language_atom(Lang) of
        {ok, Iso} ->
            proplists:get_value(Iso, Tr);
        {error, _} ->
            undefined
    end;
translation(V, _Lang, _Context) ->
    V.
