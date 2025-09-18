%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2023 Marc Worrell
%% @doc Translate a string to the current or given language.
%% @enddoc

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

-module(filter_translate).
-moduledoc("
See also

[translation](/id/doc_template_filter_filter_translation)

Translates a (English) value to the current language or the given language.

If the input is a `#trans{}` record then it is extended with the translations from the .po files before the language
lookup is done. For this the `#trans{}` record *must* have the English translation.

Example with the default language lookup (accessible via the template variable `z_language`):


```django
{{ \"Cancel\"|translate }}
```

If the current language is `de` then the output is: `\"Abbrechen\"`.

An example with a specific language:


```django
{{ \"Cancel\"|translate:\"nl\" }}
```

The output would be `\"Annuleer\"`.
").
-export([
    translate/2,
    translate/3
]).


-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Translate a value in the current language.
-spec translate(Value, Context) -> Translated when
    Value :: term(),
    Context :: z:context(),
    Translated :: term().
translate(Value, Context) ->
    translate(Value, z_context:language(Context), Context).

%% @doc Translate a value in a language.
-spec translate(Value, Language, Context) -> Translated when
    Value :: term(),
    Language :: atom() | binary() | string(),
    Context :: z:context(),
    Translated :: term().
translate(undefined, _Lang, _Context) ->
    undefined;
translate(V, Lang, Context) when is_atom(Lang) ->
    translate_1(V, Lang, Context);
translate(V, Lang, Context) ->
    case z_language:to_language_atom(Lang) of
        {ok, Code} ->
            translate_1(V, Code, Context);
        {error, _} ->
            V
    end.

translate_1(V, Lang, Context) when is_binary(V) ->
    Tr = z_trans:translations(V, Context),
    z_trans:lookup_fallback(Tr, Lang, Context);
translate_1(#trans{} = V, Lang, Context) ->
    Tr = z_trans:translations(V, Context),
    z_trans:lookup_fallback(Tr, Lang, Context);
translate_1(V, Lang, Context) ->
    case z_template_compiler_runtime:to_simple_value(V, Context) of
        undefined -> undefined;
        V1 when V1 =:= V -> V;
        V1 -> translate(V1, Lang, Context)
    end.

