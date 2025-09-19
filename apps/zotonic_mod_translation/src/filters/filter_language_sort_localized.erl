%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc Sort a list or map of languages by their localized name.

%% Copyright 2025 Marc Worrell
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

-module(filter_language_sort_localized).
-moduledoc("
Sort a list of language codes or map with languages on their localized name in the currently selected language. This is
useful for editorial interfaces where editors pick a language from a list.

Do not use for end-users wanting to select their own language, they might not be able to understand the translated
language names.

Return a list of `{Code, LanguageProps}` pairs.

LanguageProps is a map:


```erlang
#{
    fallback => [],
    language => <<\"nl\">>,
    language_atom => nl,
    name => <<\"Nederlands\">>,
    name_en => <<\"Dutch\">>,
    sort_key => <<\"dutch\">>,
    sublanguages => [
    ]
}
```

After sorting the key `name_localized` will be added to the map:


```erlang
#{
    fallback => [],
    language => <<\"nl\">>,
    language_atom => nl,
    name => <<\"Nederlands\">>,
    name_en => <<\"Dutch\">>,
    name_localized => <<\"Nederländska\"/utf8>>,
    sort_key => <<\"dutch\">>,
    sublanguages => [
    ]
}
```
").
-export([
    language_sort_localized/2
]).

language_sort_localized(undefined, _Context) ->
    [];
language_sort_localized(V, Context) ->
    z_language:sort_localized(V, Context).
