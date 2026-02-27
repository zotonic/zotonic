%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc Return the direction string "ltr" or "rtl" depending on the language.
%% Returns "" for the undefined language.

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

-module(filter_language_dir).
-moduledoc("
Return `rtl` or `ltr` depening on the direction of the language.

Example:


```django
<div dir=\"{{ z_language|language_dir }}\">
    {_ Text _}
</div>
```

Input can also be an resource (page) id. In that case the language that the resource is shown in will be used to
determine the direction.

Example:


```django
<div lang=\"{{ id|language }}\" dir=\"{{ id|language_dir }}\">
    {{ id.body }}
</div>
```

It currently returns `rtl` for Arabic (`ar`), Farsi (`fa`) and Hebrew (`he`).

See also

[language](/id/doc_template_filter_filter_language), [is_rtl](/id/doc_template_filter_filter_is_rtl)
").
-export([
    language_dir/2
]).


language_dir(undefined, _Context) ->
    <<>>;
language_dir([], _Context) ->
    <<>>;
language_dir(Id, Context) when is_integer(Id) ->
    language_dir(filter_language:language(Id, Context), Context);
language_dir([Lang|_] = Langs, Context) when is_atom(Lang) ->
    language_dir(filter_language:language(Langs, Context), Context);
language_dir(LanguageCode, _Context) ->
    dir(LanguageCode).

dir(Code) ->
    case is_rtl(Code) of
        true -> <<"rtl">>;
        false -> <<"ltr">>
    end.

is_rtl(LanguageCode) when is_binary(LanguageCode); is_atom(LanguageCode) ->
    z_language:is_rtl(LanguageCode);
is_rtl(LanguageCode) ->
    is_rtl(z_convert:to_binary(LanguageCode)).

