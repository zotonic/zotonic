%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Select the language from a list of languages

%% Copyright 2011 Marc Worrell
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

-module(filter_language).
-moduledoc("
See also

[language\\_dir](/id/doc_template_filter_filter_language_dir), [is\\_rtl](/id/doc_template_filter_filter_is_rtl)

Return the language the resource (or translated text) will be displayed in.

Example:


```erlang
{{ id|language }}
```

The languages of the resource will be fetched and using the currently selected interface language (variable
`z_language`) the language for the resource to be displayed in will be returned.
").
-export([
    language/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

language(undefined, Context) ->
    z_context:language(Context);
language(B, Context) when is_binary(B) ->
    z_context:language(Context);
language(Id, Context) when is_integer(Id) ->
    language(m_rsc:p_no_acl(Id, language, Context), Context);
language(LanguageCode, _Context) when is_atom(LanguageCode) ->
    LanguageCode;
language({trans, Tr}, Context) ->
    language([ L || {L,_} <- Tr ], Context);
language([], Context) ->
    z_context:language(Context);
language(Langs, Context) ->
    z_trans:lookup_fallback_language(Langs, z_context:language(Context), Context).
