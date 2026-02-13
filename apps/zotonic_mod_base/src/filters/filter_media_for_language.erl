%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2023-2026 Marc Worrell
%% @doc Filter a list of media items by their 'medium_language' property,
%% return the best matching with the current or given language. Only visible
%% media items are returned.
%% @end

%% Copyright 2023-2026 Marc Worrell
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

-module(filter_media_for_language).
-moduledoc("
See also

[show\\_media](/id/doc_template_filter_filter_show_media), [embedded\\_media](/id/doc_template_filter_filter_embedded_media), [without\\_embedded\\_media](/id/doc_template_filter_filter_without_embedded_media)

Filter a list of media items by their `medium_language` property, return the best matching with the current or given
language. Only visible media items are returned.

Let’s assume two media resources:

*   13925 with `medium_language` set to `en`
*   13926 with `medium_language` set to `nl`

If the current language is `nl` then:


```erlang
{% print [13926, 13925]|media_for_language %}
```

Will show:


```erlang
[13926]
```

But:


```erlang
{% print [13926, 13925]|media_for_language:\"en\" %}
```

Will show:


```erlang
[13925]
```

The filter tries to select the *best* matching language for the requested language. If there is no language requested,
then the current request language is used.

If a language could not be found then the normal *fallback* language lookup will apply, just as with text translations lookups.

For example, given the above two ids and a request language of `nl` then the following:


```erlang
{% print [13926, 13925]|media_for_language:\"de\" %}
```

Will print:


```erlang
[13926]
```

This is because there are no German translations. So the system fell back to the request language(s) and selected the
Dutch version.

This filter can be used to show all connected media that are in a certain language:


```erlang
{% for media_id in m.edge.o[id].depiction %}
   {% media media_id %}
{% endfor %}
```
").

-export([
    media_for_language/2,
    media_for_language/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

media_for_language(Ids, Context) ->
    media_1(Ids, Context).

media_for_language(Ids, Language, Context) ->
    Context1 = case z_language:to_language_atom(Language) of
        {ok, Code} ->
            z_context:set_language(Code, Context);
        {error, _} ->
            Context
    end,
    media_1(Ids, Context1).


media_1(Ids, Context) when is_list(Ids) ->
    LangIds = lists:filtermap(
        fun(Id) ->
            case m_rsc:rid(Id, Context) of
                undefined ->
                    false;
                RId ->
                    case z_acl:rsc_visible(RId, Context) of
                        true ->
                            MediaLang = m_rsc:p(RId, <<"medium_language">>, Context),
                            Lang = case z_language:to_language_atom(MediaLang) of
                                {ok, Code} -> Code;
                                {error, _} -> 'x-default'
                            end,
                            {true, {Lang, RId}};
                        false ->
                            false
                    end
            end
        end,
        Ids),
    Grouped = lists:foldr(
        fun({Code, Id}, Acc) ->
            Acc#{
                Code => [ Id | maps:get(Code, Acc, []) ]
            }
        end,
        #{},
        LangIds),
    Tr = #trans{ tr = maps:to_list(Grouped) },
    z_trans:lookup_fallback(Tr, Context);
media_1(Ids, Context) ->
    L = z_template_compiler_runtime:to_list(Ids, Context),
    media_1(L, Context).
