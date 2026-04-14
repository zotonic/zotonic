%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Filter a structure or resource and return a list of
%% all translated texts and properties.
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

-module(filter_translated_texts).
-moduledoc("
Check all properties of a map, list or resource. Returns a list of
all properties that are translated (have a `#trans{}` record) and their translations.

For example, if the input is a resource with the following properties:

```erlang
#{
    <<\"title\">> => #trans{tr = [{en, <<\"Hello\">>}, {nl, <<\"Hallo\">>}]},
    <<\"description\">> => <<\"A description\">>
}
```

Then the output will be:

```erlang
[
    {<<\"title\">>, #trans{tr = [{en, <<\"Hello\">>}, {nl, <<\"Hallo\">>}]} }
]
```

See also

[translation](/id/doc_template_filter_filter_translation)").

-export([
    translated_texts/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

translated_texts(undefined, _Context) ->
    [];
translated_texts(Id, Context) when is_integer(Id) ->
    All = m_rsc:get(Id, Context),
    translated_texts(All, Context);
translated_texts(Map, _Context) when is_map(Map) ->
    maps:fold(fun(Key, Value, Acc) ->
        case Value of
            #trans{} -> [{Key, Value} | Acc];
            _ -> Acc
        end
    end, [], Map);
translated_texts(List, _Context) when is_list(List) ->
    lists:filter(fun(Item) ->
        case Item of
            #trans{} -> true;
            _ -> false
        end
    end, List);
translated_texts(Other, Context) ->
    translated_texts(m_rsc:rid(Other, Context), Context).
