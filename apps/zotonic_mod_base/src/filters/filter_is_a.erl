%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2023 Marc Worrell
%% @doc 'is_a' filter. Filters a list of ids on category, or tests a single resource id
%% if it is in a category.

%% Copyright 2010-2023 Marc Worrell
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

-module(filter_is_a).
-moduledoc("
See also

[is\\_not\\_a](/id/doc_template_filter_filter_is_not_a), [is\\_visible](/id/doc_template_filter_filter_is_visible), [filter](/id/doc_template_filter_filter_filter)

Filter a list of resource ids on category, or test if a single resource id belongs to a category.

This filter can be applied to a list of resource ids or a single resource id.

When it is applied to a list then it will filter the list of ids. Only those resource ids that belong to a certain
category remain. Optionally the filter only returns the first n matches.

When applied to a single integer (resource id), then it will return a boolean. True when the id belongs to the
parameter’s category, false otherwise.



Apply to a single resource id
-----------------------------

Example:


```django
{{ 1|is_a:\"person\"|yesno }}
```

Will output “yes”, because the resource with id 1 is a person (the System Administrator).



Apply to a list of resource ids
-------------------------------

When applied to a list of ids:


```django
{% for part_id in m.rsc[id].o.haspart|is_a:\"person\" %}
    {{ m.rsc[part_id].title }}
{% endfor %}
```

This will list all collection members that are a person. While:


```django
{% for part_id in m.rsc[id].o.haspart|is_a:\"person\":3 %}
    {{ m.rsc[part_id].title }}
{% endfor %}
```

Lists only the first three collection members that are a person.
").
-export([is_a/3, is_a/4]).

is_a(Arg, Cat, Context) when not is_integer(Cat), not is_atom(Cat) ->
    case m_category:name_to_id(Cat, Context) of
        {ok, CatId} ->
            is_a(Arg, CatId, Context);
        {error, _} when is_list(Arg); is_tuple(Arg) ->
            [];
        {error, _} ->
            false
    end;
is_a(Rsc, Cat, Context) when is_integer(Rsc); is_atom(Rsc); is_binary(Rsc) ->
    % Single resource test.
    m_rsc:is_a(Rsc, Cat, Context);
is_a(_, undefined, _Context) ->
    [];
is_a(RscList, Cat, Context) ->
    % Ensure argument is a list, return a list.
    z_list_of_ids_filter:filter(RscList, fun(Id) -> m_rsc:is_a(Id, Cat, Context) end, Context).

is_a(List, Cat, N, Context) ->
    case m_category:name_to_id(Cat, Context) of
        {ok, CatId} ->
            z_list_of_ids_filter:filter(List, fun(Id) -> m_rsc:is_a(Id, CatId, Context) end, N, Context);
        {error, _Reason} -> []
    end.
