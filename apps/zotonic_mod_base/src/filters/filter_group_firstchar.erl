%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse

%% @doc group a list of sorted rsc ids on their first letter of the
%% title or another rsc property. Then, split this list in a number of
%% more-or-less even columns.

%% Copyright 2011 Arjan Scherpenisse
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

-module(filter_group_firstchar).
-moduledoc("
Group a list of sorted [resource](/id/doc_model_model_rsc) ids on their first letter of the title or another rsc
property. After grouping, it splits this list in a number of more-or-less even columns.

This is useful for displaying multiple columns of alphabetically sorted pages, in which the pages are grouped by the
first letter, for instance a search result.

For instance, this piece of template code:


```django
<table>
{% for cols in m.search[{query cat=\"country\" sort=\"pivot_title\"}]|group_firstchar:\"title\":3 %}
   <td>
      {% for group in cols %}
      <b>{{ group.first }}</b>
      {% for id in group.result %}
      <li>
          {{ id.title }}
      </li>
      {% endfor %}
   {% endfor %}
   </td>
{% endfor %}
</table>
```

Groups the list of ids by title in three columns. It then uses nested for-loops to render a table similar to this:


```django
+----------------+----------------+----------------+
|A               |D               |G               |
| Afghanistan    | Denmark        | Georgia        |
| Azerbeidjan    |                | Germany        |
|                |E               | Grenada        |
|B               | Ecuador        | Guadeloupe     |
| Belgium        | Egypt          | Greenland      |
| Belarus        | Eritrea        | Guinea         |
| Brazil         | Estonia        |                |
|                |                |H               |
|C               |F               | Haiti          |
| Canada         | Fiji           |                |
|                | Finland        |                |
+----------------+----------------+----------------+
```

As you can see, all three columns have approximately the same size, although the size of the indiviual groups differs.

When no nr. of columns is given, the groups are returned in a single column.

See also

[group_title_firstchar](/id/doc_template_filter_filter_group_title_firstchar)").
-export([group_firstchar/2,group_firstchar/3,group_firstchar/4]).

-include_lib("zotonic_core/include/zotonic.hrl").

group_firstchar(List, Context) ->
    group_firstchar(List, title, 1, Context).
group_firstchar(List, Column, Context) ->
    group_firstchar(List, Column, 1, Context).

group_firstchar(undefined, _Field, _Cols, _Context) ->
    undefined;
group_firstchar(In, Field, Cols, Context) ->
    Ids = z_template_compiler_runtime:to_list(In, Context),
    Max = round(length(Ids)/Cols)+1,
    First = [ [{first, firstchar(Id, Field, Context)}, {id, Id}] || Id <- Ids],
    F = z_utils:group_by(First, first, Context),

    X = [ [{first, [proplists:get_value(first, hd(P))]},
           {result, [proplists:get_value(id, Pl) || Pl <- P]}]
          || P <- F],
    Grouped = grouped(X, Max),
    Grouped.


firstchar(Id, Field, Context) ->
    case z_convert:to_list(z_trans:lookup_fallback(m_rsc:p(Id, Field, Context), Context)) of
        [] -> undefined;
        L -> z_string:first_char(L)
    end.


grouped(GroupedList, Max) ->
    grouped(GroupedList, [], 0, [], Max).

grouped([], Cur, _CurLen, Acc, _Max) ->
    lists:reverse([lists:reverse(Cur)|Acc]);

grouped([Part | Rest], Cur, CurLen, Acc, Max) ->
    L = length(proplists:get_value(result, Part)),
    New = [Part|Cur],
    case CurLen + L >= Max of
        true ->
            %% begin new group
            grouped(Rest, [], 0, [lists:reverse(New)|Acc], Max);
        false ->
            %% Add to current group
            grouped(Rest, New, CurLen+L, Acc, Max)
    end.

