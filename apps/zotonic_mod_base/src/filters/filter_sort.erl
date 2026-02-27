%% @author Andreas Stenius <git@astekk.se>
%% @copyright 2012-2024 Andreas Stenius
%% @doc 'sort' filter, sort a resource id list on property.
%% @end

%% Copyright 2012-2024 Andreas Stenius
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

-module(filter_sort).
-moduledoc("
The sort filter takes a list of items to sort. Items can be an ordinary list of terms, property lists, or maps. It can
also be a list of resource ids to be filtered based on their properties. Sort order and properties to sort on are given
as arguments to the filter.

By default it sorts the list in ascending order, and resource lists are sorted on their id if no property is specified.

Example:


```django
{{ [4, 6, 2, 3, 5]|sort }}
```

Sorts the list of numbers in ascending order.

Example:


```django
{{ [4, 6, 2, 3, 5]|sort:'desc' }}
```

Sorts the list of numbers in descending order.

Example:


```django
{% for r in id.o.author|sort:['title', 'desc', 'modified'] %}
   do something with `r`...
{% endfor %}
```

This will sort on title in ascending order first, then on modified in descending order. Any number of properties may be
added, each one can have it’s own sort order, or use the current one.

See [m_rsc](/id/doc_model_model_rsc) for a list of properties available to sort on.

Sort order may be either ascending or descending (may be abbreviated as asc, +, desc, - or as string version of those).
").
-export([sort/2, sort/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

sort(undefined, _Context) ->
    undefined;
sort(Input, Context) ->
    lists:sort(make_input_list(Input, Context)).

sort(undefined, _Arg, _Context) ->
    undefined;
sort(Input, undefined, _Context) ->
    Input;
sort(Input, SortArg, Context) ->
    InputList = make_input_list(Input, Context),
    SortArgs = make_args_list(SortArg),
    SortArgs1 = map_args(SortArgs),
    SortArgs2 = lists:filter(
            fun
                (asc) -> false;
                (desc) -> false;
                (_) -> true
            end,
            SortArgs1),
    List1 = fetch_props(SortArgs2, InputList, Context),
    Sorted = lists:sort(List1),
    SortedInput = [ P || {_,P} <- Sorted ],
    case lists:member(desc, SortArgs1) of
        true -> lists:reverse(SortedInput);
        false -> SortedInput
    end.


make_args_list(Args) when is_list(Args) -> Args;
make_args_list(Arg) -> [Arg].

make_input_list(L, _Context) when is_list(L) -> L;
make_input_list(#rsc_list{ list = L }, _Context) -> L;
make_input_list(#rsc_tree{ tree = L }, _Context) -> L;
make_input_list(Map, _Context) when is_map(Map) -> maps:to_list(Map);
make_input_list(In, Context) -> z_template_compiler_runtime:to_list(In, Context).

%% Internal functions

fetch_props([], List, _Context) ->
    [ {A,A} || A <- List ];
fetch_props(Ps, List, Context) ->
    lists:map(
        fun(A) ->
            PVals = [ fetch_prop(A, P, Context) || P <- Ps ],
            {PVals, A}
        end,
        List).

fetch_prop(Id, P, Context) when is_integer(Id); is_atom(Id); is_binary(Id) ->
    case m_rsc:p(Id, P, Context) of
        #trans{} = Tr ->
            z_string:to_lower(z_trans:lookup_fallback(Tr, Context));
        B when is_binary(B) ->
            z_string:to_lower(B);
        V ->
            V
    end;
fetch_prop(#rsc_tree{ id = Id }, P, Context) ->
    fetch_prop(Id, P, Context);
fetch_prop(L, P, _Context) when is_list(L) ->
    proplists:get_value(P, L);
fetch_prop(M, P, _Context) when is_map(M) ->
    maps:get(P, M, undefined);
fetch_prop(_, _, _Context) ->
    undefined.


map_args(Args) ->
    lists:map(fun map_arg/1, Args).

map_arg(L) when is_list(L) -> map_arg(list_to_binary(L));
map_arg(<<"asc">>) -> asc;
map_arg(<<"ascending">>) -> asc;
map_arg(<<"+">>) -> asc;
map_arg(ascending) -> asc;
map_arg('+') -> asc;
map_arg(<<"desc">>) -> desc;
map_arg(<<"descending">>) -> desc;
map_arg(<<"-">>) -> desc;
map_arg(descending) -> desc;
map_arg('-') -> desc;
map_arg(A) when is_atom(A) -> A;
map_arg(B) ->
    try
        z_convert:to_binary(B, utf8)
    catch
        _:_ -> undefined
    end.
