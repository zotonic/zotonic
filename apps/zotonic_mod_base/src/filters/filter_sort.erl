%% @author Andreas Stenius <git@astekk.se>
%% @copyright 2012 Andreas Stenius
%% @doc 'sort' filter, sort a resource id list on property.

%% Copyright 2012 Andreas Stenius
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


make_args_list(Args) when is_list(Args) ->
    case z_string:is_string(Args) of
        true -> [Args];
        false -> Args
    end;
make_args_list(Arg) ->
    [Arg].

make_input_list(L, _Context) when is_list(L) -> L;
make_input_list(#rsc_list{ list = L }, _Context) -> L;
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

fetch_prop(A, P, Context) when is_integer(A); is_atom(A); is_binary(A) ->
    case m_rsc:p(A, P, Context) of
        {trans, _} = Tr ->
            z_string:to_lower(z_trans:lookup_fallback(Tr, Context));
        B when is_binary(B) ->
            z_string:to_lower(B);
        V ->
            V
    end;
fetch_prop(L, P, _Context) when is_list(L) ->
    proplists:get_value(P, L);
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
        erlang:binary_to_existing_atom(B, utf8)
    catch
        _:_ -> undefined
    end.
