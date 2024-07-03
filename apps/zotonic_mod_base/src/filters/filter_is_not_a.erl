%% @author Paul Guyot <pguyot@kallisys.net>
%% @copyright 2011-2023 Paul Guyot
%% @doc 'is_not_a' filter, filters a list of ids or test if a single
%% resource is in a category.

%% Copyright 2011-2023 Paul Guyot
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

-module(filter_is_not_a).
-export([is_not_a/3, is_not_a/4]).


is_not_a(Arg, Cat, Context) when not is_integer(Cat), not is_atom(Cat) ->
    case m_category:name_to_id(Cat, Context) of
        {ok, CatId} ->
            is_not_a(Arg, CatId, Context);
        {error, _} when is_list(Arg); is_tuple(Arg) ->
            z_template_compiler_runtime:to_list(Arg, Context);
        {error, _} ->
            true
    end;
is_not_a(Rsc, Cat, Context) when is_integer(Rsc); is_atom(Rsc); is_binary(Rsc) ->
    % Single resource test.
    not m_rsc:is_a(Rsc, Cat, Context);
is_not_a(RscList, undefined, Context) ->
    z_template_compiler_runtime:to_list(RscList, Context);
is_not_a(RscList, Cat, Context) ->
    % Ensure argument is a list, return a list.
    z_list_of_ids_filter:filter(RscList, fun(Id) -> not m_rsc:is_a(Id, Cat, Context) end, Context).

is_not_a(List, Cat, N, Context) ->
    case m_category:name_to_id(Cat, Context) of
        {ok, CatId} -> z_list_of_ids_filter:filter(List, fun(Id) -> not m_rsc:is_a(Id, CatId, Context) end, N, Context);
        {error, _Reason} -> []
    end.
