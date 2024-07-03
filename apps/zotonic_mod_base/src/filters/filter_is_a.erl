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
