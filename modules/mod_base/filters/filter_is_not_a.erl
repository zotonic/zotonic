%% @author Paul Guyot <pguyot@kallisys.net>
%% @copyright 2011 Paul Guyot
%% @doc 'is_not_a' filter, filters a list of ids

%% Copyright 2011 Paul Guyot
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


is_not_a(Arg, Cat, Context) ->
    case m_category:name_to_id(Cat, Context) of
        {ok, CatId} -> z_list_of_ids_filter:filter(Arg, fun(Id) -> not m_rsc:is_a(Id, CatId, Context) end, Context);
        {error, _Reason} when is_integer(Arg) -> false;
        {error, _Reason} when is_list(Arg) -> []
    end.

is_not_a(List, Cat, N, Context) ->
    case m_category:name_to_id(Cat, Context) of
        {ok, CatId} -> z_list_of_ids_filter:filter(List, fun(Id) -> not m_rsc:is_a(Id, CatId, Context) end, N, Context);
        {error, _Reason} -> []
    end.
