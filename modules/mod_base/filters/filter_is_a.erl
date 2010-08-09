%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'is_a' filter, filters a list of ids

%% Copyright 2010 Marc Worrell
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


is_a(Id, Cat, Context) when is_integer(Id)->
    case m_category:name_to_id(Cat, Context) of
        {ok, CatId} -> m_rsc:is_a(Id, CatId, Context);
        {error, _Reason} -> false
    end;
is_a([], _Cat, _Context) ->
    [];
is_a(List, Cat, Context) ->
    case m_category:name_to_id(Cat, Context) of
        {ok, CatId} -> lists:filter(fun(Id) -> m_rsc:is_a(Id, CatId, Context) end, erlydtl_runtime:to_list(List, Context));
        {error, _Reason} -> []
    end.

is_a(List, Cat, N, Context) ->
    case m_category:name_to_id(Cat, Context) of
        {ok, CatId} -> take_first(erlydtl_runtime:to_list(List, Context), CatId, N, Context, []);
        {error, _Reason} -> []
    end.

take_first([], _CatId, _N, _Context, Acc) ->
    lists:reverse(Acc);
take_first(_List, _CatId, 0, _Context, Acc) ->
    lists:reverse(Acc);
take_first([Id|Rest], CatId, N, Context, Acc) ->
    case m_rsc:is_a(Id, CatId, Context) of
        true -> take_first(Rest, CatId, N-1, Context, [Id|Acc]);
        false -> take_first(Rest, CatId, N, Context, Acc)
    end.
