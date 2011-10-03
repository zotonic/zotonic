%% @author Paul Guyot <pguyot@kallisys.net>
%% @copyright 2011 Paul Guyot
%% @doc common code for is_* filters that work on lists of ids.

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

-module(z_list_of_ids_filter).
-export([filter/3, filter/4]).

-spec filter(integer() | [integer()], fun((integer()) -> boolean()), any()) -> boolean() | [integer()].
filter(Id, Fun, _Context) when is_integer(Id) ->
    Fun(Id);
filter(List, Fun, Context) ->
    lists:filter(Fun, erlydtl_runtime:to_list(List, Context)).

-spec filter([integer()], fun((integer()) -> boolean()), integer(), any()) -> [integer()].
filter(List, Fun, N, Context) ->
    filter1(erlydtl_runtime:to_list(List, Context), Fun, N, []).

filter1([], _Fun, _N, Acc) ->
    lists:reverse(Acc);
filter1(_List, _Fun, 0, Acc) ->
    lists:reverse(Acc);
filter1([Id|Rest], Fun, N, Acc) ->
    case Fun(Id) of
        true -> filter1(Rest, Fun, N-1, [Id|Acc]);
        false -> filter1(Rest, Fun, N, Acc)
    end.

