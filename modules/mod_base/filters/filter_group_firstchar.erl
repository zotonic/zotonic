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
-export([group_firstchar/2,group_firstchar/3,group_firstchar/4]).

-include("zotonic.hrl").

group_firstchar(List, Context) ->
    group_firstchar(List, title, 1, Context).
group_firstchar(List, Column, Context) ->
    group_firstchar(List, Column, 1, Context).

group_firstchar(undefined, _Field, _Cols, _Context) ->
    undefined;
group_firstchar(In, Field, Cols, Context) ->
    Ids = erlydtl_runtime:to_list(In, Context),
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

