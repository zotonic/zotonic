%% @author Arjan Scherpenisse <marc@worrell.nl>
%% @copyright 2010 Arjan Scherpenisse
%% @doc group a list of sorted rsc ids on their first letter of the title. Then, split this list in #Cols more-or-less even columns.

%% Copyright 2010 Arjan Scherpenisse
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

-module(filter_group_title_firstchar).
-export([group_title_firstchar/3]).

-include("zotonic.hrl").

group_title_firstchar(undefined, _Cols, _Context) ->
    undefined;
group_title_firstchar(In, Cols, Context) ->
    Ids = erlydtl_runtime:to_list(In, Context),
    Max = round(length(Ids)/Cols)+1,
    First = [ [{first, firstchar_title(Id, Context)}, {id, Id}] || Id <- Ids],
    F = z_utils:group_by(First, first, Context),
    
    X = [ [{first, [proplists:get_value(first, hd(P))]}, 
           {result, [proplists:get_value(id, Pl) || Pl <- P]}]
          || P <- F],
    Grouped = grouped(X, Max),
    Grouped.



firstchar_title(Id, Context) ->
    case z_convert:to_list(z_trans:lookup_fallback(m_rsc:p(Id, title, Context), Context)) of
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

