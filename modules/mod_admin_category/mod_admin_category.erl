%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2011 Marc Worrell
%% @doc Module for editing and managing categories.

%% Copyright 2009-2011 Marc Worrell
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

-module(mod_admin_category).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Admin category support").
-mod_description("Support editing and changing the category hierarchy.").
-mod_prio(600).
-mod_depends([admin]).
-mod_provides([]).

-export([
    observe_category_hierarchy_save/2
]).

-include_lib("zotonic.hrl").


observe_category_hierarchy_save(#category_hierarchy_save{tree=New}, Context) ->
    % Rebuild all parent/child relationship.
    % 1. Check if any missing. If so delete
    % 2. See if any parent/child is changed, if so update
    % 3. If any updates -> renumber
    case m_category:menu(Context) of
        New -> 
            nop;
        Old ->
            % Check if any ids are added or deleted
            NewIds = lists:sort(flatten(New, [])),
            OldIds = lists:sort(flatten(Old, [])),
            Deleted = OldIds -- NewIds,
            % Inserted = NewIds -- OldIds,
            lists:map(fun(Id) ->
                        m_category:delete(Id, undefined, Context)
                      end,
                      Deleted),
            
            % Get all pairs (parent, child)
            NewPairs = pair(New),
            OldPairs = pair(Old),
            Diff = NewPairs -- OldPairs,
            m_category:move(Diff, Context),
            ok
    end.


flatten([], Acc) ->
    Acc;
flatten([{Id,Sub}|Rest], Acc) ->
    Acc1 = flatten(Sub, Acc),
    flatten(Rest, [Id|Acc1]).


pair(Tree) ->
    pair(undefined, 1, Tree, []).

pair(_P, _N, [], Acc) ->
    Acc;
pair(P, N, [{Id, Sub}|Rest], Acc) ->
    Acc1 = pair(Id, 1, Sub, Acc),
    pair(P, N+1, Rest, [{Id,P,N}|Acc1]).

