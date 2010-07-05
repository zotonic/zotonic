%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-07-18
%% @doc Adds support to the admin for easier editing of events.

%% Copyright 2009 Marc Worrell
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

-module(mod_admin_event).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Admin event support").
-mod_description("Easier creation and editing of events.").
-mod_prio(500).

%% interface functions
-export([
    observe_search_query/2
]).

-include_lib("zotonic.hrl").

observe_search_query({search_query, Req, OffsetLimit}, Context) ->
    search(Req, OffsetLimit, Context).

    %% @doc Return the list of events for a performer, ordered on start date
    %% @spec search(SearchSpec, Range, Context) -> #search_sql{}
    search({event_for_performer, [{id, Id}]}, _OffsetLimit, Context) ->
        PredPerfomerId = m_predicate:name_to_id_check(performer, Context),
        #search_sql{
            select="r.id",
            from="rsc r, edge e",
            where="r.id = e.subject_id and e.predicate_id = $1 and e.object_id = $2",
            order="r.pivot_date_start asc",
            tables=[{rsc,"r"}],
            args=[PredPerfomerId, Id],
            cats=[{"r", event}]
        };
    search(_, _, _) ->
        undefined.

