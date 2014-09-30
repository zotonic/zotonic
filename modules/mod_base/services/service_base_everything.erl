%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% Date: 2009-10-03
%% @doc Retrieve the list of all objects in the system.

%% Copyright 2009 Arjan Scherpenisse
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

-module(service_base_everything).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-svc_title("Retrieve the list of all objects in the system.").
-svc_needauth(false).

-export([process_get/2]).

-include_lib("zotonic.hrl").

-define(IDS_PAGE_LENGTH, 100).

process_get(_ReqData, Context) ->
    PageNr = get_page_nr(Context),
    Ids = get_ids(z_acl:user(Context) =:= undefined, PageNr, Context),
    Ids1 = lists:filter(
        fun(Id) ->
            z_acl:rsc_visible(Id, Context)
        end,
        Ids),
    z_convert:to_json({array, Ids1}).

get_ids(true, PageNr, Context) ->
    Ids = z_db:q("SELECT id
                  FROM rsc 
                  WHERE visible_for = 0
                    AND is_published
                    AND publication_start <= now()
                    AND publication_end >= now()
                  ORDER BY id 
                  LIMIT $1
                  OFFSET $2",
                 [?IDS_PAGE_LENGTH, (PageNr-1) * ?IDS_PAGE_LENGTH],
                 Context),
    [ Id || {Id} <- Ids ];
get_ids(false, PageNr, Context) ->
    Ids = z_db:q("SELECT id
                  FROM rsc 
                  ORDER BY id 
                  LIMIT $1
                  OFFSET $2",
                 [?IDS_PAGE_LENGTH, (PageNr-1) * ?IDS_PAGE_LENGTH],
                 Context),
    [ Id || {Id} <- Ids ].

get_page_nr(Context) ->
    case z_context:get_q("page", Context) of
        "" -> 1;
        <<>> -> 1;
        undefined -> 1;
        N ->
            try
                erlang:max(1, z_convert:to_integer(N))
            catch
                _:_ ->
                    1
            end
    end.

