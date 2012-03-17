%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-06-13
%% @doc Identity administration.  Adds overview of users to the admin and enables to add passwords on the edit page.

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

-module(mod_admin_identity).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Admin identity/user supports").
-mod_description("Adds an user overview and possibility to edit passwords.").
-mod_depends([admin]).
-mod_provides([]).


%% interface functions
-export([
    observe_search_query/2,
    observe_admin_menu/3
]).

-include("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").

observe_search_query({search_query, Req, OffsetLimit}, Context) ->
    search(Req, OffsetLimit, Context).

%%====================================================================
%% support functions
%%====================================================================

search({users, [{text,QueryText}]}, _OffsetLimit, Context) ->
    case QueryText of
        A when A == undefined orelse A == "" orelse A == <<>> ->
            #search_sql{
                select="r.id, max(r.modified) AS rank",
                from="rsc r join identity i on r.id = i.rsc_id",
                order="rank desc",
                group_by="r.id",
                tables=[{rsc,"r"}]
            };
        _ ->
            #search_sql{
                select="r.id, max(ts_rank_cd(pivot_tsv, query, 32)) AS rank",
                from="rsc r join identity i on r.id = i.rsc_id, plainto_tsquery($2, $1) query",
                where=" query @@ pivot_tsv",
                order="rank desc",
                group_by="r.id",
                args=[QueryText, z_pivot_rsc:pg_lang(Context#context.language)],
                tables=[{rsc,"r"}]
            }
    end;
search(_, _, _) ->
    undefined.


observe_admin_menu(admin_menu, Acc, Context) ->
    [
     #menu_item{id=admin_user,
                parent=admin_auth,
                label=?__("Users", Context),
                url={admin_user},
                visiblecheck={acl, use, mod_admin_identity}}
     
     |Acc].

