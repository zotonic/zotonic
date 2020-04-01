%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020 Marc Worrell
%% @doc redirect to the next or previous resource in a category

%% Copyright 2020 Marc Worrell
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

-module(action_admin_redirect_incat).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Id = m_rsc:rid(proplists:get_value(id, Args), Context),
    CatId = m_rsc:rid(proplists:get_value(cat_id, Args), Context),
    IsPrev = z_convert:to_bool(proplists:get_value(is_prev, Args)),
    Postback = {admin_redirect_incat, Id, CatId, IsPrev},
    {PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
    {PostbackMsgJS, Context}.


%% @doc Find the next or previous page in the given category
event(#postback{message={admin_redirect_incat, Id, CatId, false}}, Context) ->
    {From, To} = m_category:get_range(CatId, Context),
    redirect(find_next(Id, From, To, Context), Id, CatId, Context);
event(#postback{message={admin_redirect_incat, Id, CatId, true}}, Context) ->
    {From, To} = m_category:get_range(CatId, Context),
    redirect(find_prev(Id, From, To, Context), Id, CatId, Context).

redirect(undefined, _Id, _CatId, Context) ->
    z_render:growl(?__("No other pages found", Context), Context);
redirect(Id, Id, _CatId, Context) ->
    z_render:growl(?__("No other pages found", Context), Context);
redirect(Id, _Id, CatId, Context) ->
    Vars = [
        {id, Id},
        {qcat, CatId}
    ],
    Url = z_dispatcher:url_for(admin_edit_rsc, Vars, Context),
    z_render:wire({redirect, [ {location, Url} ]}, Context).

find_next(Id, From, To, Context) ->
    Next = z_db:q1("
        select id
        from rsc
        where id > $1
          and pivot_category_nr >= $2
          and pivot_category_nr <= $3
        order by id
        limit 1",
        [ Id, From, To ],
        Context),
    maybe_find_first(Next, From, To, Context).

maybe_find_first(undefined, From, To, Context) ->
    z_db:q1("
        select id
        from rsc
        where pivot_category_nr >= $1
          and pivot_category_nr <= $2
        order by id
        limit 1",
        [ From, To ],
        Context);
maybe_find_first(Id, _From, _To, _Context) ->
    Id.

find_prev(Id, From, To, Context) ->
    Prev = z_db:q1("
        select id
        from rsc
        where id < $1
          and pivot_category_nr >= $2
          and pivot_category_nr <= $3
        order by id desc
        limit 1",
        [ Id, From, To ],
        Context),
    maybe_find_last(Prev, From, To, Context).

maybe_find_last(undefined, From, To, Context) ->
    z_db:q1("
        select id
        from rsc
        where pivot_category_nr >= $1
          and pivot_category_nr <= $2
        order by id desc
        limit 1",
        [ From, To ],
        Context);
maybe_find_last(Id, _From, _To, _Context) ->
    Id.
