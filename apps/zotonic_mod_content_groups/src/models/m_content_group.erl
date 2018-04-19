%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015 Marc Worrell
%%
%% @doc Model for content group memberships.

%% Copyright 2015 Marc Worrell
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

-module(m_content_group).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/2,

    is_used/2
]).


-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), z:context() ) -> {term(), list()}.
m_get([ is_used, CGId | Rest ], Context) ->
    {is_used(CGId, Context), Rest};
m_get(Vs, _Context) ->
    lager:error("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {undefined, []}.

%% @doc Check if a content group is actually in use.
is_used(ContentGroup, Context) ->
    CGId = m_rsc:rid(ContentGroup, Context),
    Ids = [ CGId | m_hierarchy:children('content_group', CGId, Context) ],
    lists:any(fun(Id) ->
                  z_db:q1("select id from rsc where content_group_id = $1", [Id], Context) =/= undefined
              end,
              Ids).

