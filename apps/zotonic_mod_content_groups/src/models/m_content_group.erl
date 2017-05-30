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

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,

    is_used/2
]).


-include_lib("zotonic_core/include/zotonic.hrl").

m_find_value(is_used, #m{value=undefined} = M, _Context) ->
    M#m{value=is_used};
m_find_value(CGId, #m{value=is_used}, Context) ->
    is_used(CGId, Context);
m_find_value(_Key, _Value, _Context) ->
    undefined.

m_to_list(#m{value=undefined}, _Context) ->
    [].

m_value(#m{value=undefined}, _Context) ->
    undefined.

%% @doc Check if a content group is actually in use.
is_used(ContentGroup, Context) ->
    CGId = m_rsc:rid(ContentGroup, Context),
    Ids = [ CGId | m_hierarchy:children('content_group', CGId, Context) ],
    lists:any(fun(Id) ->
                  z_db:q1("select id from rsc where content_group_id = $1", [Id], Context) =/= undefined
              end,
              Ids).

