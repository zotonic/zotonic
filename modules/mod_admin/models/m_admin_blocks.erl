%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013 Marc Worrell
%% @doc Zotonic: admin blocks model and interface

%% Copyright 2013 Marc Worrell
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

-module(m_admin_blocks).
-author("Marc Worrell <marc@worrell.nl>").

-include_lib("include/zotonic.hrl").


%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2
]).

%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(list, #m{value=undefined} = M, _Context) ->
    M#m{value=list};
m_find_value(Id, #m{value=list}, Context) ->
    lists:sort(z_notifier:foldr(#admin_edit_blocks{id=Id}, [], Context)).

%% @spec m_to_list(Source, Context) -> List
m_to_list(_, _Context) ->
    undefined.

%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
    undefined.
