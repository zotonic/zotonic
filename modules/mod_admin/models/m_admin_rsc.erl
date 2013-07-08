%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2013 Arjan Scherpenisse
%% @doc Zotonic: administrative functions

%% Copyright 2013 Arjan Scherpenisse
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

-module(m_admin_rsc).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include_lib("include/zotonic.hrl").


%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2
]).

%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(pivot_queue_count, #m{value=undefined}, Context) ->
    pivot_queue_count(Context).

%% @spec m_to_list(Source, Context) -> List
m_to_list(_, _Context) ->
    undefined.

%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
    undefined.


pivot_queue_count(Context) ->
    z_db:q1("SELECT COUNT(*) FROM rsc_pivot_queue", Context).
