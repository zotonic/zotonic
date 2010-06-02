%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse <arjan@scherpenisse.net>
%% @date 2010-06-01
%%
%% @doc Model for log messages.

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

-module(m_log).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    get/2
]).


-include_lib("zotonic.hrl").


m_find_value(Index, #m{value=undefined} = M, _Context) ->
    get(Index, _Context).


%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context)
m_to_list(#m{value=undefined}, Context) ->
    list(Context);
m_to_list(#m{value={log, Id}}, Context) ->
    get(Id, Context);
m_to_list(_, _Context) ->
    [].


m_value(#m{value=#m{value={log, Id}}}, Context) ->
    get(Id, Context).


get(Id, Context) ->
    {ok, R} = z_db:select(log, Id, Context),
    R.


list(Context) ->
    All = z_db:assoc("SELECT * FROM log ORDER BY created DESC", Context),
    [merge_props(R) || R <- All].

merge_props(R) ->
    proplists:delete(props, R) ++ proplists:get_value(props, R, []).


