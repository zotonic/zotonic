%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2012 Maas-Maarten Zeeman
%% Date: 2012-03-18
%%
%% @doc Model for using the zotonic notifier. Use map and first inside your templates.

%% Copyright 2012 Maas-Maarten Zeeman
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

-module(m_notifier).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2
]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(first, #m{value=undefined}=M, _Context) ->
    M#m{value=first};
m_find_value(map, #m{value=undefined}=M, _Context) ->
    M#m{value=map};
m_find_value(Message, #m{value=first}, Context) ->
    z_notifier:first(Message, Context);
m_find_value(Message, #m{value=map}, Context) ->
    z_notifier:map(Message, Context).

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> List
m_to_list(#m{value=undefined}, _Context) ->
	[].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
	undefined.

