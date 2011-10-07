%% @author Atilla Erdodi <atilla.erdodi@gmail.com>
%% @copyright 2011 Maximonster Interactive Things
%% Date: 2011-08-25
%%
%% @doc Model for the accessing the page variables. Exposes z_context:get_page/2

%% Copyright 2011 Maximonster Interactive Things
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

-module(m_page).
-author("Atilla Erdodi <atilla.erdodi@gmail.com").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2
]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_find_value(Key::term(), Source::#m{}, Context::#context{}) -> term().
m_find_value(Key, #m{value=undefined}, Context) ->
    z_context:get_page(Key, Context).

-spec m_to_list(#m{}, #context{}) -> [].
m_to_list(#m{value=undefined}, _Context) ->
    [].

-spec m_value(#m{}, #context{}) -> [].
m_value(#m{value=undefined}, _Context) ->
    [].

