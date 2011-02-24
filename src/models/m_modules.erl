%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% Date: 2010-05-05
%%
%% @doc Model for the zotonic modules. List all modules, enabled or disabled.

%% Copyright 2010 Marc Worrell
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

-module(m_modules).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    all/1
]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(all, #m{value=undefined}, Context) ->
    all(Context);
m_find_value(enabled, #m{value=undefined}, Context) ->
    enabled(Context);
m_find_value(disabled, #m{value=undefined}, Context) ->
    disabled(Context);
m_find_value(info, #m{value=undefined} = M, _Context) ->
    M#m{value=info};
m_find_value(Module, #m{value=info}, Context) ->
    M = z_convert:to_atom(Module),
    [{enabled, lists:member(M, enabled(Context))},
     {active, z_module_manager:active(M, Context)},
     {title, z_module_manager:title(M)},
     {prio, z_module_manager:prio(M)}].


%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> List
m_to_list(#m{value=undefined}, Context) ->
    all(Context).

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, Context) ->
    all(Context).


%% @doc Return the list of modules
all(Context) ->
    All = lists:sort(z_module_manager:all(Context)),
    All1 = lists:filter(fun(M) -> z_module_manager:module_exists(M) end, All),
    [ {Name, z_module_manager:title(Name)} || Name <- All1 ].

enabled(Context) ->
    Mods = z_module_manager:active(Context),
    lists:filter(fun(M) -> z_module_manager:module_exists(M) end, Mods).

disabled(Context) ->
    All = z_module_manager:all(Context),
    Active = z_module_manager:active(Context),
    lists:filter(fun(M) -> (not lists:member(M, Active)) andalso z_module_manager:module_exists(M) end, All).

