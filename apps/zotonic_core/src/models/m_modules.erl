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

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/2,
    all/1
]).

-include_lib("zotonic.hrl").



%% @doc Fetch the value for the key from a model source
-spec m_get( list(), z:context() ) -> {term(), list()}.
m_get([ all | Rest ], Context) ->
    {all(Context), Rest};
m_get([ enabled | Rest ], Context) ->
    {enabled(Context), Rest};
m_get([ disabled | Rest ], Context) ->
    {disabled(Context), Rest};
m_get([ active, Module | Rest ], Context) ->
    IsActive = lists:member(safe_to_atom(Module), active(Context)),
    {IsActive, Rest};
m_get([ info, Module | Rest ], Context) ->
    M = safe_to_atom(Module),
    Info = [
        {enabled, lists:member(M, enabled(Context))},
        {active, z_module_manager:active(M, Context)},
        {title, z_module_manager:title(M)},
        {prio, z_module_manager:prio(M)}
    ],
    {Info, Rest};
m_get(Vs, _Context) ->
    lager:error("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {undefined, []}.

safe_to_atom(M) when is_atom(M) ->
    M;
safe_to_atom(B) when is_binary(B) ->
    try
        erlang:binary_to_existing_atom(B, utf8)
    catch
        error:badarg -> undefined
    end;
safe_to_atom(L) when is_list(L) ->
    try
        erlang:list_to_existing_atom(L)
    catch
        error:badarg -> undefined
    end.

%% @doc Return the list of modules
all(Context) ->
    All = lists:sort(z_module_manager:all(Context)),
    [ {Name, z_module_manager:title(Name)} || Name <- All ].

enabled(Context) ->
    case z_memo:get('m.enabled') of
        undefined -> z_memo:set('m.enabled', z_module_manager:active(Context), Context);
        V -> V
    end.

active(Context) ->
    case z_memo:get('m.active') of
        undefined -> z_memo:set('m.active', z_module_manager:get_modules(Context), Context);
        V -> V
    end.

disabled(Context) ->
    All = z_module_manager:all(Context),
    Active = z_module_manager:active(Context),
    All -- Active.
