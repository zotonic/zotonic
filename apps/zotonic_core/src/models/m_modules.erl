%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2025 Marc Worrell
%% @doc Model for the zotonic modules. List all modules, enabled or disabled.
%% @end

%% Copyright 2025 Marc Worrell
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
    m_get/3,
    all/1
]).

-include_lib("zotonic.hrl").



%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"all">> | Rest ], _Msg, Context) ->
    {ok, {all(Context), Rest}};
m_get([ <<"enabled">> | Rest ], _Msg, Context) ->
    {ok, {enabled(Context), Rest}};
m_get([ <<"disabled">> | Rest ], _Msg, Context) ->
    {ok, {disabled(Context), Rest}};
m_get([ <<"active">>, Module | Rest ], _Msg, Context) ->
    IsActive = lists:member(safe_to_atom(Module), active(Context)),
    {ok, {IsActive, Rest}};
m_get([ <<"info">>, Module | Rest ], _Msg, Context) ->
    case is_allowed(Context) of
        true ->
            M = safe_to_atom(Module),
            Info = z_module_manager:mod_info(Module),
            Info1 = Info#{
                is_enabled => lists:member(M, enabled(Context)),
                is_active => z_module_manager:active(M, Context)
            },
            {ok, {Info1, Rest}};
        false ->
            {error, eacces}
    end;
m_get([ <<"uninstalled">> ], _Msg, Context) ->
    Uninstalled = z_module_manager:get_uninstalled(Context),
    {ok, {Uninstalled, []}};
m_get([ <<"provided">>, Service | Rest ], _Msg, Context) ->
    M = safe_to_atom(Service),
    IsProvided = z_module_manager:is_provided(M, Context),
    {ok, {IsProvided, Rest}};
m_get([ <<"provided">> ], _Msg, Context) ->
    Provided = z_module_manager:get_provided(Context),
    {ok, {Provided, []}};
m_get([ <<"get_provided">> | Rest ], _Msg, Context) ->
    case is_allowed(Context) of
        true ->
            {ok, {z_module_manager:scan_provided(Context), Rest}};
        false ->
            {error, eacces}
    end;
m_get([ <<"get_depending">> | Rest ], _Msg, Context) ->
    case is_allowed(Context) of
        true ->
            {ok, {z_module_manager:scan_depending(Context), Rest}};
        false ->
            {error, eacces}
    end;
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.

is_allowed(Context) ->
    z_acl:is_admin(Context) orelse z_acl:is_allowed(use, mod_admin_modules, Context).

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
    [ {Name, z_module_manager:mod_title(Name)} || Name <- All ].

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
