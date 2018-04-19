%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2017 Marc Worrell
%% @doc Template access for access control functions and state

%% Copyright 2009-2017 Marc Worrell
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

-module(m_acl).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/2
]).

-include_lib("zotonic.hrl").

-define(is_action(A), A =:= use orelse A =:= admin orelse A =:= view
    orelse A =:= delete orelse A =:= update orelse A =:= insert
    orelse A =:= link).

-spec m_get( list(), z:context()) -> {term(), list()}.
m_get([ user | Rest ], Context) -> {z_acl:user(Context), Rest};
m_get([ is_admin | Rest ], Context) -> {z_acl:is_admin(Context), Rest};

% Check if current user is allowed to perform an action on some object
m_get([ Action, Object | Rest ], Context) when ?is_action(Action), is_binary(Object) ->
    {is_allowed_to_atom(Action, Object, Context), Rest};
m_get([ Action, Object | Rest ], Context) when ?is_action(Action) ->
    {z_acl:is_allowed(Action, Object, Context), Rest};
m_get([ is_allowed, Action, Object | Rest ], Context) when ?is_action(Action), is_binary(Object) ->
    {is_allowed_to_atom(Action, Object, Context), Rest};
m_get([ is_allowed, Action, Object | Rest ], Context) when ?is_action(Action) ->
    {z_acl:is_allowed(Action, Object, Context), Rest};

% Check if an authenticated (default acl setttings) is allowed to perform an action on some object
m_get([ authenticated, Action, Object | Rest ], Context) when ?is_action(Action), is_binary(Object) ->
    {is_allowed_authenticated_to_atom(Action, Object, Context), Rest};
m_get([ authenticated, Action, Object | Rest ], Context) when ?is_action(Action) ->
    Context1 = case z_notifier:first(#acl_context_authenticated{}, Context) of
                    undefined -> Context;
                    Ctx -> Ctx
               end,
    {z_acl:is_allowed(Action, Object, Context1), Rest};
m_get([ authenticated, is_allowed, Action, Object | Rest ], Context)  when ?is_action(Action), is_binary(Object) ->
    {is_allowed_authenticated_to_atom(Action, Object, Context), Rest};
m_get([ authenticated, is_allowed, Action, Object | Rest ], Context) when ?is_action(Action) ->
    Context1 = case z_notifier:first(#acl_context_authenticated{}, Context) of
                    undefined -> Context;
                    Ctx -> Ctx
               end,
    {z_acl:is_allowed(Action, Object, Context1), Rest};

% Error, unknown lookup.
m_get(Vs, _Context) ->
    lager:error("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {undefined, []}.



is_allowed_to_atom(Action, Object, Context) ->
    try
        ObjectAtom = erlang:binary_to_existing_atom(Object, utf8),
        z_acl:is_allowed(Action, ObjectAtom, Context)
    catch
        error:badarg -> false
    end.

is_allowed_authenticated_to_atom(Action, Object, Context) ->
    try
        ObjectAtom = erlang:binary_to_existing_atom(Object, utf8),
        Context1 = case z_notifier:first(#acl_context_authenticated{}, Context) of
                        undefined -> Context;
                        Ctx -> Ctx
                   end,
        z_acl:is_allowed(Action, ObjectAtom, Context1)
    catch
        error:badarg -> false
    end.

