%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell
%% @doc Template access for access control functions and state
%% @end

%% Copyright 2009-2024 Marc Worrell
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
    m_get/3
]).

-include_lib("zotonic.hrl").


-spec m_get( list(), zotonic_model:opt_msg(), z:context()) -> zotonic_model:return().
m_get([ <<"user">> | Rest ], _Msg, Context) -> {ok, {z_acl:user(Context), Rest}};
m_get([ <<"is_admin">> | Rest ], _Msg, Context) -> {ok, {z_acl:is_admin(Context), Rest}};
m_get([ <<"is_admin_editable">> | Rest ], _Msg, Context) -> {ok, {z_acl:is_admin_editable(Context), Rest}};
m_get([ <<"is_read_only">> | Rest ], _Msg, Context) -> {ok, {z_acl:is_read_only(Context), Rest}};

% Check if current user is allowed to perform an action on some object
m_get([ <<"is_allowed">>, <<"link">>, Subject, Predicate, Object | Rest ], _Msg, Context) ->
    {ok, {z_acl:is_allowed_link(Subject, Predicate, Object, Context), Rest}};
m_get([ <<"is_allowed">>, Action, Object | Rest ], _Msg, Context) ->
    {ok, {is_allowed(Action, Object, Context), Rest}};

% Check if an authenticated (default acl setttings) is allowed to perform an action on some object
m_get([ <<"authenticated">>, Action, Object | Rest ], _Msg, Context) ->
    {ok, {is_allowed_authenticated(Action, Object, Context), Rest}};
m_get([ <<"authenticated">>, <<"is_allowed">>, Action, Object | Rest ], _Msg, Context) ->
    {ok, {is_allowed_authenticated(Action, Object, Context), Rest}};

% Shortcut, should use is_allowed/action/object
m_get([ <<"link">>, Subject, Predicate, Object | Rest ], _Msg, Context) ->
    {ok, {z_acl:is_allowed_link(Subject, Predicate, Object, Context), Rest}};
m_get([ Action, Object | Rest ], _Msg, Context) ->
    {ok, {is_allowed(Action, Object, Context), Rest}};

% Error, unknown lookup.
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.

is_allowed(Action, Object, Context) ->
    try
        ActionAtom = erlang:binary_to_existing_atom(Action, utf8),
        Object1 = maybe_value(Object),
        z_acl:is_allowed(ActionAtom, Object1, Context)
    catch
        error:badarg -> false
    end.

is_allowed_authenticated(Action, Object, Context) ->
    try
        ActionAtom = erlang:binary_to_existing_atom(Action, utf8),
        Context1 = case z_notifier:first(#acl_context_authenticated{}, Context) of
                        undefined -> Context;
                        Ctx -> Ctx
                   end,
        Object1 = maybe_value(Object),
        z_acl:is_allowed(ActionAtom, Object1, Context1)
    catch
        error:badarg -> false
    end.


maybe_value(<<>>) ->
    undefined;
maybe_value(<<C, _/binary>> = B) when C >= $0, C =< $9 ->
    % Assume some integer or resource name
    try
        binary_to_integer(B)
    catch
        _:_ -> B
    end;
maybe_value(B) when is_binary(B) ->
    try
        binary_to_existing_atom(B, utf8)
    catch
        _:_ -> B
    end;
maybe_value(V) ->
    V.

