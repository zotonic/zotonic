%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-27
%%
%% @doc Template access for access control functions and state

%% Copyright 2009 Marc Worrell
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
m_find_value(user, #m{value=undefined}, Context) ->
    z_acl:user(Context);
m_find_value(is_admin, #m{value=undefined}, Context) ->
    z_acl:is_admin(Context);
m_find_value(authenticated, #m{value=undefined} = M, _Context) ->
    M#m{value=authenticated};
m_find_value(Action, #m{value=Auth} = M, _Context)
    when (Action == use orelse Action == admin orelse Action == view
    orelse Action == delete orelse Action == update orelse Action == insert
    orelse Action == link), (Auth =:= undefined orelse Auth =:= authenticated) ->
    M#m{value={is_allowed, Action, Auth}};
m_find_value(is_allowed, #m{value=Auth} = M, _Context) when Auth =:= undefined; Auth =:= authenticated ->
    M#m{value={is_allowed, Auth}};
m_find_value(Action, #m{value={is_allowed, Auth}} = M, _Context) ->
    M#m{value={is_allowed, Action, Auth}};
m_find_value(Object, #m{value={is_allowed, Action, authenticated}}, Context) when is_binary(Object) ->
    Context1 = case z_notifier:first(#acl_context_authenticated{}, Context) of
                    undefined -> Context;
                    Ctx -> Ctx
               end,
    z_acl:is_allowed(Action, z_convert:to_atom(Object), Context1);
m_find_value(Object, #m{value={is_allowed, Action, undefined}}, Context) when is_binary(Object) ->
    z_acl:is_allowed(Action, z_convert:to_atom(Object), Context);
m_find_value(Object, #m{value={is_allowed, Action, authenticated}}, Context) ->
    Context1 = case z_notifier:first(#acl_context_authenticated{}, Context) of
                    undefined -> Context;
                    Ctx -> Ctx
               end,
    z_acl:is_allowed(Action, Object, Context1);
m_find_value(Object, #m{value={is_allowed, Action, undefined}}, Context) ->
    z_acl:is_allowed(Action, Object, Context).


%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> List
m_to_list(_, _Context) ->
    [].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
    undefined.
