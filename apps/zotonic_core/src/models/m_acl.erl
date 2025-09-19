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
-moduledoc("
The m\\_acl model gives access the id of the currently logged in user, and provides a mechanism to do basic access
control checks.

The following m\\_acl model properties are available in templates:

| Property                                       | Description                                                                      |
| ---------------------------------------------- | -------------------------------------------------------------------------------- |
| user                                           | Returns the current user id. If not logged in, this returns `undefined`.         |
| is\\\\_admin                                     | Check if the current user is alllowed to access the admin. Internally, this checks the `use, mod_admin_config` ACL. |
| use, admin, view, delete, update, insert, link | These properties are shortcuts to check if the current user is allowed to do some action. |
| is\\\\_allowed                                   | Perform custom ACL checks which are different from the ones mentioned.           |
| authenticated                                  | Used before the other ACL checks to check if a *typical* user is allowed to perform some actions. Example: `m.acl.authenticated.insert.article` If a user is logged on the that user’s permissions are used. |

This example prints a greeting to the currently logged in user, if logged in:


```django
{% if m.acl.user %}
    Hello, {{ m.rsc[m.acl.user].title }}!
{% else %}
    Not logged in yet
{% endif %}
```

This example checks if the user can access the admin pages:


```django
{% if m.acl.is_admin %} You are an admin {% endif %}
```

This example performs a custom check:


```django
{% if m.acl.is_allowed.use.mod_admin_config %}
    User has rights to edit the admin config
{% endif %}
```

And to check if a resource is editable:


```django
{% if m.acl.is_allowed.update[id] %}
   User can edit the resource with id {{ id }}
{% endif %}
```

A short hand for the above is (assuming id is an integer):


```django
{% if id.is_editable %}
   User can edit the resource with id {{ id }}
{% endif %}
```
").
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3
]).

-include_lib("zotonic.hrl").


-spec m_get( list(), zotonic_model:opt_msg(), z:context()) -> zotonic_model:return().
m_get([ <<"user">> | Rest ], _Msg, Context) -> {ok, {z_acl:user(Context), Rest}};
m_get([ <<"sudo_user">> | Rest ], _Msg, Context) -> {ok, {z_acl:sudo_user(Context), Rest}};
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

% Check if an anonymous (default acl setttings) is allowed to perform an action on some object
m_get([ <<"anonymous">>, Action, Object | Rest ], _Msg, Context) ->
    {ok, {is_allowed_anonymous(Action, Object, Context), Rest}};
m_get([ <<"anonymous">>, <<"is_allowed">>, Action, Object | Rest ], _Msg, Context) ->
    {ok, {is_allowed_anonymous(Action, Object, Context), Rest}};

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

is_allowed_anonymous(Action, Object, Context) ->
    is_allowed(Action, Object, z_acl:anondo(Context)).


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

