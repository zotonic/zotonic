%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Filter to check if the resource is a user. A resource is a user
%% when it has identities that allow it to log on.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(filter_is_user).
-moduledoc("
Test if a resource is a user.

A resource is a user when it has at least one identity of a user-defining type. These types are configured with
`auth_identity_types`; by default this includes `username_pw`.

The filter accepts a resource id, unique name, or other value accepted by `m_rsc:rid/2`. It returns `true` when the
resource exists and is a user, otherwise it returns `false`.

If the resource is not visible to the current user, it will return `false`, even if the resource is a user.

For example:

```django
{% if id|is_user %}
    This resource can log on.
{% endif %}
```

Note that a user does not have to be a person. Any resource can have identities attached to it.

See also

[m_identity](/id/doc_model_model_identity), [mod_authentication](/id/doc_module_mod_authentication)").

-export([ is_user/2 ]).

is_user(Id, Context) ->
    case m_rsc:rid(Id, Context) of
        undefined -> false;
        RscId ->
            case z_acl:rsc_visible(RscId, Context) of
                true -> m_identity:is_user(RscId, Context);
                false -> false
            end
    end.
