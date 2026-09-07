%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Admin webmachine_controller.
%% @end

%% Copyright 2009 Tim Benniks
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

-module(controller_admin).
-moduledoc(#{
    zotonic_keywords => ["reference", "backend_developer", "controller", "content_authoring", "authorization_and_access_control", "user_interface_and_interaction"]
}).
-moduledoc("
The admin controller is the main controller behind which admin pages are served. Its main purpose is that it does an
authentication check (Is current user allowed to `use` the module `mod_admin`).

The template parameter decides which admin template gets served, and defaults to admin.tpl.

Dispatch options
----------------

| Option | Description |
| ------ | ----------- |
| `template` | Template rendered for the request. Defaults to `admin.tpl`. |
| `selected` | Name of the active admin navigation item. Defaults to `dashboard`. |
| `acl_module` | Module checked for the `{use, Module}` permission. Defaults to `mod_admin`. |

All dispatch and request arguments are passed to the template. A typical dispatch rule is:

```erlang
{admin_overview_rsc, [\"admin\", \"overview\"], controller_admin, [
    {template, <<\"admin_overview.tpl\">>},
    {selected, <<\"overview\">>},
    seo_noindex
]}
```

Request handling
----------------

The controller disables caching and indexing, checks the configured module permission, renders the selected template,
and returns the generated HTML. Use a more specific controller when a page needs resource lookup, form processing, or a
response format other than the standard admin HTML page.
").
-author("Tim Benniks <tim@timbenniks.com>").

-export([
    service_available/1,
	is_authorized/1,
    process/4
    ]).

service_available(Context) ->
    Context1 = z_context:set_noindex_header(Context),
    Context2 = z_context:set_nocache_headers(Context1),
    {true, Context2}.

is_authorized(Context) ->
    z_controller_helper:is_authorized([{use, z_context:get(acl_module, Context, mod_admin)}], Context).

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    Template = z_context:get(template, Context, "admin.tpl"),
    Selected = z_context:get(selected, Context, "dashboard"),
    Args = z_context:get_all(Context),
    Vars = [
    	{selected, Selected} | Args
    ],
    Html = z_template:render(Template, Vars, Context),
    z_context:output(Html, Context).
