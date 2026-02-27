%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell
%% @doc Generic template controller, serves the template mentioned in the dispatch configuration.
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

-module(controller_template).
-moduledoc("
Show a template.

This controller renders the template configured in the dispatch rules.

Example dispatch rule:


```erlang
{home, [], controller_template, [{template, \"home.tpl\"}]}
```

This will render the `home.tpl` template at the url `/`.



Dispatch arguments
------------------

`controller_template` recognizes the following arguments inside the dispatch pattern:

| Argument | Description                                                                      | Example URL |
| -------- | -------------------------------------------------------------------------------- | ----------- |
| id       | A resource id to be used in the template. This can be the numerical id or the unique name of a page. More commonly the id is given as a dispatch option. | /page/12345 |



Dispatch options
----------------

The following options can be given to the dispatch rule:

| Option         | Description                                                                      | Example                                                           |
| -------------- | -------------------------------------------------------------------------------- | ----------------------------------------------------------------- |
| template       | Name of the template to be rendered. Can also be a tuple of the following form: \\\\{cat, Name\\\\}. See also: [catinclude](/id/doc_template_tag_tag_catinclude). | \\\\{template, “home.tpl”\\\\} \\\\{template, \\\\{cat, “home. tpl”\\\\}\\\\} |
| anonymous      | Render the template always as the anonymous user, even when a user is logged on. Defaults to false. | \\\\{anonymous, true\\\\}                                             |
| content\\\\_type | The content type provided by the dispatch rule. Defaults to “text/html”.         | \\\\{content\\\\_type, “application/json”\\\\}                          |
| max\\\\_age      | The number of seconds of how long to cache this file in the browser. Sets the response header: Cache-control: public; max-age=X. | \\\\{max\\\\_age, 3600\\\\}                                             |
| acl\\\\_action   | What ACL action will be checked. Defaults to ‘view’; but can also be ‘edit’ if users need edit permission on the rsc to be able to access the resource. | \\\\{acl\\\\_action, edit\\\\}                                          |
| acl            | Extra authorization checks to be performed.                                      | See [ACL options](#acl-options).                                  |
| id             | Id or unique name of a resource to be referenced in the rendered template. This overrules and id from the query arguments. | \\\\{id, page\\\\_about\\\\}                                            |
| seo\\\\_noindex  | Ask crawlers to not index this page.                                             | seo\\\\_noindex                                                     |
| nocache        | Prevent browser caching this page.                                               | nocache                                                           |
| http\\\\_status  | The HTTP status code to return. This defaults to 200.                            | \\\\{http\\\\_status, 418\\\\}                                          |



ACL options
-----------

[Authorization](/id/doc_developerguide_access_control#guide-authorization) checks to perform, in addition to the `acl_action` dispatch option, can be given in the `acl` dispatch option, and accepts the following options:

| ACL option             | Description                                                                      | Example                                                                   |
| ---------------------- | -------------------------------------------------------------------------------- | ------------------------------------------------------------------------- |
| `is_auth`              | Disable anonymous access to this resource.                                       | `{acl, is_auth}`                                                          |
| `logoff`               | Log out user before processing the request.                                      | `{acl, logoff}`                                                           |
| `{Action, Resource}`   | Check if user is allowed to perform `Action` on `Resource`. The example is equivalent to the options `{acl_action, edit}, {id, my_named_page}`. | `{acl, {edit, my_named_page}}`                                            |
| `[{Action, Resource}]` | A list of checks to be performed, as above.                                      | > \\\\{acl, \\\\[ >  \\\\{view, secret\\\\_page\\\\}, >  \\\\{update, 345\\\\} > \\\\]\\\\} |
| `ignore`               | Don’t perform any access control checks. Be careful to add your own checks in the rendered template and all its included templates. | `{acl, ignore}`                                                           |

See also

[controller_page](/id/doc_controller_controller_page).
").
-author("Marc Worrell <marc@worrell.nl>").

-export([
    service_available/1,
    content_types_provided/1,
    is_authorized/1,
    process/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

service_available(Context) ->
    Context1 = case z_convert:to_bool(z_context:get(nocache, Context)) of
        true -> z_context:set_nocache_headers(Context);
        false -> Context
    end,
    {true, Context1}.

content_types_provided(Context) ->
    case z_context:get(content_type, Context) of
        undefined ->
            {[ {<<"text">>, <<"html">>, []} ], Context};
        Mime when is_list(Mime) ->
            {[ z_convert:to_binary(Mime) ], Context};
        Mime ->
            {[ Mime ], Context}
    end.

%% @doc Check if the current user is allowed to view the resource.
is_authorized(Context) ->
    z_context:logger_md(Context),
    case z_context:get(anonymous, Context) of
        true ->
            {true, Context};
        _ ->
            Id = z_controller_helper:get_configured_id(Context),
            z_controller_helper:is_authorized(Id, Context)
    end.

process(_Method, _AcceptedCT, ProvidedCT, Context) ->
    case ProvidedCT of
        {<<"text">>, <<"html">>, _} ->
            % Redirect if the language should be added to the URL path.
            case z_controller_helper:is_redirect_language(Context) of
                true ->
                    Path = cowmachine_req:raw_path(Context),
                    Path1 = iolist_to_binary([ $/, z_convert:to_binary(z_context:language(Context)), Path ]),
                    Location = z_context:abs_url(Path1, Context),
                    z_controller_helper:redirect(true, Location, Context);
                false ->
                    process_1(Context)
            end;
        _ ->
            process_1(Context)
    end.

process_1(Context) ->
    Vars = z_context:get_all(Context),
    {Vars1, OptRscId} = maybe_configured_id(Vars, Context),
    IsSeoNoIndex = z_convert:to_bool(m_rsc:p_no_acl(OptRscId, seo_noindex, Context))
        orelse z_convert:to_bool(z_context:get(seo_noindex, Context, false)),
    Context0 = z_context:set_noindex_header(IsSeoNoIndex, Context),
    Context1 = z_context:set_resource_headers(OptRscId, Context0),
    Context2 = set_optional_cache_header(Context1),
    Template = z_context:get(template, Context2),
    case Template of
        undefined ->
            ?LOG_ERROR(#{
                in => zotonic_mod_base,
                text => <<"No template config in dispatch for controller_template">>,
                result => error,
                reason => enoent,
                dispatch_rule => z_context:get_q(<<"zotonic_dispatch">>, Context2),
                dispatch_file => z_context:get(zotonic_dispatch_file, Context2),
                dispatch_module => z_context:get(zotonic_dispatch_module, Context2),
                path => m_req:get(path, Context2)
            }),
            {{halt, 500}, Context};
        _ ->
            Vars2 = [
                {seo_noindex, IsSeoNoIndex}
                | proplists:delete(seo_noindex, Vars1)
            ],
            Rendered = z_template:render(Template, Vars2, Context2),
            {RespBody, ContextOut} = z_context:output(Rendered, Context2),
            case z_context:get(http_status, ContextOut) of
                Status when is_integer(Status) ->
                    ContextReply = cowmachine_req:set_resp_body(RespBody, ContextOut),
                    {{halt, Status}, ContextReply};
                _ ->
                    {RespBody, ContextOut}
            end
    end.

-spec maybe_configured_id(list(), z:context()) -> {list(), m_rsc:resource_id()|undefined}.
maybe_configured_id(Vars, Context) ->
    Id = z_controller_helper:get_configured_id(Context),
    {[ {id, Id} | lists:keydelete(id, 1, Vars) ], Id}.

set_optional_cache_header(Context) ->
    case z_context:get(max_age, Context) of
        undefined ->
            Context;
        MaxAge when is_integer(MaxAge) ->
            z_context:set_resp_header(
                <<"cache-control">>,
                <<"public, max-age=", (z_convert:to_binary(MaxAge))/binary>>,
                Context)
    end.
