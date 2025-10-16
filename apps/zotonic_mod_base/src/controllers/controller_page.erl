%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell
%% @doc Standard controller for HTML representation of resources. Redirects
%% to the canonical page_url and handles "gone" resources that are merged
%% into other resources.
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

-module(controller_page).
-moduledoc("
Show a rsc as a HTML page.

This controller is used to show the HTML page of a [resource](/id/doc_glossary#term-resource). A “404 Not Found” or
“410 Gone” page is shown if the requested page never existed or has been deleted.

The user will be redirected to the `logon` URL when the current user is not allowed to view the page.

This controller also adds a `noindex` response header when the page’s [seo\\_noindex](/id/doc_module_mod_seo) flag is set.

Example dispatch rule:


```erlang
{page, [\"page\", id], controller_page, []}
```



Dispatch arguments
------------------

`controller_page` recognizes the following arguments inside the dispatch pattern:

| Argument | Description                                                                      | Example URL |
| -------- | -------------------------------------------------------------------------------- | ----------- |
| id       | The id of the page (rsc) to be shown. This can be the numerical id or the unique name of a page. | /page/12345 |



Dispatch options
----------------

The following options can be given to the dispatch rule:

| Option         | Description                                                                      | Example                                                             |
| -------------- | -------------------------------------------------------------------------------- | ------------------------------------------------------------------- |
| id             | Id or unique name of the resource to be shown. This overrules any id in the query arguments. Use `user_id` for the id of the current user. | \\\\{id, page\\\\_about\\\\}                                              |
| template       | Name of the template to be rendered. Defaults to “page.tpl” Can also be a tuple of the following form: \\\\{cat, Name\\\\}. See also: [catinclude](/id/doc_template_tag_tag_catinclude). | \\\\{template, “about.tpl”\\\\}  \\\\{template, \\\\{cat, “home. tpl”\\\\}\\\\} |
| cat            | The category the resource that is requested has to be. If a page of a different category is requested, a 404 is shown. | \\\\{cat, text\\\\}                                                     |
| acl\\\\_action   | What ACL action will be checked. Defaults to ‘view’; but can also be ‘edit’ if users need edit permission on the rsc to be able to access the resource. | \\\\{acl\\\\_action, edit\\\\}                                            |
| acl            | Extra authorization checks to be performed.                                      | See [ACL options](#acl-options).                                    |
| is\\\\_canonical | Whether this URL should be considered the caninical URL of the page. If so, the controller will redirect to the rsc’s page path if set. Defaults to true. | \\\\{is\\\\_canonical, false\\\\}                                         |
| seo\\\\_noindex  | Ask crawlers to not index this page.                                             | seo\\\\_noindex                                                       |
| nocache        | Prevent browser caching this page.                                               | nocache                                                             |



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

[controller\\_template](/id/doc_controller_controller_template).
").
-author("Marc Worrell <marc@worrell.nl>").

-export([
    service_available/1,
    content_types_provided/1,
    resource_exists/1,
    previously_existed/1,
    moved_permanently/1,
    is_authorized/1,
    % last_modified/1,
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
    CTs = case z_context:get(content_type, Context) of
        undefined ->
            [ {<<"text">>, <<"html">>, []} ];
        Mime when is_list(Mime) ->
            [ z_convert:to_binary(Mime) ];
        Mime ->
            [ Mime ]
    end,
    {CTs, Context}.

%% @doc Check if the current user is allowed to view the resource.
is_authorized(Context) ->
    z_context:logger_md(Context),
    case z_context:get(anonymous, Context) of
        true ->
            {true, Context};
        _ ->
            Id = z_controller_helper:get_id(Context),
            z_controller_helper:is_authorized(Id, Context)
    end.

%% @doc Check if the id in the request (or dispatch conf) exists.
resource_exists(Context) ->
    ContextQs = z_context:ensure_qs(Context),
    try
        Id = z_controller_helper:get_id(ContextQs),
        case exists(Id, ContextQs) of
            true ->
                ok = z_context:set_req_metrics( #{ rsc_id => Id }, ContextQs),
                maybe_redirect(Id, ContextQs);
            false ->
                {false, ContextQs}
        end
    catch
        _:Reason:S ->
            ?LOG_ERROR(#{
                in => zotonic_core,
                text => <<"Error checking resource_exists">>,
                result => error,
                reason => Reason,
                stack => S
            }),
            {false, ContextQs}
    end.

%% @doc Check if the resource used to exist. If so then check if the
%% resource moved to a new location, if not then return a 404.
previously_existed(Context) ->
    Id = z_controller_helper:get_id(Context),
    {m_rsc_gone:is_gone(Id, Context), Context}.

%% @doc If the resource previously existed, then permanently redirect to the
%% new location. If no new location, then 410 gone is returned.
moved_permanently(Context) ->
    Id = z_controller_helper:get_id(Context),
    redirect(m_rsc_gone:get_new_location(Id, Context), Context).

redirect(undefined, Context) ->
    {false, Context};
redirect(Location, Context) ->
    {{true, Location}, Context}.

%% Return the modification date of the resource.
%% There is a problem with the CSP nonce being different on the 304
%% than on the original page. Prevent caching by not setting the
%% modified date till we have a fix/workaround.
% last_modified(Context) ->
%     Id = z_controller_helper:get_id(Context),
%     {m_rsc:p(Id, <<"modified">>, Context), Context}.

%% @doc Show the page. Add a noindex header when requested by the editor.
process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    Id = z_controller_helper:get_id(Context),
    CatId = m_rsc:p_no_acl(Id, category_id, Context),
    IsSeoNoIndex = z_convert:to_bool(m_rsc:p_no_acl(Id, <<"seo_noindex">>, Context))
        orelse z_convert:to_bool(m_rsc:p_no_acl(CatId, <<"is_seo_noindex_cat">>, Context))
        orelse z_convert:to_bool(z_context:get(seo_noindex, Context, false)),
    Context0 = z_context:set_noindex_header(IsSeoNoIndex, Context),
    Context1 = z_context:set_resource_headers(Id, Context0),
    IsCollection = m_rsc:is_a(Id, collection, Context),
    ContextNoLang = z_context:set_language('x-default', Context),
    IsHome = m_rsc:p_no_acl(Id, <<"page_url">>, ContextNoLang) =:= <<"/">>,
    ContentLanguage = if
        IsCollection orelse IsHome ->
            z_context:language(Context1);
        true ->
            Translations = case m_rsc:p_no_acl(Id, <<"language">>, Context1) of
                undefined -> [];
                Lngs -> Lngs
            end,
            z_trans:lookup_fallback_languages(Translations, Context1)
    end,
	RenderArgs = [
        {id, Id},
        {z_content_language, ContentLanguage},
        {seo_noindex, IsSeoNoIndex}
        | proplists:delete(seo_noindex, z_context:get_all(Context1))
    ],
    RenderArgs1 = z_notifier:foldl(template_vars, RenderArgs, Context1),
	RenderFunc = fun() ->
		Template = z_context:get(template, Context1, {cat, <<"page.tpl">>}),
	    z_template:render(Template, RenderArgs1, Context1)
	end,

    %% EXPERIMENTAL:
    %%
    %% When the 'cache_anonymous_max_age' flag is set then we enable simple page caching.
    %% This does not take into account any query args and vary headers.
    %% @todo Add the 'vary' headers to the cache key
	MaxAge = z_context:get(cache_anonymous_max_age, Context1),
	Html = case z_auth:is_auth(Context1) of
		false when is_integer(MaxAge), MaxAge > 0 ->
			QueryArgs = z_context:get_q_all(Context1),
		    z_depcache:memo(RenderFunc, {page_template_anonymous, RenderArgs, QueryArgs}, MaxAge, [Id], Context1);
		false when is_integer(MaxAge), MaxAge =:= 0 ->
			QueryArgs = z_context:get_q_all(Context1),
		    z_depcache:memo(RenderFunc, {page_template_anonymous, RenderArgs, QueryArgs}, 0, [], Context1);
		_ ->
			RenderFunc()
	end,
	%% End experimental.

	z_context:output(Html, Context1).


maybe_redirect(Id, Context) ->
    maybe_redirect_website(
          m_rsc:p_no_acl(Id, <<"website">>, Context),
          m_rsc:p_no_acl(Id, <<"is_website_redirect">>, Context),
          Id, Context).

maybe_redirect_website(undefined, _IsRedirect, Id, Context) ->
    maybe_redirect_canonical(Id, Context);
maybe_redirect_website(<<>>, _IsRedirect, Id, Context) ->
    maybe_redirect_canonical(Id, Context);
maybe_redirect_website(Website, true, _Id, Context) ->
    WebsiteUrl = z_context:abs_url(Website, Context),
    RequestUrl = z_context:abs_url(cowmachine_req:raw_path(Context), Context),
    if
        WebsiteUrl == RequestUrl ->
            % Redirect requested to itself - no further redirects
            {true, Context};
        true ->
            z_controller_helper:redirect(false, WebsiteUrl, Context)
    end;
maybe_redirect_website(_Website, _False, Id, Context) ->
    maybe_redirect_canonical(Id, Context).

maybe_redirect_canonical(Id, Context) ->
    case is_canonical(Id, Context) of
        false ->
            {true, Context};
        true ->
            ReqPath = cowmachine_req:raw_path(Context),
            [ReqPath1|_] = binary:split(ReqPath, <<"?">>),
            PageUrl = m_rsc:p(Id, <<"page_url">>, Context),
            if
                ReqPath =:= PageUrl ->
                    {true, Context};
                ReqPath1 =:= PageUrl ->
                    {true, Context};
                true ->
                    AbsUrl = z_context:abs_url(PageUrl, Context),
                    AbsUrlQs = append_qs(AbsUrl, cowmachine_req:req_qs(Context)),
                    z_controller_helper:redirect(true, AbsUrlQs, Context)
            end
    end.

%% @doc Return true if the page should only be shown on the canonical url.
is_canonical(Id, Context) ->
    case m_rsc:p_no_acl(Id, <<"is_page_path_multiple">>, Context) of
        true -> false;
        _False -> z_context:get(is_canonical, Context, true)
    end.

append_qs(AbsUrl, []) ->
    AbsUrl;
append_qs(AbsUrl, Qs) ->
    iolist_to_binary([AbsUrl, $?, cow_qs:qs(Qs)]).

exists(Id, Context) ->
    case {m_rsc:exists(Id, Context), z_context:get(cat, Context)} of
        {Exists, undefined} ->
            Exists;
        {true, Cat} ->
            m_rsc:is_a(Id, Cat, Context);
        {false, _} ->
            false
    end.

