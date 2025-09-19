%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2022 Marc Worrell
%% @doc Redirect to a defined other url.

%% Copyright 2009-2022 Marc Worrell
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

-module(controller_redirect).
-moduledoc("
Redirect to another url.

This controller redirects a request to another URL. The URL can be a fixed URL, the location of a fixed page id or the
name of a dispatch rule.

Example dispatch rule using the redirect controller:


```erlang
{redir, [\"plop\"], controller_redirect, [{url, \"/newplop\"}, {is_permanent, true}]}
```

This redirects any requests of “/plop” permanently to “/newplop”.

It has the following dispatch options:

| Option         | Description                                                                      | Example                     |
| -------------- | -------------------------------------------------------------------------------- | --------------------------- |
| url            | The url of the new location the browser is sent to.                              | \\\\{url, “/example”\\\\}       |
| dispatch       | Name of a dispatch rule to use for the location url. All arguments (except dispatch and is\\\\_permanent) are used as parameters for the dispatch rule. | \\\\{dispatch, admin\\\\}       |
| id             | Id of the page to redirect to. The controller will redirect to the page\\\\_url of this id. The id can be an integer or the name of the page (use an atom or a binary) or the atom user\\\\_id for the id of the current user. | \\\\{id, 123\\\\}               |
| qargs          | A list with querystring arguments to use in the new dispatch rule. Specifies what query (or dispatch) arguments to use from this dispatch rule into the dispatch rule that is being redirected to. | \\\\{qargs, \\\\[id, slug\\\\]\\\\} |
| is\\\\_permanent | Use a permanent (301) or temporary redirect (307). Defaults to false.            | \\\\{is\\\\_permanent, false\\\\} |
| acl            | Perform access control check before redirect. Defaults to no check.              | \\\\{acl, is\\\\_auth\\\\}        |

This controller does only handle request arguments that are specifically noted in the “qargs” list (and then only
when the “dispatch” argument is set).



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



Example
-------

A dispatch rule that always redirects /foo/12312/slug to /bar/12312/slug:


```erlang
{bar, [\"bar\", id, slug], controller_page, [{template, \"bar.tpl\"}]},
{bar_redirect, [\"foo\", id, slug], controller_redirect, [{dispatch, bar}, {qargs, [id,slug]}]}
```
").
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_authorized/1,
	resource_exists/1,
	previously_existed/1,
	moved_temporarily/1,
	moved_permanently/1
]).


is_authorized(Context) ->
    Id = z_controller_helper:get_configured_id(Context),
    z_controller_helper:is_authorized(Id, Context).

resource_exists(Context) ->
	{false, Context}.

previously_existed(Context) ->
	{true, Context}.

moved_temporarily(Context) ->
    case z_context:get(is_permanent, Context, false) of
        true -> {false, Context};
        false -> do_redirect(Context)
    end.

moved_permanently(Context) ->
    case z_context:get(is_permanent, Context, false) of
        true -> do_redirect(Context);
        false -> {false, Context}
    end.

do_redirect(Context) ->
	Location = case z_context:get(url, Context) of
		undefined ->
			case z_context:get(dispatch, Context) of
				undefined ->
                    case z_controller_helper:get_configured_id(Context) of
						undefined -> <<"/">>;
						Id -> m_rsc:p(Id, page_url, Context)
					end;
				Dispatch ->
                    Args = z_context:get_all(Context),
                    Args1 = case z_controller_helper:get_configured_id(Context) of
                        undefined -> Args;
                        Id -> [ {id, Id} | proplists:delete(id, Args) ]
                    end,
                    Args2 = lists:foldl(
                        fun(A, Acc) ->
                            proplists:delete(A, Acc)
                        end,
                        Args1,
                        [
                            is_permanent, url, dispatch, qargs, acl, csp_nonce,
                            auth_expires, auth_replay_token, auth_options,
                            is_http_request
                        ]),
                    QArgs = case z_context:get(qargs, Context) of
                                undefined ->
                                    [];
                                ArgList when is_list(ArgList) ->
                                    [ {K, z_context:get_q(K, Context, <<>>)} || K <- ArgList ]
                            end,
					Args3 = lists:foldl(fun(K, Acc) ->
											proplists:delete(K, Acc)
										end,
										QArgs ++ Args2,
										z_dispatcher:dispatcher_args()),
					 z_html:unescape( z_dispatcher:url_for(Dispatch, Args3, Context) )
			end;
		Url ->
			Url
	end,
	{{true, z_context:abs_url(Location, Context)}, Context}.

