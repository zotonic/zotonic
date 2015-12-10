%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2015 Marc Worrell
%% @doc Basic page

%% Copyright 2009-2015 Marc Worrell
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
-author("Marc Worrell <marc@worrell.nl>").

-export([
    resource_exists/2,
    previously_existed/2,
    moved_temporarily/2,
    is_authorized/2,
    html/1
]).

-include_lib("controller_html_helper.hrl").

%% @doc Check if the id in the request (or dispatch conf) exists.
resource_exists(ReqData, Context) ->
    Context1  = ?WM_REQ(ReqData, Context),
    ContextQs = z_context:ensure_qs(Context1),
    try
        Id = z_controller_helper:get_id(ContextQs),
        maybe_redirect(Id, ContextQs)
    catch
        _:_ -> ?WM_REPLY(false, ContextQs)
    end.

%% @doc Check if the resource used to exist
previously_existed(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Id = z_controller_helper:get_id(Context1),
    IsGone = m_rsc_gone:is_gone(Id, Context1),
    ?WM_REPLY(IsGone, Context1).

moved_temporarily(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Id = z_controller_helper:get_id(Context1),
    redirect(m_rsc_gone:get_new_location(Id, Context1), Context1).

redirect(undefined, Context) ->
    ?WM_REPLY(false, Context);
redirect(Location, Context) ->
    ?WM_REPLY({true, Location}, Context).


%% @doc Check if the current user is allowed to view the resource. 
is_authorized(ReqData, Context) ->
    controller_template:is_authorized(ReqData, Context).


%% @doc Show the page.  Add a noindex header when requested by the editor.
html(Context) ->
    Id = z_controller_helper:get_id(Context),
    Context1 = z_context:set_noindex_header(m_rsc:p(Id, seo_noindex, Context), Context),

	%% EXPERIMENTAL:
	%%
	%% When the 'cache_anonymous_maxage' flag is set then we enable simple page caching.
	%% This does not take into account any query args and vary headers.
	%% @todo Add the 'vary' headers to the cache key
	RenderArgs = [ {id, Id} | z_context:get_all(Context1) ],
	RenderFunc = fun() ->
		Template = z_context:get(template, Context1, "page.tpl"),
	    z_template:render(Template, RenderArgs, Context1)
	end,

	MaxAge = z_context:get(cache_anonymous_maxage, Context1),
	Html = case not z_auth:is_auth(Context1) of
		true when is_integer(MaxAge), MaxAge > 0 -> 
			QueryArgs = z_context:get_q_all(Context1),
		    z_depcache:memo(RenderFunc, {page_template_anonymous, RenderArgs, QueryArgs}, MaxAge, [Id], Context1);
		true when is_integer(MaxAge), MaxAge == 0 ->
			QueryArgs = z_context:get_q_all(Context1),
		    z_depcache:memo(RenderFunc, {page_template_anonymous, RenderArgs, QueryArgs}, 0, [], Context1);
		_ ->
			RenderFunc()
	end,
	%% End experimental.

	z_context:output(Html, Context1).


maybe_redirect(Id, Context) ->
    maybe_redirect_website(
          m_rsc:p_no_acl(Id, website, Context),
          m_rsc:p_no_acl(Id, is_website_redirect, Context),
          Id, Context).

maybe_redirect_website(undefined, _IsRedirect, Id, Context) ->
    maybe_redirect_canonical(Id, Context);
maybe_redirect_website(<<>>, _IsRedirect, Id, Context) ->
    maybe_redirect_canonical(Id, Context);
maybe_redirect_website(Website, true, Id, Context) ->
    AbsUrl = z_context:abs_url(Website, Context),
    CurrAbsUrl = z_context:abs_url(current_path(Context), Context),
    case AbsUrl of
        CurrAbsUrl -> maybe_redirect_canonical(Id, Context);
        _ -> do_temporary_redirect(AbsUrl, Context)
    end;
maybe_redirect_website(_Website, _False, Id, Context) ->
    maybe_redirect_canonical(Id, Context).

maybe_redirect_canonical(Id, Context) ->
    maybe_redirect_page_path(m_rsc:p_no_acl(Id, page_path, Context), Id, Context).

maybe_redirect_page_path(undefined, Id, Context) ->
    maybe_exists(Id, Context);
maybe_redirect_page_path(<<>>, Id, Context) ->
    maybe_exists(Id, Context);
maybe_redirect_page_path(PagePath, Id, Context) ->
    case is_canonical(Id, Context) of
        false ->
            maybe_exists(Id, Context);
        true ->
            %% Check if we need to be at a different URL. If the page_path
            %% of a resource is set, we need to redirect there if the
            %% current request's path is not equal to the resource's path.
            case current_path(Context) of
                PagePath ->
                    maybe_exists(Id, Context);
                _ ->
                    AbsUrl = m_rsc:p(Id, page_url_abs, Context),
                    AbsUrlQs = append_qs(AbsUrl, wrq:req_qs(z_context:get_reqdata(Context))),
                    do_temporary_redirect(AbsUrlQs, Context)
            end
    end.

do_temporary_redirect(Location, Context) ->
    ContextRedirect = z_context:set_resp_header("Location", Location, Context),
    ?WM_REPLY({halt, 302}, ContextRedirect).

current_path(Context) ->
    case z_context:get_q(zotonic_dispatch_path, Context) of
        [] -> <<"/">>;
        DP -> z_convert:to_binary([[ $/, P ] || P <- DP ])
    end.

is_canonical(Id, Context) ->
    case m_rsc:p_no_acl(Id, is_page_path_multiple, Context) of
        true -> false;
        _False -> z_context:get(is_canonical, Context, true)
    end.

append_qs(AbsUrl, []) ->
    AbsUrl;
append_qs(AbsUrl, Qs) ->
    iolist_to_binary([AbsUrl, $?, mochiweb_util:urlencode(Qs)]).

maybe_exists(Id, Context) ->
    case {m_rsc:exists(Id, Context), z_context:get(cat, Context)} of
        {Exists, undefined} ->
            ?WM_REPLY(Exists, Context);
        {true, Cat} ->
            ?WM_REPLY(m_rsc:is_a(Id, Cat, Context), Context);
        {false, _} ->
            ?WM_REPLY(false, Context)
    end.

