%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2025 Marc Worrell
%% @doc Basic page
%% @end

%% Copyright 2009-2025 Marc Worrell
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
    moved_permanently/2,
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
        case exists(Id, ContextQs) of
            true ->
                maybe_redirect(Id, ContextQs);
            false ->
                ?WM_REPLY(false, ContextQs)
        end
    catch
        _:_ -> ?WM_REPLY(false, ContextQs)
    end.

%% @doc Check if the resource used to exist
previously_existed(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Id = z_controller_helper:get_id(Context1),
    IsGone = m_rsc_gone:is_gone(Id, Context1),
    ?WM_REPLY(IsGone, Context1).

moved_permanently(ReqData, Context) ->
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
    CatId = m_rsc:p_no_acl(Id, category_id, Context),
    IsSeoNoIndex = z_convert:to_bool(m_rsc:p_no_acl(Id, seo_noindex, Context))
        orelse z_convert:to_bool(m_rsc:p_no_acl(CatId, is_seo_noindex_cat, Context)),
    Context1 = z_context:set_noindex_header(IsSeoNoIndex, Context),

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
        _ -> do_redirect(false, AbsUrl, Context)
    end;
maybe_redirect_website(_Website, _False, Id, Context) ->
    maybe_redirect_canonical(Id, Context).

maybe_redirect_canonical(Id, Context) ->
    case is_canonical(Id, Context) of
        false ->
            ?WM_REPLY(true, Context);
        true ->
            ReqPath = current_path(Context),
            PageUrl = m_rsc:p(Id, page_url, Context),
            if
                ReqPath =:= PageUrl ->
                    ?WM_REPLY(true, Context);
                true ->
                    AbsUrl = m_rsc:p(Id, page_url_abs, Context),
                    AbsUrlQs = append_qs(AbsUrl, wrq:req_qs(z_context:get_reqdata(Context))),
                    do_redirect(true, AbsUrlQs, Context)
            end
    end.

do_redirect(IsPermanent, Location, Context) ->
    Context1 = z_context:set_resp_header(
            "Cache-Control",
            "no-store, no-cache, must-revalidate, private, post-check=0, pre-check=0",
            Context),
    Context2 = case z_context:get_resp_header("Vary", Context1) of
        undefined ->
            z_context:set_resp_header("Vary", "accept-language", Context1);
        _ ->
            Context1
    end,
    Code = if
        IsPermanent -> 308;
        true -> 307
    end,
    ContextRedirect = z_context:set_resp_header("Location", Location, Context2),
    ?WM_REPLY({halt, Code}, ContextRedirect).

current_path(Context) ->
    Path = z_convert:to_binary(wrq:raw_path(z_context:get_reqdata(Context))),
    hd(binary:split(Path, <<"?">>)).

is_canonical(Id, Context) ->
    case m_rsc:p_no_acl(Id, is_page_path_multiple, Context) of
        true -> false;
        _False -> z_context:get(is_canonical, Context, true)
    end.

append_qs(AbsUrl, []) ->
    AbsUrl;
append_qs(AbsUrl, Qs) ->
    iolist_to_binary([AbsUrl, $?, mochiweb_util:urlencode(Qs)]).

exists(Id, Context) ->
    case {m_rsc:exists(Id, Context), z_context:get(cat, Context)} of
        {Exists, undefined} ->
            Exists;
        {true, Cat} ->
            m_rsc:is_a(Id, Cat, Context);
        {false, _} ->
            false
    end.
