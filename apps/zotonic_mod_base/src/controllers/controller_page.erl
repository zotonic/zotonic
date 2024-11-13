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
                maybe_redirect(Id, ContextQs);
            false ->
                {false, ContextQs}
        end
    catch
        _:Reason:S ->
            ?LOG_ERROR(#{
                in => zotonc_core,
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
    IsSeoNoIndex = z_convert:to_bool(m_rsc:p_no_acl(Id, seo_noindex, Context))
        orelse z_convert:to_bool(m_rsc:p_no_acl(CatId, is_seo_noindex_cat, Context))
        orelse z_convert:to_bool(z_context:get(seo_noindex, Context, false)),
    Context0 = z_context:set_noindex_header(IsSeoNoIndex, Context),
    Context1 = z_context:set_resource_headers(Id, Context0),
    IsCollection = m_rsc:is_a(Id, collection, Context),
    IsHome = m_rsc:p_no_acl(Id, page_path, Context) =:= <<"/">>,
    ContentLanguage = if
        IsCollection orelse IsHome ->
            z_context:language(Context1);
        true ->
            Translations = case m_rsc:p_no_acl(Id, language, Context1) of
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
            redirect(false, WebsiteUrl, Context)
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
                ReqPath == PageUrl ->
                    {true, Context};
                ReqPath1 == PageUrl ->
                    {true, Context};
                true ->
                    AbsUrl = z_context:abs_url(PageUrl, Context),
                    AbsUrlQs = append_qs(AbsUrl, cowmachine_req:req_qs(Context)),
                    redirect(true, AbsUrlQs, Context)
            end
    end.

redirect(IsPermanent, Url, Context) ->
    Context1 = cowmachine_req:set_resp_headers([
            {<<"location">>, Url},
            {<<"cache-control">>, <<"no-store, no-cache, must-revalidate, private, post-check=0, pre-check=0">>}
        ],
        Context),
    Context2 = case cowmachine_req:get_resp_header(<<"vary">>, Context1) of
        undefined ->
            cowmachine_req:set_resp_header(<<"vary">>, <<"accept-language">>, Context1);
        _ ->
            Context1
    end,
    Code = case IsPermanent of
        true -> 308;
        false -> 307
    end,
    {{halt, Code}, Context2}.

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

