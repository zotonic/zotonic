%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Basic page

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

-module(controller_page).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    resource_exists/2,
    previously_existed/2,
    is_authorized/2,
    html/1,
    get_id/1
]).

-include_lib("controller_html_helper.hrl").

%% @doc Check if the id in the request (or dispatch conf) exists.
resource_exists(ReqData, Context) ->
    Context1  = ?WM_REQ(ReqData, Context),
    ContextQs = z_context:ensure_qs(Context1),
    try
        Id = get_id(ContextQs),
        case {m_rsc:exists(Id, ContextQs), z_context:get(cat, ContextQs)} of
            {Exists, undefined} ->
                ?WM_REPLY(Exists, ContextQs);
            {true, Cat} ->
                ?WM_REPLY(m_rsc:is_a(Id, Cat, ContextQs), ContextQs);
            {false, _} ->
                ?WM_REPLY(false, ContextQs)
        end
    catch
        _:_ -> ?WM_REPLY(false, ContextQs)
    end.

%% @doc Check if the resource used to exist
previously_existed(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    IsGone = case get_id(Context1) of
                 Id when is_integer(Id) ->
                     m_rsc_gone:is_gone(Id, Context1);
                 _ -> false
             end,
    ?WM_REPLY(IsGone, Context1).


%% @doc Check if the current user is allowed to view the resource. 
is_authorized(ReqData, Context) ->
    controller_template:is_authorized(ReqData, Context).


%% @doc Show the page.  Add a noindex header when requested by the editor.
html(Context) ->
    Id = get_id(Context),
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


%% @doc Fetch the id from the request or the dispatch configuration.
%% @spec get_id(Context) -> int() | false
get_id(Context) ->
    ReqId = case z_context:get(id, Context) of
        undefined -> z_context:get_q("id", Context);
        ConfId -> ConfId
    end,
    case m_rsc:name_to_id(ReqId, Context) of
        {ok, RscId} -> RscId;
        _ -> false
    end.
