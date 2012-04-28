%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Generic template controller, serves the template mentioned in the dispatch configuration.

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

-module(resource_template).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    init/1,
    service_available/2,
    charsets_provided/2,
    content_types_provided/2,
    is_authorized/2,
    provide_content/2
]).

-include_lib("webmachine_resource.hrl").
-include_lib("include/zotonic.hrl").


init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    ?WM_REPLY(true, Context1).

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    case z_context:get(content_type, Context) of
        undefined ->
            {[{"text/html", provide_content}], ReqData, Context};
        Mime -> 
            {[{Mime, provide_content}], ReqData, Context}
    end.

%% @doc Check if the current user is allowed to view the resource. 
is_authorized(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    case z_context:get(acl, Context1) of
        undefined -> 
            ?WM_REPLY(true, Context1);
        is_auth -> 
            Context2 = z_context:ensure_all(Context1),
            z_acl:wm_is_authorized(z_auth:is_auth(Context2), Context2);
        logoff ->
            Context2 = z_context:ensure_all(Context1),
            case z_auth:is_auth(Context2) of
                true ->
                    Context3 = z_auth:logoff(Context2),
                    ?WM_REPLY(true, Context3);
                false ->
                    ?WM_REPLY(true, Context2)
            end;
        Acl -> 
            Context2 = z_context:ensure_all(Context1),
            z_acl:wm_is_authorized(Acl, Context2)
    end.


provide_content(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = case z_context:get(anonymous, Context) of
        true -> z_context:ensure_qs(Context1);
        _ -> z_context:ensure_all(Context1)
    end,
    Context3 = z_context:set_noindex_header(Context2),
    Template = z_context:get(template, Context3),
    Rendered = z_template:render(Template, z_context:get_all(Context), Context3),
    {Output, OutputContext} = z_context:output(Rendered, Context3),
    ?WM_REPLY(Output, OutputContext).
