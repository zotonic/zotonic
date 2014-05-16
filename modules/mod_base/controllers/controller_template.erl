%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2013 Marc Worrell
%% @doc Generic template controller, serves the template mentioned in the dispatch configuration.

%% Copyright 2009-2013 Marc Worrell
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
-author("Marc Worrell <marc@worrell.nl>").

-export([
    init/1,
    service_available/2,
    charsets_provided/2,
    content_types_provided/2,
    is_authorized/2,
    provide_content/2
]).

-include_lib("controller_webmachine_helper.hrl").
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
    Context1 = z_context:ensure_all(?WM_REQ(ReqData, Context)),
    case z_context:get(acl, Context1) of
        undefined -> 
            is_authorized_action(Context1);
        ignore ->
            ?WM_REPLY(true, Context1);
        is_auth -> 
            case z_auth:is_auth(Context1) of
                true -> is_authorized_action(Context1);
                false -> is_authorized_action(false, Context1)
            end;
        logoff ->
            Context2 = case z_auth:is_auth(Context1) of
                           true ->
                               z_auth:logoff(Context1);
                           false ->
                               Context1
                       end,
            is_authorized_action(Context2);
        Acl -> 
            is_authorized_action(append_acl(Acl, Context1), Context1)
    end.

is_authorized_action(Context) ->
    is_authorized_action([get_acl_action(Context)], Context).

is_authorized_action(Acl, Context) ->
    %% wm_is_authorized for Acl::{view, undefined} -> true
    z_acl:wm_is_authorized(Acl, Context).

get_acl_action(Context) ->
    {z_context:get(acl_action, Context, view), get_id(Context)}.

get_id(Context) ->
    controller_page:get_id(Context).


append_acl({_, _}=Acl, Context) ->
    [get_acl_action(Context), Acl];
append_acl(Acl, Context) when is_list(Acl) ->
    [get_acl_action(Context) | Acl].


provide_content(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = case z_context:get(anonymous, Context) of
        true -> z_context:ensure_qs(Context1);
        _ -> z_context:ensure_all(Context1)
    end,
    Context3 = z_context:set_noindex_header(Context2),
    Context4 = set_optional_cache_header(Context3),
    Template = z_context:get(template, Context4),
    Vars = [
        {id, get_id(Context3)}
        | z_context:get_all(Context3)
    ],
    Rendered = z_template:render(Template, Vars, Context4),
    {Output, OutputContext} = z_context:output(Rendered, Context4),
    ?WM_REPLY(Output, OutputContext).

set_optional_cache_header(Context) ->
    case z_context:get(maxage, Context) of
        undefined ->
            Context;
        MaxAge when is_integer(MaxAge) ->
            z_context:set_resp_header("Cache-Control", "public, max-age="++integer_to_list(MaxAge), Context)
    end.

            
            
