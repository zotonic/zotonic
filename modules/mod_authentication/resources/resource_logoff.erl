%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% Date: 2010-05-08
%% @doc Log off an user, remove "rememberme" cookies

%% Copyright 2010 Marc Worrell
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

-module(resource_logoff).
-author("Marc Worrell <marc@worrell.nl>").

-export([init/1, service_available/2, charsets_provided/2, content_types_provided/2, provide_content/2]).
-export([resource_exists/2, previously_existed/2, moved_temporarily/2]).

-include_lib("webmachine_resource.hrl").
-include_lib("include/zotonic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    Context2 = z_context:continue_session(Context1),
    ContextNoCookie = resource_logon:reset_rememberme_cookie(Context2),
    ContextLogOff = z_auth:logoff(ContextNoCookie),
    ContextNoSession = z_session_manager:stop_session(ContextLogOff),
    ?WM_REPLY(true, ContextNoSession).

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", provide_content}], ReqData, Context}.

resource_exists(ReqData, Context) ->
    % TODO: when there is javascript in the context, then return the javascript to be executed, together with a
    % redirect action.  This is the case when we have a Facebook connect log off.
    case lists:flatten(z_script:get_script(Context)) of
        [] -> {false, ReqData, Context};
        _Script -> {true, ReqData, Context}
    end.

previously_existed(ReqData, Context) ->
    {true, ReqData, Context}.

moved_temporarily(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_qs(Context1),
    Location = z_context:get_q("p", Context2, "/"),
    ?WM_REPLY({true, Location}, Context2).

provide_content(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_qs(Context1),
    Context3 = z_context:set_resp_header("X-Robots-Tag", "noindex", Context2),
    Rendered = z_template:render("logoff.tpl", z_context:get_all(Context3), Context3),
    {Output, OutputContext} = z_context:output(Rendered, Context3),
    ?WM_REPLY(Output, OutputContext).
    
