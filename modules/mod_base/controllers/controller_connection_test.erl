%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2015 Maas-Maarten Zeeman
%% @doc Zotonic stream connection tester.

%% Copyright 2015  Maas-Maarten Zeeman
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


-module(controller_connection_test).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-export([
    init/1,
    service_available/2,
    charsets_provided/2,
    content_types_provided/2,
    is_authorized/2,
    provide_content/2
]).

-export([
    event/2
]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("include/zotonic.hrl").

init(DispatchArgs) ->
    {ok, DispatchArgs}.
    
service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    Context2 = z_context:continue_session(Context1),
    z_context:lager_md(Context2),
    ?WM_REPLY(true, Context2).

charsets_provided(ReqData, Context) ->
    controller_template:charsets_provided(ReqData, Context).

content_types_provided(ReqData, Context) ->
    controller_template:content_types_provided(ReqData, Context).

is_authorized(ReqData, Context) ->
    controller_template:is_authorized(ReqData, Context).

provide_content(ReqData, Context) ->
    controller_template:provide_content(ReqData, Context).
    
    
%%
%% Events
%%

event(#postback{message={session_info, []}, target=TargetId}, Context) ->
    Vars = [{session_id, z_session:session_id(Context)}],
    
    Pages = lists:reverse(z_session:get_pages(Context)),
    
    AttachStates = [z_session_page:get_attach_state(Pid) || Pid <- Pages],
    PageIds = [z_session_page:page_id(Pid) || Pid <- Pages],
    Combi = lists:zip(PageIds, AttachStates),
    
    Vars1 = [{pages, Combi} | Vars],
    
    z_render:update(TargetId, #render{template="_session_info.tpl", vars=Vars1}, Context).
