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
    service_available/1,
    charsets_provided/1,
    content_types_provided/1,
    is_authorized/1,
    provide_content/1
]).

-export([
    event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").


service_available(Context) ->
    Context1 = z_context:continue_session(Context),
    {true, Context1}.

charsets_provided(Context) ->
    controller_template:charsets_provided(Context).

content_types_provided(Context) ->
    controller_template:content_types_provided(Context).

is_authorized(Context) ->
    controller_template:is_authorized(Context).

provide_content(Context) ->
    controller_template:provide_content(Context).


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
    z_render:update(TargetId, #render{template = <<"_session_info.tpl">>, vars = Vars1}, Context).

%% Test code for transport, should be moved somewhere else.
% event(#z_msg_v1{msg_id=MsgId, data=Data}, Context) ->
%     case proplists:get_value(<<"cmd">>, Data) of
%         <<"sleep">> ->
%             lager:debug("[~p] connection-test: sleeping 10 secs for msg ~p",
%                         [z_context:site(Context), MsgId]),
%             timer:sleep(10000);
%         UnknownCmd ->
%             lager:debug("[~p] Unknown connection-test command ~p",
%                         [z_context:site(Context), UnknownCmd])
%     end,
%     Context.
