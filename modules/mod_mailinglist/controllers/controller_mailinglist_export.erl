%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-12-01
%% @doc Export the list of active recipients of a mailinglist.

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

-module(controller_mailinglist_export).
-author("Marc Worrell <marc@worrell.nl>").

-export([
	init/1,
	service_available/2,
	forbidden/2,
	allowed_methods/2,
	charsets_provided/2,
	content_types_provided/2,
	
	to_text_csv/2
]).

-include_lib("webmachine_controller.hrl").
-include_lib("include/zotonic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    ?WM_REPLY(true, Context1).

forbidden(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_all(Context1),
	Id = z_convert:to_integer(z_context:get_q(id, Context2)),
	Allowed = z_acl:rsc_editable(Id, Context2),
	?WM_REPLY(not Allowed, Context2).

allowed_methods(ReqData, Context) ->
    {['GET', 'HEAD'], ReqData, Context}.

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.

content_types_provided(ReqData, Context) -> 
    { [{"text/csv", to_text_csv}], ReqData, Context }.

to_text_csv(ReqData, Context) ->
	Context1 = ?WM_REQ(ReqData, Context),
	Id = z_convert:to_integer(z_context:get_q(id, Context1)),
	%% Fetch all exported email addresses
	Recipients = m_mailinglist:get_enabled_recipients(Id, Context1),
	Export = z_utils:combine([13,10], Recipients),
	%% Set the content disposition filename
	Filename = "mailinglist-"++z_string:to_slug(m_rsc:p(Id, title, Context1))++".csv",
	Context2 = z_context:set_resp_header("Content-Disposition", "attachment; filename="++Filename, Context1),
	?WM_REPLY(Export, Context2).
