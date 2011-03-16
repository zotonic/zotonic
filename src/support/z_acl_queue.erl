%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% Date: 2010-05-04
%% @doc Queue acl requests for later handling.  For now a dummy module.
%% The idea is that when an action is not allowed then the user can request another, more powerful user, to
%% perform the action.  Suuch requests are queued per user or group.

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


-module(z_acl_queue).
-author("Marc Worrell <marc@worrell.nl>").

-export([
	queue_request/3,
	queue_list/2,
	queue_allow/2,
	queue_deny/2
]).

-include_lib("zotonic.hrl").

-record(acl_request, {request_id, action, object, actor, message, timestamp}).


%% @doc Queue a request for an action with some actors.  Eg. request to add an edge to a resource for which the
%% current actor doesn't have permissions.
queue_request(ToActors, Request, Context) ->
	Request = todo.
	
queue_list(Actor, Context) -> 
	[].

queue_allow(Request, Context) ->
	Request = todo.
	
queue_deny(Request, Context) ->
	Request = todo.

