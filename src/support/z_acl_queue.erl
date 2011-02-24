%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% Date: 2010-05-04
%% @doc Queue acl requests for later handling.  For now a dummy module.
%% The idea is that when an action is not allowed then the user can request another, more powerful user, to
%% perform the action.  Suuch requests are queued per user or group.

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

