%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Handle received e-mails, notifies email observers depending on the recipient.

%% Copyright 2011 Marc Worrell
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

-module(mod_email_receive).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Email Receive Handler").
-mod_description("Handle received e-mails, notifies email observers depending on the recipient.").
-mod_prio(500).

-export([
	init/1,
	
	event/2,

	observe_email_ensure_handler/2,
	observe_email_add_handler/2,
	observe_email_drop_handler/2,

	observe_email_received/2
]).

-include_lib("zotonic.hrl").

% E-mail handler:
% - email address
% - user_id
% - notification
% - resource_id (optional)
% 
% Search by (combination of):
% - email address
% - user_id
% - resource_id
% - notification
% 
% Notifications:
% - {Notification, received, UserId, ResourceId, Email}
% - {Notification, describe, UserId, ResourceId}

init(Context) ->
	m_email_receive_recipient:install(Context).


% Handle the administration of email addresses
event(_Event, Context) ->
	Context.

% @doc Ensure that en email handler exists
observe_email_ensure_handler(#email_ensure_handler{notification=Notification, user_id=UserId, resource_id=ResourceId}, Context) ->
	m_email_receive_recipient:ensure(Notification, UserId, ResourceId, Context).

% @doc Add an email handler, returns the email address for the handler
observe_email_add_handler(#email_add_handler{notification=Notification, user_id=UserId, resource_id=ResourceId}, Context) ->
	m_email_receive_recipient:insert(Notification, UserId, ResourceId, Context).

% @doc Drop all handlers matching the parameters
observe_email_drop_handler(#email_drop_handler{notification=Notification, user_id=UserId, resource_id=ResourceId}, Context) ->
	m_email_receive_recipient:delete(Notification, UserId, ResourceId, Context).


% @doc Find handlers based on the recipient, forward the e-mail to those handlers.
%      The e-mail is already parsed and (mostly) sanitized.
%      The e-mail address can be of the form "recipient+tag"
observe_email_received(#email_received{localpart=Recipient} = Received, Context) ->
	case m_email_receive_recipient:get_by_recipient(Recipient, Context) of
		{Notification, UserId, ResourceId} ->
			z_notifier:notify({z_convert:to_atom(Notification), received, UserId, ResourceId, Received}, Context);
		undefined ->
			undefined
	end.

