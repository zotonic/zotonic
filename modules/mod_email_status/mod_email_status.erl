%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015 Marc Worrell
%% @doc Track email bounces and other status of email recipients.

%% Copyright 2015 Marc Worrell
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

-module(mod_email_status).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Email Status").
-mod_description("Track bounce and receive status of email recipients.").
-mod_prio(10).
-mod_schema(1).

-export([
    observe_email_sent/2,
    observe_email_failed/2,
    observe_email_bounced/2,
    observe_email_received/2,

    manage_schema/2
]).

-include_lib("zotonic.hrl").

observe_email_sent(#email_sent{recipient=Recipient, is_final=IsFinal}, Context) ->
    m_email_status:mark_sent(Recipient, IsFinal, Context).

observe_email_failed(#email_failed{recipient=Recipient, is_final=IsFinal, status=Status}, Context) ->
    m_email_status:mark_failed(Recipient, IsFinal, Status, Context).

%% @doc Mark an email address as bouncing, only marks for messages which we know we have sent.
observe_email_bounced(#email_bounced{recipient=undefined}, _Context) ->
    ok;
observe_email_bounced(#email_bounced{message_nr=undefined}, _Context) ->
    ok;
observe_email_bounced(#email_bounced{recipient=Recipient}, Context) ->
    m_email_status:mark_bounced(Recipient, Context).

observe_email_received(#email_received{from=From}, Context) when is_binary(From) ->
    m_email_status:mark_received(From, Context),
    undefined.

manage_schema(install, Context) ->
    m_email_status:install(Context).
