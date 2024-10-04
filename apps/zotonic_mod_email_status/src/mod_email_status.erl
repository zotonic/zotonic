%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015-2024 Marc Worrell
%% @doc Track email bounces and other status of email recipients.
%% @end

%% Copyright 2015-2024 Marc Worrell
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
-mod_schema(3).

-export([
    event/2,

    observe_email_is_blocked/2,
    observe_email_is_recipient_ok/2,
    observe_email_sent/2,
    observe_email_failed/2,
    observe_email_bounced/2,
    observe_email_received/2,

    observe_tick_24h/2,

    manage_schema/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").


event(#postback{message={email_status_reset, Args}}, Context) ->
    Id = proplists:get_value(id, Args),
    case is_allowed(Id, Context) of
        true ->
            Email = proplists:get_value(email, Args),
            ok = m_email_status:clear_status(Id, Email, Context),
            case proplists:get_all_values(on_success, Args) of
                [] ->
                    z_render:growl(?__("The email error flag has been cleared.", Context), Context);
                OnSuccess ->
                    z_render:wire(OnSuccess, Context)
            end;
        false ->
            z_render:growl(?__("Sorry, you are not allowed to reset this email address.", Context), Context)
    end;
event(#postback{message={email_status_block, Args}}, Context) ->
    case is_allowed(Context) of
        true ->
            Email = proplists:get_value(email, Args),
            ok = m_email_status:block(Email, Context),
            case proplists:get_all_values(on_success, Args) of
                [] ->
                    z_render:growl(?__("The email address has been blocked.", Context), Context);
                OnSuccess ->
                    z_render:wire(OnSuccess, Context)
            end;
        false ->
            z_render:growl(?__("Sorry, you are not allowed to block this email address.", Context), Context)
    end.

is_allowed(undefined, Context) ->
    is_allowed(Context);
is_allowed(Id, Context) ->
    z_acl:rsc_editable(Id, Context) orelse is_allowed(Context).

is_allowed(Context) ->
    z_acl:is_allowed(use, mod_email_status, Context).


observe_email_is_blocked(#email_is_blocked{recipient = Recipient}, Context) ->
    m_email_status:is_blocked(Recipient, Context).

observe_email_is_recipient_ok(#email_is_recipient_ok{recipient = Recipient}, Context) ->
    m_email_status:is_ok_to_send(Recipient, Context).

observe_email_sent(#email_sent{recipient=Recipient, is_final=IsFinal}, Context) ->
    m_email_status:mark_sent(Recipient, IsFinal, Context).

observe_email_failed(#email_failed{reason=sender_disabled}, _Context) ->
    undefined;
observe_email_failed(#email_failed{is_final=false, retry_ct=RetryCt}, _Context) when RetryCt < 2 ->
    undefined;
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

observe_tick_24h(tick_24h, Context) ->
    m_email_status:periodic_cleanup(Context).

manage_schema(_InstallOrUpgrade, Context) ->
    m_email_status:install(Context).
