%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2026 Marc Worrell
%% @doc Relay e-mails via other Zotonic servers.
%% @end

%% Copyright 2011-2026 Marc Worrell
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

-module(mod_email_relay).
-moduledoc("
See also

[mod\\_email\\_receive](/id/doc_module_mod_email_receive), [E-mail handling](/id/doc_developerguide_email#guide-email).

This module supports relaying email between Zotonic servers.

With `is_email_relay` enabled, this server forwards outbound email payloads to another Zotonic relay server at
`email_relay_url`. The two servers authenticate requests using shared secrets:
`email_relay_send_secret` for sending relay requests and `email_relay_receive_secret` for receiving relay requests.
The relay server sends delivery status updates back through webhook reports.

The optional `is_user_relay` mode relays inbound messages for local `username@hostname` addresses directly to user
mailbox addresses from identity/resource data. This user-relay mode is experimental and should not be relied on for
production mail flow.


Accepted Events
---------------

This module handles the following notifier callbacks:

- `observe_email_bounced`: If the bounced email is a relayed email, then forward a delivery report using `z_db:q`.
- `observe_email_failed`: If the failed email is a relayed email, then forward a delivery report using `z_string:sanitize_utf8`.
- `observe_email_received`: Check if the recipient is a known user, if so redirect the received e-mail as-is to that user using `m_config:get_boolean`.
- `observe_email_send_encoded`: Relay an email via another Zotonic server using `m_config:get_boolean`.
- `observe_email_sent`: If the sent email is a relayed email, then forward a delivery report using `z_db:q`.
- `observe_email_status`: Forward blocking/unblocking of email addresses to the relaying Zotonic server using `m_config:get_boolean`.
- `observe_tick_24h`: Handle `tick_24h` notifications using `m_email_relay:periodic_cleanup`.

").
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Email Relay").
-mod_description("Relay e-mails via other Zotonic servers.").
-mod_prio(500).
-mod_schema(1).
-mod_config([
        #{
            key => is_email_relay,
            type => boolean,
            default => false,
            description => "Enable email relay via other Zotonic servers."
        },
        #{
            key => email_relay_url,
            type => string,
            default => "",
            description => "The Zotonic server URL to relay emails to."
        },
        #{
            key => email_relay_send_secret,
            type => string,
            default => "",
            description => "The secret used to authenticate sending emails to the relay server. "
                           "This is a shared secret between this server and the relay server and must "
                           "be set manually on both servers. This must be kept secret."
        },
        #{
            key => email_relay_receive_secret,
            type => string,
            default => "",
            description => "The secret used to authenticate receiving emails from the sending server. "
                           "This is a shared secret between this server and the sending server and must "
                           "be set manually on both servers. This must be kept secret."
        },
        #{
            key => is_user_relay,
            type => boolean,
            default => false,
            description => "[EXPERIMENTAL - DO NOT USE] Relay received emails for a user directly to known users."
        }
    ]).

-export([
    observe_email_status/2,
    observe_email_received/2,
    observe_email_send_encoded/2,
    observe_email_failed/2,
    observe_email_sent/2,
    observe_email_bounced/2,
    observe_tick_24h/2,
    task_set_email_block_status/4,
    task_webhook_status/6,
    manage_schema/2
]).

-define(MAX_WEBHOOK_RETRIES, 20).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Check if the recipient is a known user, if so redirect the received e-mail as-is to that user.
%% @todo This needs to be modified, so that extra relay headers are added to the message before
%% it is forwarded, otherwise DKIM and sender checks will fail for this forwarded message.
observe_email_received(#email_received{localpart=Recipient} = Received, Context) ->
    case m_config:get_boolean(mod_email_relay, is_user_relay, Context) of
        true ->
            case m_identity:lookup_by_username(Recipient, Context) of
                undefined ->
                    undefined;
                Props ->
                    case proplists:get_value(is_verified, Props) of
                        true ->
                            UserId = proplists:get_value(rsc_id, Props),
                            Email = m_rsc:p_no_acl(UserId, email_raw, Context),
                            case z_utils:is_empty(Email) of
                                true ->
                                    undefined;
                                false ->
                                    % Relay the e-mail as-is
                                    Msg = #email{
                                        to = Email,
                                        raw = Received#email_received.raw
                                    },
                                    z_email:send(Msg, Context)
                            end;
                        false ->
                            undefined
                    end
            end;
        false ->
            undefined
    end.

%% @doc Forward blocking/unblocking of email addresses to the relaying Zotonic server.
%% If the relay server blocked an address then we can unblock it in this way. The relay
%% server will report its blocking status when receiving an email relay request.
observe_email_status(#email_status{ is_manual = true, is_valid = false, recipient = Email }, Context) ->
    set_email_block_status(Email, true, Context);
observe_email_status(#email_status{ is_manual = true, is_valid = true, recipient = Email }, Context) ->
    set_email_block_status(Email, false, Context);
observe_email_status(#email_status{}, _Context) ->
    ok.

%% @doc Forward changes to the email block status to the relaying Zotonic server. Any queued
%% update is overwriting previous retrying status changes.
set_email_block_status(Email, IsBlock, Context) ->
    case m_config:get_boolean(?MODULE, is_email_relay, Context) of
        true ->
            case m_config:get_value(?MODULE, email_relay_url, Context) of
                undefined -> ok;
                <<>> -> ok;
                _ ->
                    Key = z_crypto:hex_sha(Email),
                    z_pivot_rsc:insert_task(
                        ?MODULE, task_set_email_block_status, Key,
                        [Email, IsBlock, 0],
                        Context),
                    ok
            end;
        false ->
            ok
    end.

%% @doc Task to forward a manual change to the block status of an email address to the
%% relay server. Incremental backoff if the relay server cannot be reached.
task_set_email_block_status(Email, IsBlock, Retries, Context) ->
    case m_config:get_boolean(?MODULE, is_email_relay, Context) of
        true ->
            case m_config:get_value(?MODULE, email_relay_url, Context) of
                undefined -> ok;
                <<>> -> ok;
                RelayUrl ->
                    Secret = z_convert:to_binary(m_config:get_value(?MODULE, email_relay_send_secret, Context)),
                    Url1 = iolist_to_binary([ RelayUrl, "/recipient?s=", z_url:url_encode(Secret) ]),
                    Payload = #{
                        <<"recipient">> => Email,
                        <<"is_blocked">> => IsBlock
                    },
                    Options = [
                        {content_type, <<"application/json">>}
                    ],
                    case z_fetch:fetch_json(post, Url1, Payload, Options, Context) of
                        {ok, _} ->
                            ok;
                        {error, Reason} when Retries < ?MAX_WEBHOOK_RETRIES ->
                            ?LOG_WARNING(#{
                                in => zotonic_mod_email_relay,
                                text => <<"Email block forward returns error, retrying">>,
                                result => error,
                                reason => Reason,
                                retries => Retries,
                                recipient => Email,
                                url => RelayUrl
                            }),
                            {delay, backoff(Retries), [Email, IsBlock, Retries+1]};
                        {error, Reason} = Error ->
                            ?LOG_WARNING(#{
                                in => zotonic_mod_email_relay,
                                text => <<"Email block forward returns error, giving up">>,
                                result => error,
                                reason => Reason,
                                retries => Retries,
                                recipient => Email,
                                url => RelayUrl
                            }),
                            Error
                    end
            end;
        false ->
            ok
    end.

%% @doc If the failed email is a relayed email, then forward a delivery report
%% to the webhook of the relayed email.
observe_email_failed(#email_failed{
        message_nr = MsgId,
        recipient = Recipient,
        is_final = IsFinal,
        reason = Reason,
        status = Status
    }, Context) ->
    Severity = case Reason of
        bounce -> permanent_failure;
        illegal_address -> permanent_failure;
        smtphost -> permanent_failure;
        _ when not IsFinal -> temporary_failure;
        _ when IsFinal -> permanent_failure
    end,
    Report = #{
        <<"type">> => Severity,
        <<"recipient">> => Recipient,
        <<"status">> => ensure_binary(Status)
    },
    queue_for_webhook(MsgId, Report, Context).

ensure_binary(undefined) ->
    <<>>;
ensure_binary({error, Reason}) ->
    z_string:sanitize_utf8(z_convert:to_binary(io_lib:format("error: ~p", [Reason])));
ensure_binary(B) when is_binary(B) ->
    z_string:sanitize_utf8(B).


%% @doc If the sent email is a relayed email, then forward a delivery report
%% to the webhook of the relayed email.
observe_email_sent(#email_sent{
            message_nr = MsgId,
            recipient = Recipient,
            is_final = false
        }, Context) ->
    Report = #{
        <<"type">> => relayed,
        <<"recipient">> => Recipient
    },
    queue_for_webhook(MsgId, Report, Context);
observe_email_sent(#email_sent{ is_final = true }, _Context) ->
    ok.

%% @doc If the bounced email is a relayed email, then forward a delivery report
%% to the webhook of the relayed email.
observe_email_bounced(#email_bounced{
            message_nr = MsgId,
            recipient = Recipient
        }, Context) ->
    Report = #{
        <<"type">> => <<"permanent_failure">>,
        <<"recipient">> => Recipient,
        <<"status">> => <<"bounce">>
    },
    queue_for_webhook(MsgId, Report, Context).

queue_for_webhook(MsgId, Report, Context) ->
    Hooks = z_db:q("
        select webhook_url, sender_message_nr
        from email_relay
        where message_nr = $1", [ MsgId ], Context),
    lists:foreach(
        fun({HookUrl, SenderMsgId}) ->
            case is_https_url(HookUrl) of
                true ->
                    Report1 = Report#{
                        <<"message_nr">> => SenderMsgId
                    },
                    z_pivot_rsc:insert_task(
                        ?MODULE, task_webhook_status, MsgId,
                        [MsgId, SenderMsgId, HookUrl, 0, Report1],
                        Context);
                false ->
                    ok
            end
        end,
        Hooks).

is_https_url(<<"https://", _/binary>>) -> true;
is_https_url(_) -> false.

%% @doc Task queue callback to send a delivery report to the webhook of a relayed
%% email. If the report could not be sent then the delivery is retried with an
%% incremental backoff.
task_webhook_status(MsgId, SenderMsgId, HookUrl, Retries, Report, Context) ->
    Secret = z_convert:to_binary(m_config:get_value(?MODULE, email_relay_receive_secret, Context)),
    Url1 = iolist_to_binary([ HookUrl, "?s=", z_url:url_encode(Secret) ]),
    Payload = #{
        <<"type">> => <<"delivery_report">>,
        <<"report">> => Report
    },
    Options = [
        {content_type, <<"application/json">>}
    ],
    case z_fetch:fetch_json(post, Url1, Payload, Options, Context) of
        {ok, _} ->
            ok;
        {error, Reason} when Retries < ?MAX_WEBHOOK_RETRIES ->
            ?LOG_WARNING(#{
                in => zotonic_mod_email_relay,
                text => <<"WebhookUrl returns error, retrying">>,
                result => error,
                reason => Reason,
                retries => Retries,
                message_nr => MsgId,
                sender_message_nr => SenderMsgId,
                webhook_url => HookUrl
            }),
            {delay, backoff(Retries), [MsgId, SenderMsgId, HookUrl, Retries+1, Report]};
        {error, Reason} = Error ->
            ?LOG_WARNING(#{
                in => zotonic_mod_email_relay,
                text => <<"WebhookUrl returns error, giving up">>,
                result => error,
                reason => Reason,
                message_nr => MsgId,
                sender_message_nr => SenderMsgId,
                webhook_url => HookUrl
            }),
            Error
    end.

backoff(1) -> 60;
backoff(N) when N < 4 -> 600;
backoff(N) when N < 10 -> 3600;
backoff(N) -> (N-9) * 3600.


%% @doc Relay an email via another Zotonic server. This webhook is called by the
%% z_email_server when sending the email.
observe_email_send_encoded(#email_send_encoded{
            message_nr = MsgId,
            from = _VERP,
            to = RecipientEmail,
            encoded = EncodedMail
        },
        Context) ->
    case m_config:get_boolean(?MODULE, is_email_relay, Context) of
        true ->
            case m_config:get_value(?MODULE, email_relay_url, Context) of
                undefined -> undefined;
                <<>> -> undefined;
                RelayUrl ->
                    Host = host(RelayUrl),
                    Secret = z_convert:to_binary(m_config:get_value(?MODULE, email_relay_send_secret, Context)),
                    Url1 = iolist_to_binary([ RelayUrl, "?s=", z_url:url_encode(Secret) ]),
                    Hostname = z_context:hostname(Context),
                    MsgId1 = <<MsgId/binary, "@", Hostname/binary>>,
                    Payload = #{
                        <<"message_nr">> => MsgId1,
                        <<"recipient">> => RecipientEmail,
                        <<"email">> => EncodedMail,
                        <<"webhook_url">> => webhook_url(Context)
                    },
                    Options = [
                        {content_type, <<"application/json">>}
                    ],
                    case z_fetch:fetch_json(post, Url1, Payload, Options, Context) of
                        {ok, #{
                            <<"status">> := <<"ok">>,
                            <<"result">> := #{
                                <<"receipt">> := Receipt
                            } = R
                        }} ->
                            ?LOG_INFO(#{
                                in => zotonic_mod_relay,
                                text => <<"Relayed email to remote server">>,
                                result => ok,
                                to => RecipientEmail,
                                relay => RelayUrl,
                                message => R
                            }),
                            {ok, Receipt};
                        {ok, #{
                            <<"status">> := <<"error">>,
                            <<"result">> := #{
                                <<"receipt">> := Message
                            } = R
                        }} ->
                            ?LOG_ERROR(#{
                                in => zotonic_mod_relay,
                                text => <<"Error relaying email to remote server">>,
                                result => error,
                                reason => Message,
                                to => RecipientEmail,
                                relay => RelayUrl,
                                message => R
                            }),
                            {error, relay, {temporary_failure, Host, Message}};
                        {error, {Status, _Url, _Hs, _Sz, Body}} ->
                            ?LOG_ERROR(#{
                                in => zotonic_mod_relay,
                                text => <<"Error relaying email to remote server">>,
                                result => error,
                                reason => Status,
                                to => RecipientEmail,
                                relay => RelayUrl,
                                http_body => Body
                            }),
                            Message = iolist_to_binary(io_lib:format("400 Relay error: ~p", [ Status ])),
                            {error, relay, {temporary_failure, Host, Message}};
                        {error, Reason} ->
                            ?LOG_ERROR(#{
                                in => zotonic_mod_relay,
                                text => <<"Error relaying email to remote server">>,
                                result => error,
                                reason => Reason,
                                to => RecipientEmail,
                                relay => RelayUrl
                            }),
                            Message = iolist_to_binary(io_lib:format("400 Relay error: ~p", [ Reason ])),
                            {error, relay, {temporary_failure, Host, Message}}
                    end
            end;
        false ->
            undefined
    end.

host(RelayUrl) ->
    #{ host := Host } =  uri_string:parse(RelayUrl),
    Host.

webhook_url(Context) ->
    Url = z_dispatcher:url_for(api, [ {star, <<"model/email_relay/post/status">>} ], Context),
    z_dispatcher:abs_url(Url, Context).

observe_tick_24h(tick_24h, Context) ->
    m_email_relay:periodic_cleanup(Context).


manage_schema(_Version, Context) ->
    m_email_relay:install(Context).

