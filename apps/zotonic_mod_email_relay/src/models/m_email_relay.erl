%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2024 Marc Worrell
%% @doc Accept complete rendered emails for relay, receive delivery reports
%% and updates to manual changes of email block status.
%% @end

%% Copyright 2024 Marc Worrell
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

-module(m_email_relay).
-moduledoc("
Not yet documented.
").

-export([
    m_get/3,
    m_post/3,
    relay/2,
    periodic_cleanup/1,
    install/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

m_get(_Path, _Msg, _Context) ->
    {error, enoent}.

%% @doc Relay an encoded email to some address.
m_post([ <<"relay">> ], #{ payload := Msg }, Context) ->
    % Relay a an email
    case is_allowed_relay(Context) of
        true ->
            relay(Msg, Context);
        false ->
            {error, eacces}
    end;
m_post([ <<"relay">>, <<"recipient">> ], #{ payload := Msg }, Context) ->
    % Set the block status of an email address.
    case is_allowed_relay(Context) of
        true ->
            set_recipient_block_status(Msg, Context);
        false ->
            {error, eacces}
    end;
m_post([ <<"status">> ], #{ payload := #{
        <<"type">> := <<"delivery_report">>,
        <<"report">> := Report
    } }, Context) ->
    % Receive a delivery report from the server that relayed the email.
    % This is the webhook that is sent along with the email relay request.
    case is_allowed_status(Context) of
        true ->
            update_status(Report);
        false ->
            {error, eacces}
    end;
m_post(_Path, _Msg, _Context) ->
    {error, enoent}.


-spec is_allowed_relay(Context) -> boolean() when
    Context :: z:context().
is_allowed_relay(Context) ->
    case z_acl:is_allowed(use, mod_email_relay, Context) of
        true ->
            z_auth:is_auth(Context);
        false ->
            case z_convert:to_binary(m_config:get_value(mod_email_relay, email_relay_receive_secret, Context)) of
                <<>> -> false;
                Secret ->
                    KeyQ = z_convert:to_binary(z_context:get_q(<<"s">>, Context)),
                    m_identity:is_equal(KeyQ, Secret)
            end
    end.

-spec is_allowed_status(Context) -> boolean() when
    Context :: z:context().
is_allowed_status(Context) ->
    case z_acl:is_allowed(use, mod_email_relay, Context) of
        true ->
            z_auth:is_auth(Context);
        false ->
            case z_convert:to_binary(m_config:get_value(mod_email_relay, email_relay_send_secret, Context)) of
                <<>> -> false;
                Secret ->
                    KeyQ = z_convert:to_binary(z_context:get_q(<<"s">>, Context)),
                    m_identity:is_equal(KeyQ, Secret)
            end
    end.

update_status(#{
        <<"type">> := Type,
        <<"message_nr">> := MsgId,
        <<"recipient">> := Recipient
    } = Report) ->
    StatusMessage = ensure_binary_status(maps:get(<<"status">>, Report, undefined)),
    DeliveryType = delivery_type(Type),
    z_email_server:delivery_report(DeliveryType, Recipient, MsgId, StatusMessage);
update_status(Report) ->
    ?LOG_ERROR(#{
        in => zotonic_mod_email_relay,
        text => <<"Received unknown status report">>,
        result => error,
        reason => unknown_report,
        report => Report
    }),
    {error, unknown_report}.

ensure_binary_status(undefined) -> <<>>;
ensure_binary_status(B) when is_binary(B) -> B;
ensure_binary_status({<<"error">>, B}) when is_binary(B) -> B;
ensure_binary_status({<<"error">>, V}) -> z_convert:to_binary(io_lib:format("~p", [ V ]));
ensure_binary_status(V) -> z_convert:to_binary(io_lib:format("~p", [ V ])).


% permanent_failure | temporary_failure | sent | received | relayed
-spec delivery_type(binary()) -> z_email_server:delivery_type().
delivery_type(<<"permanent_failure">>) -> permanent_failure;
delivery_type(<<"temporary_failure">>) -> temporary_failure;
delivery_type(<<"sent">>) -> relayed;
delivery_type(<<"relayed">>) -> relayed;
delivery_type(<<"received">>) -> relayed;
delivery_type(_) -> temporary_failure.


%% @doc Copy the email block status from the other Zotonic server to this server.
%% This enables editors on the sending server to reset the block status of email
%% addresses on the relaying (this) server.
set_recipient_block_status(#{ <<"recipient">> := Email, <<"is_blocked">> := IsBlocked }, Context) ->
    Email1 = m_identity:normalize_key(email, Email),
    case z_convert:to_bool(IsBlocked) of
        true ->
            ?LOG_INFO(#{
                in => zotonic_mod_email_relay,
                text => <<"Blocked recipient on behalf of remote Zotonic">>,
                result => ok,
                recipient => Email1
            }),
            m_email_status:block(Email1, Context);
        false ->
            ?LOG_INFO(#{
                in => zotonic_mod_email_relay,
                text => <<"Unblocked recipient on behalf of remote Zotonic">>,
                result => ok,
                recipient => Email1
            }),
            m_email_status:clear_status(Email1, Context)
    end;
set_recipient_block_status(Payload, _Context) ->
    ?LOG_WARNING(#{
        in => zotonic_mod_email_relay,
        text => <<"Unknown payload for recipient block status API">>,
        result => error,
        reason => payload,
        payload => Payload
    }),
    {error, payload}.



%% @doc Relay an email from another Zotonic server to the email system here.
-spec relay(Payload, Context) -> {ok, Result} | {error, Reason} when
    Payload :: map(),
    Context :: z:context(),
    Result :: map(),
    Reason :: term().
relay(#{ <<"recipient">> := To, <<"email">> := Email } = Msg, Context) when is_binary(Email) ->
    case m_config:get_boolean(?MODULE, is_email_relay, Context) of
        true ->
            ?LOG_ERROR(#{
                in => zotonic_mod_email_relay,
                text => <<"Not relaying email message, as this server is configured to relay to another server">>,
                result => error,
                reason => relaying,
                recipient => To
            }),
            {error, relaying};
        false ->
            case decode(Email) of
                {ok, {_MimeType, _MimeSubType, _Headers, _Parameters, _Parts} = Decoded} ->
                    MessageNr = maps:get(<<"message_nr">>, Msg, undefined),
                    WebhookUrl = maps:get(<<"webhook_url">>, Msg, undefined),
                    relay_1(To, MessageNr, WebhookUrl, Decoded, Context);
                {error, _} = Error ->
                    Error
            end
    end;
relay(_Msg, _Context) ->
    {error, payload}.


relay_1(To, MessageNr, WebhookUrl, {MimeType, MimeSubType, Headers, Parameters, Parts}, Context) ->
    ExtMessageNr = if
        MessageNr =:= undefined orelse MessageNr =:= <<>> ->
            extract_message_id(proplists:get_value(<<"Message-Id">>, Headers));
        true ->
            MessageNr
    end,
    case z_db:q1("
        select message_nr
        from email_relay
        where sender_message_nr = $1",
        [ ExtMessageNr ],
        Context)
    of
        undefined ->
            Headers1 = filter_headers(Headers),
            Email = #email{
                to = To,
                reply_to = proplists:get_value(<<"Reply-To">>, Headers),
                body = {MimeType, MimeSubType, Headers1, Parameters, Parts}
            },
            {ok, RelayId} = save_relay(To, undefined, ExtMessageNr, WebhookUrl, Context),
            case z_email:send(Email, Context) of
                {ok, MsgNr} ->
                    z_db:q("
                        update email_relay
                        set message_nr = $1
                        where id = $2",
                        [ MsgNr, RelayId ],
                        Context),
                    {ok, #{
                        <<"receipt">> => <<"200 [RELAY] Message queued as ", MsgNr/binary>>,
                        <<"to">> => To,
                        <<"message_nr">> => MsgNr
                    }};
                {error, Reason} ->
                    z_db:q("
                        delete from email_relay
                        where id = $1",
                        [ RelayId ],
                        Context),
                    Reason1 = iolist_to_binary(io_lib:format("~p", [ Reason ])),
                    {error, #{
                        <<"receipt">> => <<"500 [RELAY] ", Reason1/binary>>,
                        <<"to">> => To
                    }}
            end;
        MsgNr ->
            {ok, #{
                <<"receipt">> => <<"200 [RELAY] Message already queued as ", MsgNr/binary>>,
                <<"to">> => To,
                <<"message_nr">> => MsgNr
            }}
    end.

extract_message_id(undefined) ->
    undefined;
extract_message_id(<<"<", _/binary>> = MsgId) ->
    {Email, _} = z_email:split_name_email(MsgId),
    [Local|_] = binary:split(Email, <<"@">>),
    Local;
extract_message_id(MsgId) ->
    MsgId.


save_relay(To, MsgId, ExtMessageNr, WebhookUrl, Context) ->
    {Email, _} = z_email:split_name_email(To),
    Email1 = m_identity:normalize_key(email, Email),
    z_db:insert(
        email_relay,
        #{
            <<"recipient">> => Email1,
            <<"message_nr">> => MsgId,
            <<"sender_message_nr">> => ExtMessageNr,
            <<"webhook_url">> => WebhookUrl
        },
        Context).

filter_headers(Headers) ->
    lists:filter(
        fun
            ({<<"Subject">>, _}) -> true;
            ({<<"Date">>, _}) -> true;
            ({<<"Cc">>, _}) -> true;
            (_) -> false
        end,
        Headers).

decode(Email) ->
    try
        {ok, mimemail:decode(Email)}
    catch
        _:_ -> {error, email}
    end.

%% @doc Delete all relayed email older than a week.
-spec periodic_cleanup(z:context()) -> ok.
periodic_cleanup(Context) ->
    z_db:q("
        delete from email_relay
        where created < now() - interval '1 week'",
        Context),
    ok.

install(Context) ->
    case z_db:table_exists(email_relay, Context) of
        true ->
            ok;
        false ->
            [] = z_db:q("
                create table email_relay (
                    id bigserial not null,
                    recipient character varying(250) not null,
                    message_nr character varying(100),
                    sender_message_nr character varying(100),
                    webhook_url character varying(250),

                    created timestamp with time zone not null default now(),
                    modified timestamp with time zone not null default now(),

                    primary key (id)
                )", Context),
            [] = z_db:q("CREATE INDEX email_relay_message_nr_key ON email_relay (message_nr)", Context),
            [] = z_db:q("CREATE INDEX email_relay_sender_message_nr_key ON email_relay (sender_message_nr)", Context),
            [] = z_db:q("CREATE INDEX email_relay_recipient_key ON email_relay (recipient)", Context),
            [] = z_db:q("CREATE INDEX email_relay_modified_key ON email_relay (modified)", Context),
            z_db:flush(Context),
            ok
    end.

