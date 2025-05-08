%% @doc SMTP server for handling incoming and bounced messages.
%%      Code based on the example callback module supplied
%%      with gen_smtp.
%%      Original author: Andrew Thompson (andrew@hijacked.us)
%% @author Atilla Erdodi <atilla@maximonster.com>
%% @copyright 2010-2025 Maximonster Interactive Things
%% @end

%% Copyright 2010-2025 Maximonster Interactive Things
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


-module(zotonic_listen_smtp).

-behaviour(gen_smtp_server_session).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([ child_spec/0 ]).

-export([init/4, handle_STARTTLS/1, handle_HELO/2, handle_EHLO/3, handle_MAIL/2, handle_MAIL_extension/2,
         handle_RCPT/2, handle_RCPT_extension/2, handle_DATA/4, handle_RSET/1, handle_VRFY/2,
         handle_other/3, code_change/3, terminate/2]).

-record(state, {
    options = [] :: list(),
    peer :: tuple(),
    banner :: binary(),
    hostname :: binary(),
    helo :: binary() | undefined,
    rcpt :: binary() | undefined,
    from :: binary() | undefined
}).

-define(SESSION_LIMIT, 20).

child_spec() ->
    case {z_config:get(smtp_listen_ip),z_config:get(smtp_listen_port)} of
        {none, Port} ->
            ?LOG_WARNING(#{
                text => <<"SMTP server disabled: 'smtp_listen_ip' is set to 'none'">>,
                in => zotonic_listen_smtp,
                ip => none,
                port => Port,
                protocol => smtp
            }),
            ignore;
        {IP, none} ->
            ?LOG_WARNING(#{
                text => <<"SMTP server disabled: 'smtp_listen_port' is set to 'none'">>,
                in => zotonic_listen_smtp,
                ip => ip_to_string(IP),
                port => none,
                protocol => smtp
            }),
            ignore;
        {IP, Port} ->
            Args1 = case z_config:get(smtp_listen_domain) of
                undefined -> [];
                "" -> [];
                Domain -> [{domain, Domain}]
            end,
            Args2 = case IP of
                any -> [{address, {0,0,0,0}} | Args1];
                _ when is_tuple(IP) ->
                    [{address, IP} | Args1]
            end,
            ?LOG_NOTICE(#{
                text => <<"SMTP server listening">>,
                in => zotonic_listen_smtp,
                ip => ip_to_string(IP),
                port => Port,
                protocol => smtp
            }),
            Options = [
                {port, Port},
                {sessionoptions, [
                    {tls_options, tls_options()}
                ]}
                | Args2
            ],
            gen_smtp_server:child_spec(?MODULE, ?MODULE, Options)
    end.

tls_options() ->
    Options = z_ssl_certs:ssl_listener_options(),
    Options1 = lists:keydelete(session_tickets, 1, Options),
    Options2 = lists:keydelete(reuse_sessions, 1, Options1),
    Options3 = lists:keydelete(anti_replay, 1, Options2),
    [
        {session_tickets, disabled}
        | Options3
    ].

ip_to_string(any) -> "any";
ip_to_string(IP) -> inet:ntoa(IP).


-spec init(Hostname :: atom() | string(), SessionCount :: non_neg_integer(), Peer :: tuple(), Options :: list()) -> {'ok', iodata(), #state{}} | {'stop', any(), iodata()}.
init(Hostname, SessionCount, Peer, Options) ->
    HostnameB = z_convert:to_binary(Hostname),
    case SessionCount > ?SESSION_LIMIT of
        false ->
            State = #state{
                options = Options,
                peer = Peer,
                hostname = HostnameB,
                banner = <<HostnameB/binary, " ESMTP Zotonic">>
            },
            {ok, State#state.banner, State};
        true ->
            ?LOG_WARNING(#{
                text => <<"SMTP Connection limit exceeded">>,
                in => zotonic_listen_smtp,
                limit => ?SESSION_LIMIT,
                session_count => SessionCount,
                src => inet:ntoa(Peer),
                hostname => HostnameB,
                protocol => smtp
            }),
            {stop, normal, io_lib:format("421 ~s is too busy to accept mail right now", [Hostname])}
    end.

-spec handle_STARTTLS(#state{}) -> #state{}.
handle_STARTTLS(State) ->
    State.

-spec handle_HELO(Hostname :: binary(), State :: #state{}) -> {'error', string(), #state{}} | {'ok', pos_integer(), #state{}} | {'ok', #state{}}.
handle_HELO(Hostname, State) ->
    {ok, 655360, State#state{helo=Hostname}}. % 640kb of HELO should be enough for anyone.
    % If {ok, State} was returned here, we'd use the default 10mb limit

-spec handle_EHLO(Hostname :: binary(), Extensions :: list(), State :: #state{}) -> {'error', string(), #state{}} | {'ok', list(), #state{}}.
handle_EHLO(Hostname, Extensions, State) ->
    IsSTARTTLS = z_config:get(smtp_starttls),
    MyExtensions = case proplists:get_value(auth, State#state.options, false) of
                       true ->
                           % auth is enabled, so advertise it
                           Extensions ++ [{"AUTH", "PLAIN LOGIN CRAM-MD5"}, {"STARTTLS", true}];
                       false when IsSTARTTLS ->
                           Extensions ++ [{"STARTTLS", true}];
                       false ->
                           Extensions
                   end,
    {ok, MyExtensions, State#state{helo=Hostname}}.

-spec handle_MAIL(From :: binary(), State :: #state{}) -> {'ok', #state{}} | {'error', string(), #state{}}.
handle_MAIL(From, State) ->
    check_dnsbl(State#state{from=From}).

check_dnsbl(State) ->
    DNSBL = z_config:get(smtp_dns_blocklist, z_email_dnsbl:dns_blocklist()),
    DNSWL = z_config:get(smtp_dns_allowlist, z_email_dnsbl:dns_allowlist()),
    case z_email_dnsbl:status(State#state.peer, DNSBL, DNSWL) of
        {ok, notlisted} ->
            {ok, State};
        {ok, allowed} ->
            {ok, State};
        {ok, {blocked, Service}} ->
            ?LOG_NOTICE(#{
                text => <<"SMTP DNSBL check: blocked -- closing connection with a 451">>,
                in => zotonic_listen_smtp,
                src => inet:ntoa(State#state.peer),
                dnsbl => Service,
                protocol => smtp
            }),
            Error = io_lib:format("451 ~s has recently sent spam. If you are not a spammer, please try later. Listed at ~s",
                                 [inet:ntoa(State#state.peer), Service]),
            {error, Error, State}
    end.


-spec handle_MAIL_extension(Extension :: binary(), State :: #state{}) -> {'ok', #state{}} | 'error'.
handle_MAIL_extension(_Extension, State) ->
    {ok, State}.

-spec handle_RCPT(To :: binary(), State :: #state{}) -> {'ok', #state{}} | {'error', string(), #state{}}.
handle_RCPT(To, State) ->
    % Check if the "To" address exists
    % Check domain, check addressee in domain.
    % For bounces:
    % - To = <noreply+MSGID@example.org>
    % - Return-Path header should be present and contains <>
    case zotonic_listen_smtp_receive:get_site(To) of
        {ok, Site} ->
            ?LOG_INFO(#{
                text => <<"SMTP accepting incoming email">>,
                in => zotonic_listen_smtp,
                recipient => To,
                site => Site,
                src => inet:ntoa(State#state.peer),
                protocol => smtp
            }),
            {ok, State};
        {error, unknown_host} ->
            ?LOG_WARNING(#{
                text => <<"SMTP not accepting mail">>,
                in => zotonic_listen_smtp,
                reason => unknown_host,
                recipient => To,
                src => inet:ntoa(State#state.peer),
                protocol => smtp
            }),
            {error, "551 User not local. Relay denied.", State};
        {error, not_running} ->
            ?LOG_WARNING(#{
                text => <<"SMTP not accepting mail for site">>,
                in => zotonic_listen_smtp,
                reason => not_running,
                recipient => To,
                src => inet:ntoa(State#state.peer),
                protocol => smtp
            }),
            {error, "453 System not accepting network messages.", State}
        % {error, Reason} ->
        %     ?LOG_INFO("SMTP not accepting mail for ~p: ~p", [Reason]),
        %     {error, "451 Server error. Please try again later.", State}
    end.

-spec handle_RCPT_extension(Extension :: binary(), State :: #state{}) -> {'ok', #state{}}.
handle_RCPT_extension(_Extension, State) ->
    {ok, State}.

-spec handle_DATA(From :: binary(), To :: [binary(),...], Data :: binary(), State :: #state{}) -> {'ok', binary(), #state{}} | {'error', string(), #state{}}.
handle_DATA(From, To, Data, State) ->
    MsgId = z_ids:id(32),
    DataRcvd = add_received_header(Data, MsgId, State),
    decode_and_receive(MsgId, From, To, DataRcvd, State).

-spec handle_RSET(State :: #state{}) -> #state{}.
handle_RSET(State) ->
    % reset any relevant internal state
    reset_state(State).

-spec handle_VRFY(Address :: binary(), State :: #state{}) -> {'ok', string(), #state{}} | {'error', string(), #state{}}.
handle_VRFY(_Address, State) ->
    {error, "252 VRFY disabled by policy, just send some mail", State}.

-spec handle_other(Verb :: binary(), Args :: binary(), #state{}) -> {string(), #state{}}.
handle_other(Verb, _Args, State) ->
    {lists:flatten(io_lib:format("500 Error: command not recognized : '~s'", [Verb])), State}.


-spec code_change(OldVsn :: any(), State :: #state{}, Extra :: any()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec terminate(Reason :: any(), State :: #state{}) -> {'ok', any(), #state{}}.
terminate(Reason, State) ->
    {ok, Reason, State}.


%%% ------------------------------------------------------------------------------------------
%%% Internal functions
%%% ------------------------------------------------------------------------------------------

reset_state(State) ->
    State#state{helo=undefined, rcpt=undefined, from=undefined}.

decode_and_receive(MsgId, From, To, DataRcvd, State) ->
    case decode(DataRcvd) of
        {ok, {Type, Subtype, Headers, _Params, Body} = Decoded} ->
            case find_bounce_id({Type, Subtype}, To, Headers) of
                {ok, MessageId} ->
                    ?LOG_NOTICE(#{
                        text => <<"SMTP email is bounce of previous message id">>,
                        in => zotonic_listen_smtp,
                        recipient => To,
                        from => From,
                        message_id => MessageId,
                        src => inet:ntoa(State#state.peer),
                        protocol => smtp
                    }),
                    % The e-mail server knows about the messages sent from our system.
                    % Only report fatal bounces, silently ignore delivery warnings
                    case zotonic_listen_smtp_check:is_nonfatal_bounce({Type, Subtype}, Headers, Body) of
                        true -> nop;
                        false -> z_email_server:bounced(State#state.peer, MessageId)
                    end,
                    {ok, MsgId, reset_state(State)};
                bounce ->
                    % Bounced, but without a message id (accept & silently drop the message)
                    ?LOG_NOTICE(#{
                        text => <<"SMTP email is bounce of unknown message id">>,
                        in => zotonic_listen_smtp,
                        recipient => To,
                        from => From,
                        src => inet:ntoa(State#state.peer),
                        protocol => smtp
                    }),
                    {ok, MsgId, reset_state(State)};
                maybe_autoreply ->
                    % Sent to a bounce address, but not a bounce (accept & silently drop the message)
                    ?LOG_NOTICE(#{
                        text => <<"SMTP email is an autoreply, ignored">>,
                        in => zotonic_listen_smtp,
                        recipient => To,
                        from => From,
                        src => inet:ntoa(State#state.peer),
                        protocol => smtp
                    }),
                    {ok, MsgId, reset_state(State)};
                no_bounce ->
                    receive_data(zotonic_listen_smtp_spam:spam_check(DataRcvd),
                                 Decoded, MsgId, From, To, DataRcvd, State)
            end;
        {error, Reason} ->
            ?LOG_ERROR(#{
                text => <<"SMTP receive: Message decode FAILED">>,
                in => zotonic_listen_smtp,
                reason => Reason,
                recipient => To,
                from => From,
                src => inet:ntoa(State#state.peer),
                protocol => smtp
            }),
            {error, "550 Your email cannot be parsed", State}
    end.

receive_data({ok, {ham, SpamStatus, _SpamHeaders}}, {Type, Subtype, Headers, Params, Body}, MsgId, From, To, DataRcvd, State) ->
    ?LOG_NOTICE(#{
        text => <<"SMTP email received">>,
        in => zotonic_listen_smtp,
        recipient => To,
        from => From,
        message_id => MsgId,
        src => inet:ntoa(State#state.peer),
        spam_status => SpamStatus
    }),
    Received = zotonic_listen_smtp_receive:received(To, From, State#state.peer, MsgId,
                                        {Type, Subtype}, Headers, Params, Body, DataRcvd),
    reply_handled_status(Received, MsgId, reset_state(State));
receive_data({ok, {spam, SpamStatus, _SpamHeaders}}, _Decoded, MsgId, From, To, _DataRcvd, State) ->
    ?LOG_NOTICE(#{
        text => <<"SMTP Refusing spam">>,
        in => zotonic_listen_smtp,
        recipient => To,
        from => From,
        message_id => MsgId,
        src => inet:ntoa(State#state.peer),
        spam_status => SpamStatus
    }),
    {error, zotonic_listen_smtp_spam:smtp_status(SpamStatus, From, To, State#state.peer), reset_state(State)};
receive_data({error, Reason}, Decoded, MsgId, From, To, DataRcvd, State) ->
    ?LOG_WARNING(#{
        text => <<"SMTP receive: passing erronous spam check as ham">>,
        in => zotonic_listen_smtp,
        reason => Reason,
        recipient => To,
        from => From,
        message_id => MsgId,
        src => inet:ntoa(State#state.peer)
    }),
    receive_data({ok, {ham, [], []}}, Decoded, MsgId, From, To, DataRcvd, State).

reply_handled_status(Received, MsgId, State) ->
    IsUnknownHost = is_unknown_host(Received),
    IsUnknownUser = is_unknown_user(Received),
    IsSiteDown = is_site_not_running(Received),
    IsAnyError = is_any_error(Received),
    case {IsSiteDown, IsUnknownHost, IsUnknownUser, IsAnyError} of
        {_Down, true, _User, IsAnyError} ->
            {error, "551 User not local. Relay denied.", State};
        {_Down, _Host, true, IsAnyError} ->
            {error, "550 No such user here.", State};
        {true, _Host, _User, IsAnyError} ->
            {error, "453 System not accepting network messages.", State};
        {_Down, _Host, _User, true} ->
            {error, "451 Server error. Please try again later.", State};
        {false, false, false, false} ->
            {ok, MsgId, State}
    end.

is_unknown_host([]) -> false;
is_unknown_host([ {error, unknown_host} | _ ]) -> true;
is_unknown_host([ _ | Rs ]) -> is_unknown_host(Rs).

is_unknown_user([]) -> false;
is_unknown_user([ {error, unknown_recipient} | _ ]) -> true;
is_unknown_user([ _ | Rs ]) -> is_unknown_user(Rs).

is_site_not_running([]) -> false;
is_site_not_running([ {error, not_running} | _ ]) -> true;
is_site_not_running([ _ | Rs ]) -> is_site_not_running(Rs).

is_any_error([]) -> false;
is_any_error([ {error, _} | _ ]) -> true;
is_any_error([ _ | Rs ]) -> is_any_error(Rs).


%%% If the message is a {<<"multipart">>,<<"report">>} then here is also
%%% a {<<"message">>,<<"rfc822">>} part that contains the original message.
%%% From that original message we can find the original message id

%% @doc A message is classified as a bounce if the Return-Path is set to an empty address
%%      and other appropriate headers are present. Try to match noreply+MSGID@@example.org
find_bounce_id(Type, Recipients, Headers) ->
    case zotonic_listen_smtp_check:is_bounce(Type, Headers) of
        true ->
            case find_bounce_email(Recipients) of
                {ok, _MessageId} = M -> M;
                undefined -> bounce
            end;
        false ->
            case find_bounce_email(Recipients) of
                {ok, _MessageId} -> maybe_autoreply;
                undefined -> no_bounce
            end
    end.


% Check if one of the recipients is a bounce address
find_bounce_email([]) ->
    undefined;
find_bounce_email([To|Other]) ->
    case z_email_server:is_bounce_email_address(To) of
        true -> {ok, To};
        false -> find_bounce_email(Other)
    end.

decode(Data) ->
    try
        {ok, mimemail:decode(Data)}
    catch
        Type:Reason ->
            {error, {Type,Reason}}
    end.

%% TODO: Add 'for user@example.com' header (depends on recipient, we might have multiple)
%% TODO: Add Return-Path
add_received_header(Data, MsgId, State) ->
    iolist_to_binary([
        <<"Received:">>,
        <<" from [">>, inet_parse:ntoa(State#state.peer), <<"] (helo=">>, filter_string(State#state.helo), <<")">>,
        <<"\r\n\tby ">>, State#state.hostname, <<" with ESMTP (Zotonic)">>,
        <<"\r\n\t(envelope-from <">>, filter_string(State#state.from), <<">)">>,
        <<"\r\n\tid ">>, MsgId,
        <<"; ">>, z_dateformat:format(calendar:local_time(), "r", []),
        <<"\r\n">>,
        Data
    ]).

filter_string(S) ->
    lists:filter(fun(C) ->
                            C >= 32
                    andalso C =< 126
                    andalso C =/= $[
                    andalso C =/= $]
                    andalso C =/= $(
                    andalso C =/= $)
                    andalso C =/= $<
                    andalso C =/= $>
                    andalso C =/= $\\
                 end,
                 z_convert:to_list(S)).
