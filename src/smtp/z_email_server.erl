%% @author Marc Worrell <marc@worrell.nl>
%% @author Atilla Erdodi <atilla@maximonster.com>
%% @copyright 2010-2015 Maximonster Interactive Things
%% @doc Email server. Queues, renders and sends e-mails.

%% Copyright 2010-2015 Maximonster Interactive Things
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

-module(z_email_server).
-author("Atilla Erdodi <atilla@maximonster.com>").
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% interface functions
-export([
    start_link/0,
    is_bounce_email_address/1,
    bounced/2,
    generate_message_id/0,
    send/2,
    send/3,

    tempfile/0,
    is_tempfile/1,
    is_tempfile_deletable/1,

    is_sender_enabled/2,
    is_sender_enabled/3
]).

-include_lib("zotonic.hrl").
-include_lib("stdlib/include/qlc.hrl").

% Maximum times we retry to send a message before we mark it as failed.
-define(MAX_RETRY, 10).

% Max number of e-mails being sent at the same time
-define(EMAIL_MAX_SENDING, 100).

% Max number of connections per (relay) domain.
-define(EMAIL_MAX_DOMAIN, 5).

% Extension of files with queued copies of tmpfile attachments
-define(TMPFILE_EXT, ".mailspool").


-record(state, {smtp_relay, smtp_relay_opts, smtp_no_mx_lookups,
                smtp_verp_as_from, smtp_bcc, override,
                sending=[], delete_sent_after}).
-record(email_queue, {id, retry_on=inc_timestamp(os:timestamp(), 1), retry=0,
                      recipient, email, created=os:timestamp(), sent,
                      pickled_context}).

-record(email_sender, {id, sender_pid, domain, is_connected=false}).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link() ->
    start_link([]).
%% @spec start_link(Args::list()) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


%% @doc Check if the received e-mail address is a bounce address
is_bounce_email_address(<<"noreply+",_/binary>>) -> true;
is_bounce_email_address("noreply+"++_) -> true;
is_bounce_email_address(_) -> false.

%% @doc Handle a bounce
bounced(Peer, NoReplyEmail) ->
    gen_server:cast(?MODULE, {bounced, Peer, NoReplyEmail}).


%% @doc Generate a new message id
generate_message_id() ->
    z_ids:random_id('az09', 20).

%% @doc Send an email
send(#email{} = Email, Context) ->
    send(generate_message_id(), Email, Context).

%% @doc Send an email using a predefined unique id.
send(Id, #email{} = Email, Context) ->
    case is_sender_enabled(Email, Context) of
        true ->
            Email1 = copy_attachments(Email),
            Context1 = z_context:depickle(z_context:pickle(Context)),
            gen_server:cast(?MODULE, {send, Id, Email1, Context1}),
            {ok, Id};
        false ->
            {error, sender_disabled}
    end.

%% @doc Return the filename for a tempfile that can be used for the emailer
tempfile() ->
    z_tempfile:tempfile(?TMPFILE_EXT).

%% @doc Check if a file is a tempfile of the emailer
is_tempfile(File) when is_list(File) ->
    z_tempfile:is_tempfile(File) andalso filename:extension(File) =:= ?TMPFILE_EXT.

%% @doc Return the max age of a tempfile
is_tempfile_deletable(undefined) ->
    false;
is_tempfile_deletable(File) ->
    case is_tempfile(File) of
        true ->
            case filelib:last_modified(File) of
                0 ->
                    false;
                Modified when is_tuple(Modified) ->
                    ModifiedSecs = calendar:datetime_to_gregorian_seconds(Modified),
                    NowSecs = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
                    NowSecs > max_tempfile_age() + ModifiedSecs
            end;
        false ->
            true
    end.

%% @doc Max tempfile age in seconds
max_tempfile_age() ->
    max_tempfile_age(?MAX_RETRY, 0) + 24*3600.

max_tempfile_age(0, Acc) -> Acc;
max_tempfile_age(N, Acc) -> max_tempfile_age(N-1, period(N) + Acc).


%% @doc Check if the sender is allowed to send email. If an user is disabled they are only
%%      allowed to send mail to themselves or to the admin.
is_sender_enabled(#email{} = Email, Context) ->
    is_sender_enabled(z_acl:user(Context), Email#email.to, Context).

is_sender_enabled(undefined, _RecipientEmail, _Context) ->
    true;
is_sender_enabled(1, _RecipientEmail, _Context) ->
    true;
is_sender_enabled(Id, RecipientEmail, Context) when is_list(RecipientEmail) ->
    is_sender_enabled(Id, z_convert:to_binary(RecipientEmail), Context);
is_sender_enabled(Id, RecipientEmail, Context) when is_integer(Id) ->
    (m_rsc:exists(Id, Context) andalso z_convert:to_bool(m_rsc:p_no_acl(Id, is_published, Context)))
    orelse recipient_is_user_or_admin(Id, RecipientEmail, Context).

recipient_is_user_or_admin(Id, RecipientEmail, Context) ->
    m_config:get_value(zotonic, admin_email, Context) =:= RecipientEmail
    orelse m_rsc:p_no_acl(1, email, Context) =:= RecipientEmail
    orelse m_rsc:p_no_acl(Id, email, Context) =:= RecipientEmail
    orelse lists:any(fun(Idn) ->
                        proplists:get_value(key, Idn) =:= RecipientEmail
                     end,
                     m_identity:get_rsc_by_type(Id, email, Context)).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(_Args) ->
    ok = create_email_queue(),
    timer:send_interval(5000, poll),
    State = update_config(#state{}),
    process_flag(trap_exit, true),
    {ok, State}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
handle_call({is_sending_allowed, Pid, Relay}, _From, State) ->
    DomainWorkers = length(lists:filter(
                                fun(#email_sender{domain=Domain, is_connected=IsConnected}) ->
                                    IsConnected andalso Relay =:= Domain
                                end,
                                State#state.sending)),
    case DomainWorkers < ?EMAIL_MAX_DOMAIN of
        true ->
            Workers = [
                    case E#email_sender.sender_pid of
                        Pid -> E#email_sender{is_connected=true};
                        _ -> E
                    end
                    || E <- State#state.sending
                ],
            {reply, ok, State#state{sending=Workers}};
        false ->
            {reply, {error, wait}, State}
    end;

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}

%% @doc Send an e-mail.
handle_cast({send, Id, #email{} = Email, Context}, State) ->
    State1 = update_config(State),
    State2 = case z_utils:is_empty(Email#email.to) of
        true -> State1;
        false -> send_email(Id, Email#email.to, Email, Context, State1)
    end,
    State3 = case z_utils:is_empty(Email#email.cc) of
        true -> State2;
        false -> send_email(<<Id/binary, "+cc">>, Email#email.cc, Email, Context, State2)
    end,
    State4 = case z_utils:is_empty(Email#email.bcc) of
        true -> State3;
        false -> send_email(<<Id/binary, "+bcc">>, Email#email.bcc, Email, Context, State3)
    end,
    {noreply, State4};

%%@ doc Handle a bounced email
handle_cast({bounced, Peer, BounceEmail}, State) ->
    % Fetch the MsgId from the bounce address
    [BounceLocalName,Domain] = binstr:split(z_convert:to_binary(BounceEmail), <<"@">>),
    <<"noreply+", MsgId/binary>> = BounceLocalName,

    % Find the original message in our database of recent sent e-mail
    TrFun = fun()->
                    [QEmail] = mnesia:read(email_queue, MsgId),
                    mnesia:delete_object(QEmail),
                    {(QEmail#email_queue.email)#email.to, QEmail#email_queue.pickled_context}
            end,
    case mnesia:transaction(TrFun) of
        {atomic, {Recipient, PickledContext}} ->
            Context = z_context:depickle(PickledContext),
            z_notifier:notify(#email_bounced{
                                message_nr=MsgId,
                                recipient=Recipient
                            }, Context),
            z_notifier:notify(#zlog{
                                user_id=z_acl:user(Context),
                                props=#log_email{
                                    severity = ?LOG_ERROR,
                                    message_nr = MsgId,
                                    mailer_status = bounce,
                                    mailer_host = z_convert:ip_to_list(Peer),
                                    envelop_to = BounceEmail,
                                    envelop_from = "<>",
                                    to_id = z_acl:user(Context),
                                    props = []
                                }}, Context);
        _ ->
            % We got a bounce, but we don't have the message anymore.
            % Custom bounce domains make this difficult to process.
            case z_sites_dispatcher:get_host_for_domain(Domain) of
                {ok, Host} ->
                    Context = z_context:new(Host),
                    z_notifier:notify(#email_bounced{
                                    message_nr=MsgId,
                                    recipient=undefined
                                }, Context),
                    z_notifier:notify(#zlog{
                                user_id=undefined,
                                props=#log_email{
                                    severity = ?LOG_WARNING,
                                    message_nr = MsgId,
                                    mailer_status = bounce,
                                    mailer_host = z_convert:ip_to_list(Peer),
                                    envelop_to = BounceEmail,
                                    envelop_from = "<>",
                                    props = []
                                }}, Context);
                undefined ->
                    ignore
            end
    end,
    {noreply, State};

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @doc Poll the database queue for any retrys.
handle_info(poll, State) ->
    State1 = poll_queued(State),
    z_utils:flush_message(poll),
    {noreply, State1};

%% @doc Spawned process has crashed. Clear it from the sending list.
handle_info({'EXIT', Pid, _Reason}, State) ->
    {noreply, remove_worker(Pid, State)};

%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

%% @doc Create the email queue in mnesia
create_email_queue() ->
    TabDef = [
        {type, set},
        {record_name, email_queue},
        {attributes, record_info(fields, email_queue)}
        | case application:get_env(mnesia, dir) of
             {ok, _} -> [ {disc_copies, [node()]} ];
             undefined -> []
          end
    ],
    case mnesia:create_table(email_queue, TabDef) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, email_queue}} -> ok
    end.


%% @doc Refetch the emailer configuration so that we adapt to any config changes.
update_config(State) ->
    SmtpRelay = z_config:get(smtp_relay),
    SmtpRelayOpts =
        case SmtpRelay of
            true ->
                [{relay, z_config:get(smtp_host, "localhost")},
                 {port, z_config:get(smtp_port, 25)},
                 {ssl, z_config:get(smtp_ssl, false)}]
                ++ case {z_config:get(smtp_username),
                         z_config:get(smtp_password)} of
                        {undefined, undefined} ->
                            [];
                        {User, Pass} ->
                            [{auth, always},
                             {username, User},
                             {password, Pass}]
                   end;
            false ->
                []
        end,
    SmtpNoMxLookups = z_config:get(smtp_no_mx_lookups),
    SmtpVerpAsFrom = z_config:get(smtp_verp_as_from),
    SmtpBcc = z_config:get(smtp_bcc),
    Override = z_config:get(email_override),
    DeleteSentAfter = z_config:get(smtp_delete_sent_after),
    State#state{smtp_relay=SmtpRelay,
                smtp_relay_opts=SmtpRelayOpts,
                smtp_no_mx_lookups=SmtpNoMxLookups,
                smtp_verp_as_from=SmtpVerpAsFrom,
                smtp_bcc=SmtpBcc,
                override=Override,
                delete_sent_after=DeleteSentAfter}.


%% @doc Get the bounce email address. Can be overridden per site in config setting site.bounce_email_override.
bounce_email(MessageId, Context) ->
    case m_config:get_value(site, bounce_email_override, Context) of
        undefined ->
            case z_config:get(smtp_bounce_email_override) of
                undefined -> "noreply+"++MessageId;
                VERP -> z_convert:to_list(VERP)
            end;
        VERP ->
            z_convert:to_list(VERP)
    end.

reply_email(MessageId, Context) ->
    "reply+"++z_convert:to_list(MessageId)++[$@ | z_email:email_domain(Context)].


% The 'From' is either the message id (and bounce domain) or the set from.
get_email_from(EmailFrom, VERP, State, Context) ->
    From = case EmailFrom of
        L when L =:= [] orelse L =:= undefined orelse L =:= <<>> ->
            get_email_from(Context);
        _ -> EmailFrom
    end,
    case State#state.smtp_verp_as_from of
        true ->
            {FromName, _FromEmail} = z_email:split_name_email(From),
            string:strip(FromName ++ " " ++ VERP);
        _ ->
            {FromName, FromEmail} = z_email:split_name_email(From),
            case FromEmail of
                [] -> string:strip(FromName ++ " <" ++ get_email_from(Context) ++ ">");
                _ -> From
            end
    end.

% When the 'From' is not the VERP then the 'From' is derived from the site
get_email_from(Context) ->
    %% Let the default be overruled by the config setting
    case m_config:get_value(site, email_from, Context) of
        undefined -> "noreply@" ++ z_email:email_domain(Context);
        EmailFrom -> z_convert:to_list(EmailFrom)
    end.

% Unique message-id, depends on bounce domain
message_id(MessageId, Context) ->
    z_convert:to_list(MessageId)++[$@ | z_email:bounce_domain(Context)].

%% @doc Remove a worker Pid from the server state.
remove_worker(Pid, State) ->
    Filtered = lists:filter(fun(#email_sender{sender_pid=P}) -> P =/= Pid end, State#state.sending),
    State#state{sending=Filtered}.

%% =========================
%% SENDING related functions
%% =========================

% Send an email
send_email(Id, Recipient, Email, Context, State) ->
    QEmail = #email_queue{id=Id,
                          recipient=Recipient,
                          email=Email,
                          retry_on=inc_timestamp(os:timestamp(), 0),
                          sent=undefined,
                          pickled_context=z_context:pickle(Context)},
    QEmailTransFun = fun() -> mnesia:write(QEmail) end,
    {atomic, ok} = mnesia:transaction(QEmailTransFun),
    case Email#email.queue orelse length(State#state.sending) > ?EMAIL_MAX_SENDING of
        true -> State;
        false -> spawn_send(Id, Recipient, Email, Context, State)
    end.


spawn_send(Id, Recipient, Email, Context, State) ->
    case lists:keyfind(Id, #email_sender.id, State#state.sending) =/= false of
        false ->
            spawn_send_check_email(Id, Recipient, Email, Context, State);
        _ ->
            %% Is already being sent. Do nothing, it will retry later
            State
    end.

spawn_send_check_email(Id, Recipient, Email, Context, State) ->
    case is_sender_enabled(Email, Context) of
        true ->
            case is_valid_email(Recipient) of
                true ->
                    spawn_send_checked(Id, Recipient, Email, Context, State);
                false ->
                    %% delete email from the queue and notify the system
                    delete_email(illegal_address, Id, Recipient, Email, Context),
                    State
            end;
        false ->
            delete_email(sender_disabled, Id, Recipient, Email, Context),
            State
    end.

delete_email(Error, Id, Recipient, Email, Context) ->
    delete_emailq(Id),
    z_notifier:first(#email_failed{
            message_nr=Id,
            recipient=Recipient,
            is_final=true,
            status= case Error of
                        illegal_address -> <<"Malformed email address">>;
                        sender_disabled -> <<"Sender disabled">>
                    end,
            reason=Error
        }, Context),
    LogEmail = #log_email{
        severity=?LOG_ERROR,
        mailer_status=error,
        props=[{reason, Error}],
        message_nr=Id,
        envelop_to=Recipient,
        envelop_from="",
        to_id=proplists:get_value(recipient_id, Email#email.vars),
        from_id=z_acl:user(Context),
        content_id=proplists:get_value(id, Email#email.vars),
        other_id=proplists:get_value(list_id, Email#email.vars),
        message_template=Email#email.html_tpl
    },
    z_notifier:notify(#zlog{user_id=z_acl:user(Context), props=LogEmail}, Context).


% Start a worker, prevent too many workers per domain.
spawn_send_checked(Id, Recipient, Email, Context, State) ->
    Recipient1 = check_override(Recipient, m_config:get_value(site, email_override, Context), State),
    Recipient2 = string:strip(z_string:line(binary_to_list(z_convert:to_binary(Recipient1)))),
    {_RcptName, RecipientEmail} = z_email:split_name_email(Recipient2),
    [_RcptLocalName, RecipientDomain] = string:tokens(RecipientEmail, "@"),
    SmtpOpts = [
        {no_mx_lookups, State#state.smtp_no_mx_lookups},
        {hostname, z_email:email_domain(Context)}
        | case State#state.smtp_relay of
            true -> State#state.smtp_relay_opts;
            false -> [{relay, RecipientDomain}]
          end
    ],
    BccSmtpOpts = case z_utils:is_empty(State#state.smtp_bcc) of
                      true ->
                            [];
                      false ->
                            {_BccName, BccEmail} = z_email:split_name_email(State#state.smtp_bcc),
                            [_BccLocalName, BccDomain] = string:tokens(BccEmail, "@"),
                            [
                                {no_mx_lookups, State#state.smtp_no_mx_lookups},
                                {hostname, z_email:email_domain(Context)}
                                | case State#state.smtp_relay of
                                    true -> State#state.smtp_relay_opts;
                                    false -> [{relay, BccDomain}]
                                  end
                            ]
                  end,
    MessageId = message_id(Id, Context),
    VERP = "<"++bounce_email(MessageId, Context)++">",
    From = get_email_from(Email#email.from, VERP, State, Context),
    SenderPid = erlang:spawn_link(
                    fun() ->
                        spawned_email_sender(
                                Id, MessageId, Recipient, RecipientEmail, VERP,
                                From, State#state.smtp_bcc, Email, SmtpOpts, BccSmtpOpts,
                                Context)
                    end),
    {relay, Relay} = proplists:lookup(relay, SmtpOpts),
    State#state{
            sending=[
                #email_sender{id=Id, sender_pid=SenderPid, domain=Relay} | State#state.sending
            ]}.

spawned_email_sender(Id, MessageId, Recipient, RecipientEmail, VERP, From,
                     Bcc, Email, SmtpOpts, BccSmtpOpts, Context) ->
    EncodedMail = encode_email(Id, Email, "<"++MessageId++">", From, Context),
    spawned_email_sender_loop(Id, MessageId, Recipient, RecipientEmail, VERP, From,
                              Bcc, Email, EncodedMail, SmtpOpts, BccSmtpOpts, Context).

spawned_email_sender_loop(Id, MessageId, Recipient, RecipientEmail, VERP, From,
                          Bcc, Email, EncodedMail, SmtpOpts, BccSmtpOpts, Context) ->
    {relay, Relay} = proplists:lookup(relay, SmtpOpts),
    case gen_server:call(?MODULE, {is_sending_allowed, self(), Relay}) of
        {error, wait} ->
            lager:debug("[smtp] Delaying email to ~p (~p), too many parallel senders for relay ~p",
                        [RecipientEmail, Id, Relay]),
            timer:sleep(1000),
            spawned_email_sender(Id, MessageId, Recipient, RecipientEmail, VERP, From,
                                 Bcc, Email, SmtpOpts, BccSmtpOpts, Context);
        ok ->
            LogEmail = #log_email{
                message_nr=Id,
                envelop_to=RecipientEmail,
                envelop_from=VERP,
                to_id=proplists:get_value(recipient_id, Email#email.vars),
                from_id=z_acl:user(Context),
                content_id=proplists:get_value(id, Email#email.vars),
                other_id=proplists:get_value(list_id, Email#email.vars), %% Supposed to contain the mailinglist id
                message_template=Email#email.html_tpl
            },
            z_notifier:notify(#zlog{
                                user_id=LogEmail#log_email.from_id,
                                props=LogEmail#log_email{severity=?LOG_INFO, mailer_status=sending}
                              }, Context),

            lager:info("[smtp] Sending email to ~p (~p), via relay ~p",
                       [RecipientEmail, Id, Relay]),

            %% use the unique id as 'envelope sender' (VERP)
            case gen_smtp_client:send_blocking({VERP, [RecipientEmail], EncodedMail}, SmtpOpts) of
                {error, retries_exceeded, {_FailureType, Host, Message}} ->
                    %% do nothing, it will retry later
                    z_notifier:notify(#email_failed{
                            message_nr=Id,
                            recipient=Recipient,
                            is_final=false,
                            reason=retry,
                            status=Message
                        }, Context),
                    z_notifier:notify(#zlog{
                                        user_id=LogEmail#log_email.from_id,
                                        props=LogEmail#log_email{
                                                severity=?LOG_WARNING,
                                                mailer_status=retry,
                                                mailer_message=Message,
                                                mailer_host=Host
                                            }
                                      }, Context),
                    ok;
                {error, no_more_hosts, {permanent_failure, Host, Message}} ->
                    % classify this as a permanent failure, something is wrong with the receiving server or the recipient
                    z_notifier:notify(#email_failed{
                            message_nr=Id,
                            recipient=Recipient,
                            is_final=true,
                            reason=smtphost,
                            status=Message
                        }, Context),
                    z_notifier:notify(#zlog{
                                        user_id=LogEmail#log_email.from_id,
                                        props=LogEmail#log_email{
                                                severity = ?LOG_ERROR,
                                                mailer_status = bounce,
                                                mailer_message = Message,
                                                mailer_host = Host
                                            }
                                      }, Context),
                    % delete email from the queue and notify the system
                    delete_emailq(Id);
                {error, Reason} ->
                    % Returned when the options are not ok
                    z_notifier:notify(#email_failed{
                            message_nr=Id,
                            recipient=Recipient,
                            is_final=true,
                            reason=error
                        }, Context),
                    z_notifier:notify(#zlog{
                                        user_id=LogEmail#log_email.from_id,
                                        props=LogEmail#log_email{
                                                severity=?LOG_ERROR,
                                                mailer_status=error,
                                                props=[{reason, Reason}]
                                            }
                                      }, Context),
                    %% delete email from the queue and notify the system
                    delete_emailq(Id);
                Receipt when is_binary(Receipt) ->
                    z_notifier:notify(#email_sent{
                            message_nr=Id,
                            recipient=Recipient,
                            is_final=false
                        }, Context),
                    z_notifier:notify(#zlog{
                                        user_id=LogEmail#log_email.from_id,
                                        props=LogEmail#log_email{
                                                severity=?LOG_INFO,
                                                mailer_status=sent,
                                                mailer_message=Receipt
                                            }
                                      }, Context),
                    %% email accepted by relay
                    mark_sent(Id),
                    %% async send a copy for debugging if necessary
                    case z_utils:is_empty(Bcc) of
                        true ->
                            ok;
                        false ->
                            catch gen_smtp_client:send({VERP, [Bcc], EncodedMail}, BccSmtpOpts)
                    end
            end
    end.


encode_email(_Id, #email{raw=Raw}, _MessageId, _From, _Context) when is_list(Raw); is_binary(Raw) ->
    z_convert:to_binary([
        "X-Mailer: Zotonic ", ?ZOTONIC_VERSION, " (http://zotonic.com)\r\n",
        Raw
    ]);
encode_email(Id, #email{body=undefined} = Email, MessageId, From, Context) ->
    %% Optionally render the text and html body
    Vars = [{email_to, Email#email.to}, {email_from, From} | Email#email.vars],
    ContextRender = set_recipient_prefs(Vars, Context),
    Text = optional_render(Email#email.text, Email#email.text_tpl, Vars, ContextRender),
    Html = optional_render(Email#email.html, Email#email.html_tpl, Vars, ContextRender),

    %% Fetch the subject from the title of the HTML part or from the Email record
    Subject = case {Html, Email#email.subject} of
                      {[], undefined} ->
                          <<>>;
                      {_Html, undefined} ->
                          {match, [_, {Start,Len}|_]} = re:run(Html, "<title>(.*?)</title>", [dotall, caseless]),
                          string:strip(z_string:line(z_html:unescape(lists:sublist(Html, Start+1, Len))));
                      {_Html, Sub} ->
                          Sub
                  end,
    Headers = [{"From", From},
               {"To", z_convert:to_list(Email#email.to)},
               {"Subject", z_convert:to_flatlist(Subject)},
               {"Date", date(Context)},
               {"MIME-Version", "1.0"},
               {"Message-ID", MessageId},
               {"X-Mailer", "Zotonic " ++ ?ZOTONIC_VERSION ++ " (http://zotonic.com)"}],
    Headers2 = add_reply_to(Id, Email, add_cc(Email, Headers), Context),
    build_and_encode_mail(Headers2, Text, Html, Email#email.attachments, Context);
encode_email(Id, #email{body=Body} = Email, MessageId, From, Context) when is_tuple(Body) ->
    Headers = [{<<"From">>, From},
               {<<"To">>, Email#email.to},
               {<<"Message-ID">>, MessageId},
               {<<"X-Mailer">>, "Zotonic " ++ ?ZOTONIC_VERSION ++ " (http://zotonic.com)"}
                | Email#email.headers ],
    Headers2 = add_reply_to(Id, Email, add_cc(Email, Headers), Context),
    {BodyType, BodySubtype, BodyHeaders, BodyParams, BodyParts} = Body,
    MailHeaders = [
        {z_convert:to_binary(H), z_convert:to_binary(V)} || {H,V} <- (Headers2 ++ BodyHeaders)
    ],
    mimemail:encode({BodyType, BodySubtype, MailHeaders, BodyParams, BodyParts}, opt_dkim(Context));
encode_email(Id, #email{body=Body} = Email, MessageId, From, Context) when is_list(Body); is_binary(Body) ->
    Headers = [{"From", From},
               {"To", z_convert:to_list(Email#email.to)},
               {"Message-ID", MessageId},
               {"X-Mailer", "Zotonic " ++ ?ZOTONIC_VERSION ++ " (http://zotonic.com)"}
                | Email#email.headers ],
    Headers2 = add_reply_to(Id, Email, add_cc(Email, Headers), Context),
    iolist_to_binary([ encode_headers(Headers2), "\r\n\r\n", Body ]).

    date(Context) ->
        z_convert:to_list(z_datetime:format("r", z_context:set_language(en, Context))).


    add_cc(#email{cc=undefined}, Headers) ->
        Headers;
    add_cc(#email{cc=[]}, Headers) ->
        Headers;
    add_cc(#email{cc=Cc}, Headers) ->
        Headers ++ [{"Cc", Cc}].

    add_reply_to(_Id, #email{reply_to=undefined}, Headers, _Context) ->
        Headers;
    add_reply_to(_Id, #email{reply_to = <<>>}, Headers, _Context) ->
        [{"Reply-To", "<>"} | Headers];
    add_reply_to(Id, #email{reply_to=message_id}, Headers, Context) ->
        [{"Reply-To", reply_email(Id, Context)} | Headers];
    add_reply_to(_Id, #email{reply_to=ReplyTo}, Headers, Context) ->
        {Name, Email} = z_email:split_name_email(ReplyTo),
        ReplyTo1 = string:strip(Name ++ " <" ++ z_email:ensure_domain(Email, Context) ++ ">"),
        [{"Reply-To", ReplyTo1} | Headers].


build_and_encode_mail(Headers, Text, Html, Attachment, Context) ->
    Headers1 = [
        {z_convert:to_binary(H), z_convert:to_binary(V)} || {H,V} <- Headers
    ],
    Params = [
        {<<"content-type-params">>, [ {<<"charset">>, <<"utf-8">>} ]},
        {<<"disposition">>, <<"inline">>},
        {<<"transfer-encoding">>, <<"quoted-printable">>},
        {<<"disposition-params">>, []}
    ],
    Parts = case z_utils:is_empty(Text) of
        true ->
            case z_utils:is_empty(Html) of
                true ->
                    [];
                false ->
                    [{<<"text">>, <<"plain">>, [], Params,
                     expand_cr(z_convert:to_binary(z_markdown:to_markdown(Html, [no_html])))}]
            end;
        false ->
            [{<<"text">>, <<"plain">>, [], Params,
             expand_cr(z_convert:to_binary(Text))}]
    end,
    Parts1 = case z_utils:is_empty(Html) of
        true ->
            Parts;
        false ->
            z_email_embed:embed_images(Parts ++ [{<<"text">>, <<"html">>, [], Params, z_convert:to_binary(Html)}], Context)
    end,
    case Attachment of
        [] ->
            case Parts1 of
                [{T,ST,[],Ps,SubParts}] -> mimemail:encode({T,ST,Headers1,Ps,SubParts}, opt_dkim(Context));
                _MultiPart -> mimemail:encode({<<"multipart">>, <<"alternative">>, Headers1, [], Parts1}, opt_dkim(Context))
            end;
        _ ->
            AttsEncoded = [ encode_attachment(Att, Context) || Att <- Attachment ],
            AttsEncodedOk = lists:filter(fun({error, _}) -> false; (_) -> true end, AttsEncoded),
            mimemail:encode({<<"multipart">>, <<"mixed">>,
                             Headers1,
                             [],
                             [ {<<"multipart">>, <<"alternative">>, [], [], Parts1} | AttsEncodedOk ]
                            }, opt_dkim(Context))
    end.

    encode_attachment(Att, Context) when is_integer(Att) ->
        case m_media:get(Att, Context) of
            undefined ->
                {error, no_medium};
            Props ->
                Upload = #upload{
                            tmpfile=filename:join(z_path:media_archive(Context),
                                                   proplists:get_value(filename, Props)),
                            mime=proplists:get_value(mime, Props)
                        },
                encode_attachment(Upload, Context)
        end;
    encode_attachment(#upload{mime=undefined, data=undefined, tmpfile=File, filename=Filename} = Att, Context) ->
        case z_media_identify:identify(File, Filename, Context) of
            {ok, Ps} ->
                Mime = proplists:get_value(mime, Ps, <<"application/octet-stream">>),
                encode_attachment(Att#upload{mime=Mime}, Context);
            {error, _} ->
                encode_attachment(Att#upload{mime= <<"application/octet-stream">>}, Context)
        end;
    encode_attachment(#upload{mime=undefined, filename=Filename} = Att, Context) ->
        Mime = z_media_identify:guess_mime(Filename),
        encode_attachment(Att#upload{mime=Mime}, Context);
    encode_attachment(#upload{} = Att, _Context) ->
        Data = case Att#upload.data of
                    undefined ->
                        {ok, FileData} = file:read_file(Att#upload.tmpfile),
                        FileData;
                    AttData ->
                       AttData
               end,
        [Type, Subtype] = binstr:split(z_convert:to_binary(Att#upload.mime), <<"/">>, 2),
        {
            Type, Subtype,
            [],
            [
                {<<"transfer-encoding">>, <<"base64">>},
                {<<"disposition">>, <<"attachment">>},
                {<<"disposition-params">>, [{<<"filename">>, filename(Att)}]}
            ],
            Data
        }.

        filename(#upload{filename=undefined, tmpfile=undefined}) ->
            <<"untitled">>;
        filename(#upload{filename=undefined, tmpfile=Tmpfile}) ->
            z_convert:to_binary(filename:basename(z_convert:to_list(Tmpfile)));
        filename(#upload{filename=Filename}) ->
            z_convert:to_binary(Filename).



% Make sure that loose \n characters are expanded to \r\n
expand_cr(B) -> expand_cr(B, <<>>).

    expand_cr(<<>>, Acc) -> Acc;
    expand_cr(<<13, 10, R/binary>>, Acc) -> expand_cr(R, <<Acc/binary, 13, 10>>);
    expand_cr(<<10, R/binary>>, Acc) -> expand_cr(R, <<Acc/binary, 13, 10>>);
    expand_cr(<<13, R/binary>>, Acc) -> expand_cr(R, <<Acc/binary, 13, 10>>);
    expand_cr(<<C, R/binary>>, Acc) -> expand_cr(R, <<Acc/binary, C>>).



check_override(EmailAddr, _SiteOverride, _State) when EmailAddr == undefined; EmailAddr == []; EmailAddr == <<>> ->
    undefined;
check_override(EmailAddr, SiteOverride, #state{override=ZotonicOverride}) ->
    UseOverride = case z_utils:is_empty(ZotonicOverride) of
        true -> SiteOverride;
        false -> ZotonicOverride
    end,
    case z_utils:is_empty(UseOverride) of
        true ->
            z_convert:to_list(EmailAddr);
        false ->
            escape_email(z_convert:to_list(EmailAddr)) ++ " (override) <" ++ z_convert:to_list(UseOverride) ++ ">"
    end.


escape_email(Email) ->
   escape_email(Email, []).
escape_email([], Acc) ->
    lists:reverse(Acc);
escape_email([$@|T], Acc) ->
    escape_email(T, [$-,$t,$a,$-|Acc]);
escape_email([H|T], Acc) ->
    escape_email(T, [H|Acc]).

optional_render(undefined, undefined, _Vars, _Context) ->
    [];
optional_render(Text, undefined, _Vars, _Context) ->
    Text;
optional_render(undefined, Template, Vars, Context) ->
    {Output, _Context} = z_template:render_to_iolist(Template, Vars, Context),
    binary_to_list(iolist_to_binary(Output)).

set_recipient_prefs(Vars, Context) ->
    case proplists:get_value(recipient_id, Vars) of
        UserId when is_integer(UserId) ->
            z_notifier:foldl(#user_context{id=UserId}, Context, Context);
        _Other ->
            Context
    end.

%% @doc Mark email as sent by adding the 'sent' timestamp.
%%      This will schedule it for deletion as well.
mark_sent(Id) ->
    Tr = fun() ->
                 case mnesia:read(email_queue, Id) of
                    [QEmail] -> mnesia:write(QEmail#email_queue{sent=os:timestamp()});
                    [] -> {error, notfound}
                end
         end,
    {atomic, Result} = mnesia:transaction(Tr),
    Result.

%% @doc Deletes a message from the queue.
delete_emailq(Id) ->
    Tr = fun()->
                 [QEmail] = mnesia:read(email_queue, Id),
                 mnesia:delete_object(QEmail)
         end,
    {atomic, ok} = mnesia:transaction(Tr).


%%
%% QUEUEING related functions
%%

%% @doc Fetch a new batch of queued e-mails. Deletes failed messages.
poll_queued(State) ->
    %% delete sent messages
    Now = os:timestamp(),
    DelTransFun = fun() ->
                          DelQuery = qlc:q([QEmail || QEmail <- mnesia:table(email_queue),
                                                      QEmail#email_queue.sent =/= undefined andalso
                                                        timer:now_diff(
                                                            inc_timestamp(QEmail#email_queue.sent, State#state.delete_sent_after),
                                                            Now) < 0
                                            ]),
                          DelQueryRes = qlc:e(DelQuery),
                          [ begin
                                mnesia:delete_object(QEmail),
                                {QEmail#email_queue.id,
                                 QEmail#email_queue.recipient,
                                 QEmail#email_queue.pickled_context}
                            end || QEmail <- DelQueryRes ]
                  end,
    {atomic, NotifyList1} = mnesia:transaction(DelTransFun),
    %% notify the system that these emails were sucessfully sent and (probably) received
    lists:foreach(fun({Id, Recipient, PickledContext}) ->
                        z_notifier:notify(#email_sent{
                                    message_nr=Id,
                                    recipient=Recipient,
                                    is_final=true
                                }, z_context:depickle(PickledContext))
                  end,
                  NotifyList1),

    %% delete all messages with too high retry count
    SetFailTransFun = fun() ->
                              PollQuery = qlc:q([QEmail || QEmail <- mnesia:table(email_queue),
                                                 QEmail#email_queue.sent =:= undefined,
                                                 QEmail#email_queue.retry > ?MAX_RETRY]),
                              PollQueryRes = qlc:e(PollQuery),
                              [ begin
                                    mnesia:delete_object(QEmail),
                                    {QEmail#email_queue.id,
                                     QEmail#email_queue.recipient,
                                     QEmail#email_queue.pickled_context}
                                end || QEmail <- PollQueryRes ]
                      end,
    {atomic, NotifyList2} = mnesia:transaction(SetFailTransFun),
    %% notify the system that these emails were failed to be sent
    lists:foreach(fun({Id, Recipient, PickledContext}) ->
                      z_notifier:first(#email_failed{
                                message_nr=Id,
                                recipient=Recipient,
                                is_final=true,
                                reason=retry,
                                status= <<"Retries exceeded">>
                            }, z_context:depickle(PickledContext))
                  end,
                  NotifyList2),

    MaxListSize = ?EMAIL_MAX_SENDING - length(State#state.sending),
    case MaxListSize > 0 of
        true ->
            %% fetch a batch of messages for sending
            FetchTransFun =
                fun() ->
                        Q = qlc:q([QEmail || QEmail <- mnesia:table(email_queue),
                                   %% Should not already have been sent
                                   QEmail#email_queue.sent =:= undefined,
                                   %% Should not be currently sending
                                   proplists:get_value(QEmail#email_queue.id, State#state.sending) =:= undefined,
                                   %% Eligible for retry
                                   timer:now_diff(QEmail#email_queue.retry_on, Now) < 0]),
                        QCursor = qlc:cursor(Q),
                        QFound = qlc:next_answers(QCursor, MaxListSize),
                        ok = qlc:delete_cursor(QCursor),
                        QFound
                end,
            {atomic, Ms} = mnesia:transaction(FetchTransFun),
            %% send the fetched messages
            case Ms of
                [] ->
                    State;
                _  ->
                    State2 = update_config(State),
                    lists:foldl(
                      fun(QEmail, St) ->
                          update_retry(QEmail),
                          spawn_send(QEmail#email_queue.id,
                                     QEmail#email_queue.recipient,
                                     QEmail#email_queue.email,
                                     z_context:depickle(QEmail#email_queue.pickled_context),
                                     St)
                      end,
                      State2, Ms)
            end;
        false ->
            State
    end.


%% @doc Sets the next retry time for an e-mail.
update_retry(QEmail=#email_queue{retry=Retry}) ->
    Period = period(Retry),
    Tr = fun()->
                 mnesia:write(QEmail#email_queue{retry=Retry+1,
                                                 retry_on=inc_timestamp(os:timestamp(), Period)})
         end,
    mnesia:transaction(Tr).

period(0) -> 10;
period(1) -> 60;
period(2) -> 12 * 60;
period(_) -> 24 * 60. % Retry every day for extreme cases


%% @doc Increases a timestamp (as returned by now/0) with a value provided in minutes
inc_timestamp({MegaSec, Sec, MicroSec}, MinToAdd) when is_integer(MinToAdd) ->
    Sec2 = Sec + (MinToAdd * 60),
    Sec3 = Sec2 rem 1000000,
    MegaSec2 = MegaSec + Sec2 div 1000000,
    {MegaSec2, Sec3, MicroSec}.


%% @doc Check if an e-mail address is valid
is_valid_email(Recipient) ->
    Recipient1 = string:strip(z_string:line(binary_to_list(z_convert:to_binary(Recipient)))),
    {_RcptName, RecipientEmail} = z_email:split_name_email(Recipient1),
    case re:run(RecipientEmail, [$^|re()]++"$", [extended]) of
        nomatch   -> false;
        {match,_} -> true
    end.

    re() ->
        "(
                (\"[^\"\\f\\n\\r\\t\\v\\b]+\")
            |   ([\\w\\!\\#\\$\\%\\&\'\\*\\+\\-\\~\\/\\^\\`\\|\\{\\}]+
                    (\\.[\\w\\!\\#\\$\\%\\&\\'\\*\\+\\-\\~\\/\^\`\\|\\{\\}]+)*
                )
        )
        @
        (
            (
                ([A-Za-z0-9\\-])+\\.
            )+
            [A-Za-z\\-]{2,}
        )".

%% @doc Simple header encoding.
encode_header({Header, [V|Vs]}) when is_list(V) ->
    Hdr = lists:map(fun ({K, Value}) when is_list(K), is_list(Value) ->
                            K ++ "=" ++ Value;
                        ({K, Value}) when is_atom(K), is_list(Value) ->
                            atom_to_list(K) ++ "=" ++ Value;
                        (Value) when is_list(Value) -> Value
                    end,
                    [V|Vs]),
    Header ++ ": " ++ string:join(Hdr, ";\r\n  ");
encode_header({Header, Value})
    when Header =:= "To"; Header =:= "From"; Header =:= "Reply-To";
         Header =:= "Cc"; Header =:= "Bcc"; Header =:= "Date";
         Header =:= "Content-Type"; Header =:= "Mime-Version"; Header =:= "MIME-Version";
         Header =:= "Content-Transfer-Encoding" ->
    Value1 = lists:filter(fun(H) -> H >= 32 andalso H =< 126 end, Value),
    Header ++ ": " ++ Value1;
encode_header({Header, Value}) when is_list(Header), is_list(Value) ->
    % Encode all other headers according to rfc2047
    Header ++ ": " ++ rfc2047:encode(Value);
encode_header({Header, Value}) when is_atom(Header), is_list(Value) ->
    atom_to_list(Header) ++ ": " ++ rfc2047:encode(Value).

encode_headers(Headers) ->
    string:join(lists:map(fun encode_header/1, Headers), "\r\n").

%% @doc Copy all tempfiles in the #mail attachments, to prevent automatic deletion while
%% the email is queued.
copy_attachments(#email{attachments=[]} = Email) ->
    Email;
copy_attachments(#email{attachments=Atts} = Email) ->
    Atts1 = [ copy_attachment(Att) || Att <- Atts ],
    Email#email{attachments=Atts1}.

copy_attachment(#upload{tmpfile=File} = Upload) when is_binary(File) ->
    copy_attachment(Upload#upload{tmpfile=binary_to_list(File)});
copy_attachment(#upload{tmpfile=File} = Upload) when is_list(File) ->
    case filename:extension(File) of
        ?TMPFILE_EXT ->
            Upload;
        _Other ->
            case z_tempfile:is_tempfile(File) of
                true ->
                    NewFile = tempfile(),
                    {ok, _Size} = file:copy(File, NewFile),
                    Upload#upload{tmpfile=NewFile};
                false ->
                    Upload
            end
    end;
copy_attachment(Att) ->
    Att.

opt_dkim(Context) ->
    z_email_dkim:mimemail_options(Context).
