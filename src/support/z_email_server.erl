%% @author Marc Worrell <marc@worrell.nl>
%% @author Atilla Erdodi <atilla@maximonster.com>
%% @copyright 2010 Maximonster Interactive Things
%% Date: 2010-11-25
%% @doc Email server.  Queues, renders and sends e-mails.

%% Copyright 2010 Maximonster Interactive Things
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
-author("Marc Worrell <marc@worrell.nl>").
-author("Atilla Erdodi <atilla@maximonster.com>").
-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% interface functions
-export([start_link/0]).

-include_lib("zotonic.hrl").
-include_lib("stdlib/include/qlc.hrl").

% Maximum times we retry to send a message before we mark it as failed.
-define(MAX_RETRY, 7).
% The time in minutes how long sent email should be kept in the queue.
-define(DELETE_AFTER, 240).
% Timeout value for the connection of the spamassassin daemon
-define(SPAMD_TIMEOUT, 10000).

-record(state, {smtp_relay, smtp_relay_opts, smtp_no_mx_lookups,
                smtp_verp_as_from, smtp_bcc, override, smtp_spamd_ip, smtp_spamd_port}).
-record(email_queue, {id, retry_on=inc_timestamp(now(), 10),
                      retry=0, email, created=now(), sent, context}).

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

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(_Args) ->
    process_flag(trap_exit, true),
    mnesia:create_table(email_queue,
                        [{attributes, record_info(fields, email_queue)}]),
    timer:send_interval(5000, poll),
    State = #state{},
    {ok, State}.
	

%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Send an e-mail to an e-mail address.
handle_call({send, #email{} = Email, Context}, _From, State) ->
    State1 = update_config(State),
    Id = generate_message_id(Context),
    PickledContext= z_context:pickle(Context),
    QEmail = #email_queue{id=Id, email=Email,
                          context=PickledContext},
    QEmailTransFun = fun() -> mnesia:write(QEmail) end,
    {atomic, ok} = mnesia:transaction(QEmailTransFun),        
    case Email#email.queue of
        false -> spawn_send(Id, Email, Context, State1);
        true -> ok
    end,
    {reply, {ok, Id}, State1};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
handle_cast({bounced, MsgId}, State) ->
    TrFun = fun()-> 
                    [QEmail] = mnesia:read(email_queue, MsgId), 
                    mnesia:delete_object(QEmail),
                    {(QEmail#email_queue.email)#email.to, QEmail#email_queue.context}
            end,
    case mnesia:transaction(TrFun) of
        {atomic, {Recipient, PickledContext}} ->
            Context = z_context:depickle(PickledContext),
            z_notifier:first({email_bounced, MsgId, Recipient}, Context);
        _ ->
            %% TODO: find out the host, but
            %% custom bounce domains make this tricky...
            %Context = z_context:new(Host),
            %z_notifier:first({email_received, MsgId, Recipient}, Context)
            ok
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

%% @doc Refetch the emailer configuration so that we adapt to any config changes.
update_config(State) ->
    SmtpRelay = z_config:get(smtp_relay),
    SmtpRelayOpts = 
        case SmtpRelay of 
            true ->
                [{relay, z_config:get(smtp_host)},
                 {port, z_config:get(smtp_port)},
                 {ssl, z_config:get(smtp_ssl)}]
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
    SmtpSpamdIp = z_config:get(smtp_spamd_ip),
    SmtpSpamdPort = z_config:get(smtp_spamd_port),
    State#state{smtp_relay=SmtpRelay,
                smtp_relay_opts=SmtpRelayOpts,
                smtp_no_mx_lookups=SmtpNoMxLookups,
                smtp_verp_as_from=SmtpVerpAsFrom,
                smtp_bcc=SmtpBcc,
                override=Override,
                smtp_spamd_ip=SmtpSpamdIp,
                smtp_spamd_port=SmtpSpamdPort}.

generate_message_id(Context) ->
    FQDN = smtp_util:guess_FQDN(),
    BounceDomain = 
        case z_config:get('smtp_bounce_domain') of
		     undefined ->
                [Hostname|_] = string:tokens(z_convert:to_list(m_site:get(hostname, Context)), ":"),
                Hostname;
		     BounceDomain_ -> 
                BounceDomain_
		     end,
    Md5 = [io_lib:format("~2.16.0b", [X]) || <<X>> <= erlang:md5(term_to_binary([erlang:now(), FQDN]))],
    lists:flatten(io_lib:format("<noreply+~s@~s>", [Md5, BounceDomain])).

%% =========================
%% SENDING related functions
%% =========================
spawn_send(Id, Email, Context, State) ->
    F = fun() ->
            To = check_override(Email#email.to, State),
	    Cc = check_override(Email#email.cc, State),            
            From = get_email_from(Email#email.from, Id, State, Context),

            %% Optionally render the text and html body
            Vars = [{email_to, To}, {email_from, From} | Email#email.vars],
            Text = optional_render(Email#email.text, Email#email.text_tpl, Vars, Context),
            Html = optional_render(Email#email.html, Email#email.html_tpl, Vars, Context),

            %% Fetch the subject from the title of the HTML part or from the Email record
            Subject = case {Html, Email#email.subject} of
                              {[], undefined} -> [];
                              {[], Sub} -> Sub;
                              {_Html, undefined} ->
                                  {match, [_, {Start,Len}|_]} = re:run(Html, "<title>(.*)</title>", [dotall, caseless]),
                                  string:strip(z_string:line(lists:sublist(Html, Start+1, Len)))
                          end,

            Headers = [{"From", From},
                       {"To", To},
                       {"Subject", Subject},
                       {"MIME-Version", "1.0"},
                       {"Message-ID", Id},
                       {"X-Mailer",
                          "Zotonic " ++ ?ZOTONIC_VERSION ++ " (http://zotonic.com)"}]
                        ++ case Cc of
                            undefined -> [];
                            _-> [{"Cc", Cc}]
                        end,

            EncodedMail = build_and_encode_mail(Headers, Text, Html, Context),  

            To1 = string:strip(z_string:line(binary_to_list(z_convert:to_binary(To)))),
            {_ToName, ToEmail} = z_email:split_name_email(To1),
            [_ToUsername, ToDomain] = string:tokens(ToEmail, "@"),
            
            SmtpOpts = 
                case State#state.smtp_relay of
                    true ->
                        [{no_mx_lookups, State#state.smtp_no_mx_lookups} |
                         State#state.smtp_relay_opts];
                    false ->
                        [{no_mx_lookups, State#state.smtp_no_mx_lookups},
                         {relay, ToDomain}]
                end,

            %% use the unique id as 'envelope sender' (VERP)                    
            case gen_smtp_client:send_blocking({Id, [ToEmail], EncodedMail},
                                      SmtpOpts) of
                {error, retries_exceeded, {temporary_failure, _LastRelay, _Bin}} ->
                    %% do nothing, it will retry later
                    ok;
                {error, no_more_hosts, _ErrorDetails} ->
                    %% delete email from the queue and notify the system
                    delete_emailq(Id),
                    ?DEBUG(_ErrorDetails),
                    z_notifier:first({email_failed, Id, Email#email.to}, Context);
                {error, Reason} ->
                    %% delete email from the queue and notify the system
                    delete_emailq(Id),
                    z_notifier:first({email_failed, Id, Email#email.to}, Context),
                    io:format("Invalid SMTP options: ~p\n", [Reason]);
                Recepit when is_binary(Recepit) ->
                    %% email accepted by relay
                    mark_sent(Id),
                    %% async send a copy for debugging if necessary
                    case State#state.smtp_bcc of
                            _BccTo when _BccTo =:= [] orelse _BccTo =:= undefined ->
                                ok;
                            BccTo ->
                                catch gen_smtp_client:send({Id, [BccTo], EncodedMail},
                                      SmtpOpts)
                    end,
                    %% check SpamAssassin spamscore
                    case {State#state.smtp_spamd_ip, State#state.smtp_spamd_port} of
                        {Addr, _Port} when Addr =:= [] orelse Addr =:= undefined ->
                           ok;
                        {Addr, Port} ->
                            SpamStatus = spamcheck(EncodedMail, Addr, Port),
                            z_notifier:first({email_spamstatus, Id, SpamStatus}, Context)
                    end
                            
            end
        end,
    spawn(F).

build_and_encode_mail(Headers, Text, Html, Context) ->
    ToEncode = esmtp_mime:create_multipart(),
    ToEncode1 = lists:foldl(fun(Header, Acc) -> esmtp_mime:add_header(Acc, Header) end, ToEncode, Headers),
    ToEncode2 = esmtp_mime:set_multipart_type(ToEncode1, alternative),

    ToEncode3 = case Text of
        [] ->
            case Html of
                [] ->
                    ToEncode2;
                _ ->
                    Markdown = z_markdown:to_markdown(Html, [no_html]),
                    esmtp_mime:add_part(ToEncode2, esmtp_mime:create_text_part(z_convert:to_list(Markdown)))
            end;
        _ -> 
            esmtp_mime:add_part(ToEncode2, esmtp_mime:create_text_part(z_convert:to_list(Text)))
        end,

    ToEncode4 = case Html of
        [] -> 
            ToEncode3;
        _ -> 
            {Parts, Html1} = z_email_embed:embed_images(z_convert:to_list(Html), Context),
            MimeHtml = esmtp_mime:set_multipart_type(esmtp_mime:create_multipart(), related),
            MimeHtml2 = esmtp_mime:add_part(MimeHtml, esmtp_mime:create_html_part(Html1)),
            MimeHtml3 = lists:foldr(fun(P, Msg) -> esmtp_mime:add_part(Msg, P) end, MimeHtml2, Parts),
            esmtp_mime:add_part(ToEncode3, MimeHtml3)
    end,
    %EncodedMail = mimemail:encode(ToEncode4),
    EncodedMail = esmtp_mime:encode(ToEncode4),
    EncodedMail.

spamcheck(EncodedMail, SpamDServer, SpamDPort) ->
    Email = binary_to_list(EncodedMail),
    
    {ok, Socket} = gen_tcp:connect(SpamDServer, SpamDPort, [list]),
    gen_tcp:send(Socket, "HEADERS SPAMC/1.2\r\n"),
    ContLen = integer_to_list(length(Email) + 2),
    gen_tcp:send(Socket, "Content-length: " ++ ContLen ++ "\r\n"),
    gen_tcp:send(Socket, "User: spamd\r\n"),
    gen_tcp:send(Socket, "\r\n"),
    gen_tcp:send(Socket, Email),
    gen_tcp:send(Socket, "\r\n"),
    
    Response = recv_spamd(Socket, []),
    gen_tcp:close(Socket),
    
    ParsedRes = parse_spamd_headers(Response),
    SpamStatus = proplists:get_value("X-Spam-Status", ParsedRes),
    IsSpam = case SpamStatus of
        "Yes, " ++ RestStatus -> true;
        "No, " ++ RestStatus -> false
    end,
    Results = [{is_spam, IsSpam} | [{list_to_atom(Field), Value} || [Field, Value] <- [string:tokens(Field, "=") || Field <- string:tokens(RestStatus, " ")]]],
    
    Results.

parse_spamd_headers(L) ->
    parse_spamd_headers(L, [], undefined).
parse_spamd_headers([], Acc, _) ->
    lists:reverse(Acc);
parse_spamd_headers(L, Acc, undefined) ->
    {FieldName, Rest} = parse_spamd_field_name(L, []),
    parse_spamd_headers(Rest, Acc, FieldName);
parse_spamd_headers(L, Acc, FieldName) ->
    {FieldValue, Rest} = parse_spamd_field_value(L, [], empty),
    parse_spamd_headers(Rest, [{FieldName, FieldValue} | Acc], undefined).


parse_spamd_field_name([], _) -> % ignore trailing characters
    {[], []};
parse_spamd_field_name([$: | Rest], Acc) ->
    {string:strip(lists:reverse(Acc)), Rest};
parse_spamd_field_name([C | Rest], Acc) ->
    parse_spamd_field_name(Rest, [C | Acc]).

parse_spamd_field_value([$\r | [$\n | Rest]], Acc, rn) -> % omit multiple \r\n-s
    parse_spamd_field_value(Rest, Acc, rn);
parse_spamd_field_value([$\r | Rest], Acc, empty) -> % put \r to the stack
    parse_spamd_field_value(Rest, Acc, r);
parse_spamd_field_value([$\n | Rest], Acc, r) -> % put \n to the stack
    parse_spamd_field_value(Rest, Acc, rn);
parse_spamd_field_value([$\t | Rest], Acc, rn) -> % read-ahead rule for \t
    parse_spamd_field_value(Rest, Acc, empty); % omit tabulator characters
parse_spamd_field_value([C | Rest], Acc, r) -> % read-ahead rule for non \n chars after \r
    parse_spamd_field_value(Rest, [C | [$\r | Acc]], empty);
parse_spamd_field_value([C | Rest], Acc, empty) ->
    parse_spamd_field_value(Rest, [C | Acc], empty);
parse_spamd_field_value(Rest, Acc, rn) -> % terminate
    {string:strip(lists:reverse(Acc)), Rest}.
    
recv_spamd(Socket, Res) ->
    receive
        {tcp, Socket, "SPAMD/1.1 0 EX_OK\r\n" ++ Data} ->
            recv_spamd(Socket, Res ++ Data);
        {tcp, Socket, Data} ->
            recv_spamd(Socket, Res ++ Data);
        {tcp_closed, Socket} ->
            Res
    after ?SPAMD_TIMEOUT ->
            io:format("spamassassin timeout~n"),
            Res
    end.

get_email_from(EmailFrom, Id, State, Context) ->
    From = case EmailFrom of 
        L when L =:= [] orelse L =:= undefined -> 
            get_email_from(Context); 
        _ -> EmailFrom
    end,
    case State#state.smtp_verp_as_from of
        true ->
            {FromName, _FromEmail} = z_email:split_name_email(From),
            string:strip(FromName ++ " " ++ Id);
        _ ->
            From
    end.

get_email_from(Context) ->
    %% Let the default be overruled by the config setting
    case m_site:get(email_from, Context) of
        undefined ->
            %% Make the default no-reply e-mail address for the main site url.            
            [Hostname|_] = string:tokens(z_convert:to_list(m_site:get(hostname, Context)), ":"),            
            "noreply@" ++ Hostname;
        EmailFrom_ ->
            EmailFrom_
    end.
    
check_override(EmailAddr, _) when EmailAddr == undefined; EmailAddr == []; EmailAddr == <<>> ->
    undefined;
check_override(EmailAddr, #state{override=Override}) when Override == undefined; Override == []; Override == <<>> ->
    z_convert:to_list(EmailAddr);
check_override(EmailAddr, State) ->
    escape_email(z_convert:to_list(EmailAddr)) ++ " (override) <" ++ State#state.override ++ ">".

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
    

%% @doc Mark email as sent by adding the 'sent' timestamp. 
%%      This will schedule it for deletion as well.
mark_sent(Id) ->
    Tr = fun() ->
                 [QEmail] = mnesia:read(email_queue, Id),
                 SentTS = now(),
                 mnesia:write(QEmail#email_queue{sent=SentTS})
         end,
    {atomic, SentTimestamp} = mnesia:transaction(Tr),
    SentTimestamp.

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
    DelTransFun = fun() -> 
                          DelQuery = qlc:q([QEmail || QEmail <- mnesia:table(email_queue),
                                                      QEmail#email_queue.sent /= undefined andalso
                                                        timer:now_diff(
                                                            inc_timestamp(QEmail#email_queue.sent, ?DELETE_AFTER),
                                                            now()) < 0
                                            ]),
                          DelQueryRes = qlc:e(DelQuery),
                          [ begin
                                mnesia:delete_object(QEmail),
                                {QEmail#email_queue.id,
                                 (QEmail#email_queue.email)#email.to,
                                 QEmail#email_queue.context}
                            end || QEmail <- DelQueryRes ]
                  end,
    {atomic, NotifyList1} = mnesia:transaction(DelTransFun),
    %% notify the system that these emails were sucessfuly sent and (probably) received
    [ z_notifier:first({email_sent, Id, Recipient}, z_context:depickle(PickledContext)) 
     || {Id, Recipient, PickledContext} <- NotifyList1 ],

    %% delete all messages with too high retry count
    SetFailTransFun = fun() ->
                              PollQuery = qlc:q([QEmail || QEmail <- mnesia:table(email_queue),
                                                 QEmail#email_queue.sent == undefined,
                                                 QEmail#email_queue.retry > ?MAX_RETRY]),
                              PollQueryRes = qlc:e(PollQuery),
                              [ begin
                                    mnesia:delete_object(QEmail),
                                    {QEmail#email_queue.id,
                                     (QEmail#email_queue.email)#email.to,
                                     QEmail#email_queue.context}
                                end || QEmail <- PollQueryRes ]
                      end,
    {atomic, NotifyList2} = mnesia:transaction(SetFailTransFun),
    %% notify the system that these emails were failed to be sent
    [ z_notifier:first({email_failed, Id, Recipient}, z_context:depickle(PickledContext)) 
     || {Id, Recipient, PickledContext} <- NotifyList2 ],
 
    %% fetch a batch of messages for sending
    FetchTransFun =
        fun() ->
                Q = qlc:q([QEmail || QEmail <- mnesia:table(email_queue),
                           QEmail#email_queue.sent == undefined,
                           timer:now_diff(QEmail#email_queue.retry_on, now()) < 0]),
                qlc:e(Q)
        end,
    {atomic, Ms} = mnesia:transaction(FetchTransFun),
    %% send the fetched messages
    case Ms of
        [] ->
            State;
        _  ->
            State1 = update_config(State),
            [ begin
                  update_retry(QEmail),
                  spawn_send(QEmail#email_queue.id, 
                             QEmail#email_queue.email,
                             z_context:depickle(QEmail#email_queue.context), 
                             State1)
              end || QEmail <- Ms ],
            State1
    end.


%% @doc Sets the next retry time for an e-mail.
update_retry(QEmail=#email_queue{retry=Retry}) ->
    Period = period(Retry),
    Tr = fun()->
                 mnesia:write(QEmail#email_queue{retry=Retry+1,
                                                 retry_on=inc_timestamp(now(), Period)})
         end,
    mnesia:transaction(Tr).

period(0) -> 10;
period(1) -> 60;
period(2) -> 12 * 60;
period(3) -> 24 * 60;
period(4) -> 48 * 60;
period(5) -> 72 * 60;
period(_) -> 7 * 24 * 60.       % Retry every week for extreme cases
    

%% @doc Increases a timestamp (as returned by now/0) with a value provided in minutes
inc_timestamp({MegaSec, Sec, MicroSec}, MinToAdd) ->
    Sec2 = Sec + MinToAdd, %%!!! * 60,
    Sec3 = Sec2 rem 1000000,
    MegaSec2 = MegaSec + Sec2 div 1000000,
    {MegaSec2, Sec3, MicroSec}.

