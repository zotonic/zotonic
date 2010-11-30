%% @author Marc Worrell <marc@worrell.nl>
%% @author Atilla Erdodi <atilla@maximonster.com>
%% @copyright 2010 Maximonster Interactive Things
%% @date 2010-11-25
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

-record(state, {host, ssl, port, username, password, override}).
-record(email_queue, {id, retry_on=inc_timestamp(now(), 10),
                      retry=0, email, created=now(), sent, context}).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link(Args) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link() ->
    start_link([]).
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
%% Description: Handling call messages
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
    check_if_msgid_is_vaild,
    TrFun = fun()-> 
                    [QEmail] = mnesia:read(email_queue, MsgId), 
                    mnesia:delete_object(QEmail),
                    {(QEmail#email_queue.email)#email.to, QEmail#email_queue.context}
            end,
    {atomic, {Recipient, PickledContext}} = mnesia:transaction(TrFun),
    Context = z_context:depickle(PickledContext),
    z_notifier:first({email_failed, MsgId, Recipient}, Context), 
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
    {noreply, State1};

%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(R    eason, State) -> void()
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
    State#state{
        
        host     = z_config:get(smtp_host),
        port     = z_config:get(smtp_port),
        ssl      = z_config:get(smtp_ssl),
        
        username = z_config:get(smtp_username),
        password = z_config:get(smtp_password),

        override = z_config:get(email_override)

               }.

generate_message_id(Context) ->
    FQDN = smtp_util:guess_FQDN(),
    [Hostname|_] = string:tokens(z_convert:to_list(m_site:get(hostname, Context)), ":"),
    Md5 = [io_lib:format("~2.16.0b", [X]) || <<X>> <= erlang:md5(term_to_binary([erlang:now(), FQDN]))],
    lists:flatten(io_lib:format("<noreply+~s@~s>", [Md5, Hostname])).

%% =========================
%% SENDING related functions
%% =========================
spawn_send(Id, Email, Context, State) ->
    F = fun() ->
                process_flag(trap_exit, true),    
    
                To = case State#state.override of 
                         O when O =:= [] orelse O =:= undefined -> Email#email.to; 
                         Override -> z_convert:to_list(Email#email.to) ++ " (override) <" ++ Override ++ ">"
                     end,

                    
		From = case Email#email.from of 
                           L when L =:= [] orelse L =:= undefined -> 
                               get_email_from(Context); 
                           EmailFrom -> EmailFrom end,

		% Optionally render the text and html body
                Vars = [{email_to, To}, {email_from, From} | Email#email.vars],
	        Text = optional_render(Email#email.text, Email#email.text_tpl, Vars, Context),
		Html = optional_render(Email#email.html, Email#email.html_tpl, Vars, Context),

                % Fetch the subject from the title of the HTML part or from the Email record
		Subject = case {Html, Email#email.subject} of
                              {[], undefined} -> [];
                              {[], Sub} -> Sub;
                              {_Html, undefined} ->
                                  {match, [_, {Start,Len}|_]} = re:run(Html, "<title>(.*)</title>", [dotall, caseless]),
                                  string:strip(z_string:line(lists:sublist(Html, Start+1, Len)))
                          end,
                
                Text1 = {<<"text">>,<<"plain">>,
                         [{<<"Content-Type">>,
                           <<"text/plain;charset=utf-8;format=flowed">>},
                          {<<"Content-Transfer-Encoding">>,<<"quoted-printable">>}],
                         [{<<"content-type-params">>,
                           [{<<"charset">>,<<"utf-8">>},
                            {<<"format">>,<<"flowed">>}]},
                          {<<"disposition">>,<<"inline">>},
                          {<<"disposition-params">>,[]}],
                         list_to_binary(Text)},
                    
                Html1 = {<<"text">>,<<"html">>,
                         [{<<"Content-Type">>,<<"text/html;charset=utf-8">>},
                          {<<"Content-Transfer-Encoding">>,<<"quoted-printable">>}],
                         [{<<"content-type-params">>,
                           [{<<"charset">>,<<"utf-8">>}]},
                          {<<"disposition">>,<<"inline">>},
                          {<<"disposition-params">>,[]}],
                         list_to_binary(Html)},
                
                optional_embed_images,
                
                ToEncode = {<<"multipart">>, <<"related">>,
                            [{<<"From">>, From},
                             {<<"To">>, To},
                             {<<"Subject">>, Subject},
                             {<<"MIME-Version">>, <<"1.0">>},
                             {<<"Message-ID">>, Id},
                             {<<"X-Mailer">>, "Zotonic " ++ ?ZOTONIC_VERSION ++ " (http://zotonic.com)"}],
                            [],                       
                            [{<<"multipart">>,<<"alternative">>,
                              [], [],
                              [Text1, Html1]}]
                           },
                
                %io:format("original:\n~p\n", [ToEncode]),
                
                EncodedMail = mimemail:encode(ToEncode),                                

                To1 = string:strip(z_string:line(binary_to_list(z_convert:to_binary(To)))),
                {_ToName, ToEmail} = z_email:split_name_email(To1),

                [_ToUsername, ToDomain] = string:tokens(ToEmail, "@"),

                SmtpOpts = 
                    case z_config:get(smtp_relay) of 
                        true ->
                            [{relay, State#state.host},
                             {port, State#state.port},
                             {ssl, State#state.ssl}] 
                            ++ case {State#state.username,
                                     State#state.password} of
                                   {undefined, undefined} ->
                                       [];
                                   {User, Pass} ->
                                       [{auth, always},
                                        {username, User},
                                        {password, Pass}]
                               end;
                        false ->
                            [{relay, ToDomain}]
                    end,                

                %% use the unique id as 'envelope sender' (VERP)                    
                case gen_smtp_client:send({Id, [ToEmail], EncodedMail},
                                          SmtpOpts) of
                    {ok, Pid} ->
                        receive
                            {'EXIT', Pid, normal} ->
                                SentTimestamp = mark_sent(Id),
                                z_notifier:first({email_accepted, Id,
                                                Email#email.to,
                                                SentTimestamp}, Context);
                            {'EXIT', Pid, {temporary_failure, FailReason}} ->
                                %% do nothing, it will retry later
                                io:format("Temporary failure: ~p\n", [FailReason]);
                            {'EXIT', Pid, Error} ->
                                %% delete email from the queue and notify the system
                                delete_emailq(Id),
                                z_notifier:first({email_failed, Id, Email#email.to}, Context),
                                io:format("Permanent failure: ~p\n", [Error])
                        end;
                    {error, Reason} ->
                        %% delete email from the queue and notify the system
                        delete_emailq(Id),
                        z_notifier:first({email_failed, Id, Email#email.to}, Context),
                        io:format("Invalid SMTP options: ~p\n", [Reason])
                end
                      
        end,
    spawn(F).

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
    % Delete sent messages
    DelTransFun = fun() -> 
                          DelQuery = qlc:q([QEmail || QEmail <- mnesia:table(email_queue),
                                            timer:now_diff(
                                                inc_timestamp(QEmail#email_queue.sent, ?DELETE_AFTER),
                                                now()) < 0]),
                          DelQueryRes = qlc:e(DelQuery),
                          [mnesia:delete_object(QEmail)|| QEmail <- DelQueryRes]
                  end,
    mnesia:transaction(DelTransFun),    

    % Delete all messages with too high retry count
    SetFailTransFun = fun() ->
                              PollQuery = qlc:q([QEmail || QEmail <- mnesia:table(email_queue),
                                                 QEmail#email_queue.sent == undefined,
                                                 QEmail#email_queue.retry > ?MAX_RETRY]),
                              PollQueryRes = qlc:e(PollQuery),
                              [begin
                                   mnesia:delete_object(QEmail),
                                   {QEmail#email_queue.id,
                                    (QEmail#email_queue.email)#email.to,
                                    QEmail#email_queue.context}
                               end || QEmail <- PollQueryRes]
                      end,
    {atomic, NotifyList} = mnesia:transaction(SetFailTransFun),
    [z_notifier:first({failed_email, Id, Recipient}, z_context:depickle(PickledContext)) 
     || {Id, Recipient, PickledContext} <- NotifyList],
 
    % Fetch a batch of messages for sending
    FetchTransFun =
        fun() ->
                Q = qlc:q([QEmail || QEmail <- mnesia:table(email_queue),
                           QEmail#email_queue.sent == undefined,
                           timer:now_diff(QEmail#email_queue.retry_on, now()) < 0]),
                qlc:e(Q)
        end,
    {atomic, Ms} = mnesia:transaction(FetchTransFun),
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

