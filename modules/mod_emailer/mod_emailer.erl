%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-05-02
%% @doc Email server.  Queues, renders and sends e-mails.

%% Copyright 2009 Marc Worrell
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

-module(mod_emailer).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

-mod_title("e-Mail sending").
-mod_description("Provided sending of e-mails. e-Mails are queued before sending.").
-mod_prio(1000).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
]).

-include_lib("zotonic.hrl").
-include_lib("esmtp/include/esmtp_mime.hrl").

%% -define(SMTP_PORT_TLS, 587).
%% -define(SMTP_PORT_SSL, 465).

% Maximum times we retry to send a message before we mark it as failed.
-define(MAX_RETRY, 7).

-record(state, {sendmail, from, ehlo, host, ssl, port, username, password, override, context}).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link(Args) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    z_notifier:observe(#email{}, self(), Context),
    timer:send_interval(5000, poll),
    State = update_config(#state{context=z_context:new(Context)}),
    {ok, State}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Send an e-mail to an e-mail address.
handle_cast({#email{} = Email, Context}, State) ->
	State1 = update_config(State),
    Cols = [
        {recipient, Email#email.to},
		{sender, Email#email.from},
        {email, Email},
        {context, z_context:pickle(Context)},
        {retry, 0}
    ],
    {ok, Id} = z_db:insert(emailq, Cols, Context),
	case Email#email.queue of
		false -> send_queued([{id, Id}|Cols], State1);
		true -> ok
	end,
    {noreply, State1};


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

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    z_notifier:detach(#email{}, self(), State#state.context),
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
	%% Make the default no-reply e-mail address for the main site url.
	[EmailFrom|_] = string:tokens("no-reply@" ++ z_convert:to_list(m_site:get(hostname, State#state.context)), ":"),

	%% Let the defaults be overruled by the config settings (from the admin and site config)
    State#state{
		sendmail = z_convert:to_list(m_config:get_value(?MODULE, sendmail, State#state.context)),
        from     = z_convert:to_list(m_config:get_value(?MODULE, email_from, EmailFrom, State#state.context)),
        host     = z_convert:to_list(m_config:get_value(?MODULE, smtp_host, "localhost", State#state.context)),
        port     = z_convert:to_integer(m_config:get_value(?MODULE, smtp_port, 25, State#state.context)),
        ssl      = z_convert:to_bool(m_config:get_value(?MODULE, smtp_ssl, false, State#state.context)),
        ehlo     = z_convert:to_list(m_config:get_value(?MODULE, smtp_ehlo, "localhost", State#state.context)),
        username = z_convert:to_list(m_config:get_value(?MODULE, smtp_username, State#state.context)),
        password = z_convert:to_list(m_config:get_value(?MODULE, smtp_password, State#state.context)),
        override = z_convert:to_list(m_config:get_value(?MODULE, email_override, State#state.context))
    }.


%% @doc Fetch a new batch of queued e-mails. Set status of failed messages.
poll_queued(State) ->
    % Set all messages with too high retry count to 'failed'
    z_db:q("update emailq set status = 'fail' where status = 'new' and retry > $1", [?MAX_RETRY], State#state.context),
    % Fetch a batch of messages for sending
    Ms = z_db:assoc_props("select * from emailq where status = 'new' and retry_on < now() order by retry_on asc limit 100", State#state.context),
	case Ms of
		[] -> 
			State;
		_  ->
			State1 = update_config(State), 
    		[ send_queued(M, State) || M <- Ms ],
			State1
	end.


send_queued(Cols, State) ->
    {id, Id} = proplists:lookup(id, Cols),
	{email, Email} = proplists:lookup(email, Cols),
    {context, PickledContext} = proplists:lookup(context, Cols),
    Context = z_context:depickle(PickledContext),
    {retry, Retry} = proplists:lookup(retry, Cols),
    mark_retry(Id, Retry, Context),
	spawn_send(Id, Email, Context, State).


spawn_send(Id, Email, Context, State) ->
    F = fun() ->
	    To = case State#state.override of 
                 O when O =:= [] orelse O =:= undefined -> Email#email.to; 
                 Override -> z_convert:to_list(Email#email.to) ++ " (override) <" ++ Override ++ ">"
             end,
		From = case Email#email.from of L when L =:= [] orelse L =:= undefined -> State#state.from; EmailFrom -> EmailFrom end,

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

        % Build the message and send it
        MimeMsg = esmtp_mime:msg(z_convert:to_list(To), z_convert:to_list(From), z_convert:to_list(Subject)),
        MimeMsg1 = esmtp_mime:add_header(MimeMsg, {"X-Mailer", "Zotonic " ?ZOTONIC_VERSION " (http://zotonic.com)"}),
        MimeMsg2 = esmtp_mime:set_multipart_type(MimeMsg1, related),
        MimeTxt = esmtp_mime:create_multipart(),
                
        MimeTxt1 = case Text of
            [] -> MimeTxt;
            _ -> esmtp_mime:add_part(MimeTxt, esmtp_mime:create_text_part(z_convert:to_list(Text)))
        end,
        MimeBase = case Html of
            [] -> esmtp_mime:add_part(MimeMsg2, MimeTxt1);
            _ -> 
                 {Parts, Html1} = mod_emailer_embed:embed_images(z_convert:to_list(Html), Context),
                 MimeTxt2 = esmtp_mime:add_part(MimeTxt1, esmtp_mime:create_html_part(Html1)),
                 MimeMsg3 = esmtp_mime:add_part(MimeMsg2, MimeTxt2),
                 lists:foldr(fun(P, Msg) -> esmtp_mime:add_part(Msg, P) end, MimeMsg3, Parts)                          
        end,
        sendemail(MimeBase, State),
        mark_sent(Id, Context)
    end,
    spawn(F).


optional_render(undefined, undefined, _Vars, _Context) ->
	[];
optional_render(Text, undefined, _Vars, _Context) ->
	Text;
optional_render(undefined, Template, Vars, Context) ->
    {Output, _Context} = z_template:render_to_iolist(Template, Vars, Context),
    binary_to_list(iolist_to_binary(Output)).


mark_sent(Id, Context) ->
    z_db:q("update emailq set status = 'sent', sent = now() where id = $1", [Id], Context).

mark_retry(Id, Retry, Context) ->
    Period = period(Retry),
    z_db:q("update emailq set retry = retry+1, retry_on = now() + interval '"++integer_to_list(Period)++" minutes' where id = $1", [Id], Context).
    
    period(0) -> 10;
    period(1) -> 60;
    period(2) -> 12 * 60;
    period(3) -> 24 * 60;
    period(4) -> 48 * 60;
    period(5) -> 72 * 60;
    period(_) -> 7 * 24 * 60.       % Retry every week for extreme cases
    


%% Send the e-mail with sendmail
sendemail(Msg = #mime_multipart{}, #state{sendmail=Sendmail}) when Sendmail /= [] ->
	Message = esmtp_mime:encode(Msg),
	{_FromName, FromEmail} = z_email:split_name_email(esmtp_mime:from(Msg)),
	SendmailFrom = [ Sendmail, " -f", z_utils:os_escape(FromEmail), " ", z_utils:os_escape(esmtp_mime:to(Msg)) ],
	SendmailFromAsList = binary_to_list(iolist_to_binary(SendmailFrom)),
	P = open_port({spawn, SendmailFromAsList}, [use_stdio]),
	port_command(P, Message),
	port_close(P);

%% Send the e-mail with smtp
sendemail(Msg = #mime_multipart{}, State) ->
    Login = case State#state.username of
        undefined -> no_login;
		[] -> no_login;
        _ -> {State#state.username, State#state.password}
    end,
    MX = {
        State#state.host,
        State#state.port,
        State#state.ssl,
        Login
    },
    Ehlo = State#state.ehlo,
    sendemail(MX, Ehlo, esmtp_mime:from(Msg), esmtp_mime:to(Msg), esmtp_mime:encode(Msg)).
    
sendemail({Host,Port,SSL,Login}, Ehlo, From, To, Msg) ->
    To1 = string:strip(z_string:line(binary_to_list(z_convert:to_binary(To)))),
    {_ToName, ToEmail} = z_email:split_name_email(To1),
    {_FromName, FromEmail} = z_email:split_name_email(From),
    {ok, Fsm} = esmtp_fsm:start_link(Host, Port, SSL),
    {ok, _} = esmtp_fsm:ehlo(Fsm, Ehlo),
    case Login of
        {User,Pass} -> {ok, _} = esmtp_fsm:login(Fsm,User,Pass);
        no_login -> ok
    end,
    {ok, _} = esmtp_fsm:mail_from(Fsm,FromEmail),
    {ok, _} = esmtp_fsm:rcpt_to(Fsm,ToEmail),
    {ok, _} = esmtp_fsm:message(Fsm,Msg),
    ok = esmtp_fsm:close(Fsm),
    ok.

