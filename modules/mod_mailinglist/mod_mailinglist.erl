%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-11-23
%% @doc Mailinglist implementation. Enables to send pages to a list of recipients.

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

-module(mod_mailinglist).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

-mod_title("Mailing list").
-mod_description("Mailing lists. Send a page to a list of recipients.").
-mod_prio(600).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
	observe_search_query/2,
	observe_mailinglist_message/2,
	event/2,
	update_scheduled_list/2
]).

-include_lib("zotonic.hrl").

-record(state, {context}).


observe_search_query({search_query, {mailinglist_recipients, [{id,Id}]}, _OffsetLimit}, _Context) ->
    #search_sql{
        select="id, email, is_enabled",
        from="mailinglist_recipient",
		where="mailinglist_id = $1",
		args=[Id],
        order="email",
        tables=[]
    };
observe_search_query(_, _) ->
	undefined.


%% @doc Send status messages to a recipient.
observe_mailinglist_message({mailinglist_message, silent, _ListId, _Email}, _Context) ->
	ok;
observe_mailinglist_message({mailinglist_message, send_goodbye, ListId, Email}, Context) ->
	z_email:send_render(Email, "email_mailinglist_goodbye.tpl", [{list_id, ListId}, {email, Email}], Context),
	ok;
observe_mailinglist_message({mailinglist_message, Message, ListId, RecipientId}, Context) ->
	Template = case Message of
		send_welcome -> "email_mailinglist_welcome.tpl";
		send_confirm -> "email_mailinglist_confirm.tpl"
	end,
	Props = m_mailinglist:recipient_get(RecipientId, Context),
	z_email:send_render(proplists:get_value(email, Props), Template, [{list_id, ListId}, {recipient, Props}], Context),
	ok.
	

%% @doc Request confirmation of canceling this mailing.
event({postback, {dialog_mailing_cancel_confirm, Args}, _TriggerId, _TargetId}, Context) ->
	MailingId = proplists:get_value(mailinglist_id, Args),
	case z_acl:rsc_editable(MailingId, Context) of
		true ->
			z_render:dialog("Confirm mailing cancelation.", "_dialog_mailing_cancel_confirm.tpl", Args, Context);
		false ->
			z_render:growl_error("You are not allowed to cancel this mailing.", Context)
	end;
event({postback, {mailing_cancel, Args}, _TriggerId, _TargetId}, Context) ->
	MailingId = proplists:get_value(mailinglist_id, Args),
	PageId = proplists:get_value(page_id, Args),
	case z_acl:rsc_editable(MailingId, Context) of
		true ->
			m_mailinglist:delete_scheduled(MailingId, PageId, Context),
			Context1 = update_scheduled_list(PageId, Context),
			z_render:growl("The mailing has been canceled.", Context1);
		false ->
			z_render:growl_error("You are not allowed to cancel this mailing.", Context)
	end;

%% @doc Handle upload of a new recipients list
event({submit, {mailinglist_upload,[{id,Id}]}, _TriggerId, _TargetId}, Context) ->
	{upload, _OriginalFilename, TmpFile} = z_context:get_q_validated("file", Context),
    case import_file(TmpFile, Id, Context) of
        ok ->
            z_render:wire([{dialog_close, []}, {reload, []}], Context);
        {error, Msg} ->
            z_render:growl(Msg, "error", true, Context)
    end.

%% @doc Update the list with the mailings scheduled for this page.
update_scheduled_list(Id, Context) ->
	Vars = [
		{id, Id},
		{scheduled, m_mailinglist:get_scheduled(Id, Context)}
	],
	Html = z_template:render("_mailinglist_scheduled.tpl", Vars, Context),
	z_render:update("mailinglist-scheduled", Html, Context).


	

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
    m_mailinglist:install(Context),
    z_notifier:observe(mailinglist_mailing, self(), Context),
    z_notifier:observe(dropbox_file, self(), 100, Context),
    timer:send_interval(180000, poll),
    {ok, #state{context=z_context:new(Context)}}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @doc Handle a dropbox file with recipients.
handle_call({{dropbox_file, File}, _SenderContext}, _From, State) ->
	GetFiles = fun() ->
		C = z_acl:sudo(State#state.context),
		#search_result{result=Ids} = z_search:search({all, [{cat,mailinglist}]}, C),
		[ {m_rsc:p(Id, mailinglist_dropbox_filename, C), Id} || Id <- Ids ]
	end,
	Files = z_depcache:memo(GetFiles, mailinglist_dropbox_filenames, ?WEEK, [mailinglist], State#state.context),
	case proplists:get_value(list_to_binary(filename:basename(File)), Files) of
		undefined ->
			{reply, undefined, State};
		ListId ->
			HandleF = fun() ->
				C = z_acl:sudo(State#state.context),
                case import_file(File, ListId, C) of
                    ok ->
        				z_email:send_admin(
                          "mod_mailinglist: Import from dropbox",
                          ["Replaced all recipients of ", m_rsc:p(ListId, title, C), " with the contents of ", File, "."], State#state.context);
                    {error, Msg} ->
        				z_email:send_admin("mod_mailinglist: Import from dropbox FAILED", Msg, State#state.context)
                end
			end,
			spawn(HandleF),
			{reply, ok, State}
	end;

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Send a mailing.
handle_cast({{mailinglist_mailing, ListId, PageId}, SenderContext}, State) ->
	send_mailing(ListId, PageId, SenderContext),
	{noreply, State};
	
%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @doc Poll the database for scheduled mailings.
handle_info(poll, State) ->
    poll_scheduled(z_acl:sudo(State#state.context)),
    {noreply, State};

%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    z_notifier:detach(mailinglist_mailing, self(), State#state.context),
    z_notifier:detach(dropbox_file, self(), State#state.context),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================


%% @doc Import a file, replacing the recipients of the list.
import_file(TmpFile, Id, Context) ->
	{ok, Data} = file:read_file(TmpFile),
	file:delete(TmpFile),
    try
        ok = m_mailinglist:replace_recipients(Id, Data, Context)
    catch
        _: {badmatch, {rollback, {{case_clause, {error, {error, error, <<"22021">>, _, _}}},_}}}->
            {error, "The encoding of the input file is not right. Please upload a file with UTF-8 encoding."};
        _: _ ->
            {error, "Something unexpected went wrong while importing the recipients list."}
    end.



%% @doc Check if there are any scheduled mailings waiting.
poll_scheduled(Context) ->
	case m_mailinglist:check_scheduled(Context) of
		{ListId, PageId} ->
			m_mailinglist:delete_scheduled(ListId, PageId, Context),
			send_mailing(ListId, PageId, Context);
		undefined ->
			ok
	end.
	

%% @doc Send the page to the mailinglist. The first 20 e-mails are send directly, which is useful for
%% short lists like test e-mail lists. All other e-mails are queued and send later.  When the page is
%% not yet visible then the mailing is queued.  The test mailinglist is always send directly.
send_mailing(ListId, PageId, Context) ->
	case m_rsc:p(ListId, name, Context) of
		<<"mailinglist_test">> ->
			spawn(fun() -> send_mailing_process(ListId, PageId, Context) end);
		_ ->
			AnonymousContext = z_acl:anondo(Context),
			case z_acl:rsc_visible(PageId, AnonymousContext) of
				true -> spawn(fun() -> send_mailing_process(ListId, PageId, AnonymousContext) end);
				false -> m_mailinglist:insert_scheduled(ListId, PageId, Context)
			end
	end.

send_mailing_process(ListId, PageId, Context) ->
	Recipients = m_mailinglist:get_enabled_recipients(ListId, Context),
	{Direct,Queued} = split_list(20, Recipients),
	FromEmail = case m_rsc:p(ListId, mailinglist_reply_to, Context) of
        	    Empty when Empty =:= undefined; Empty =:= <<>> ->
        	        z_convert:to_list(m_config:get_value(mod_emailer, email_from, Context));
        	    RT ->
        	        z_convert:to_list(RT)
    	     end,
    FromName = case m_rsc:p(ListId, mailinglist_sender_name, Context) of
                  undefined -> [];
                  <<>> -> []; 
                  SenderName -> z_convert:to_list(SenderName)
               end,
    From = z_email:combine_name_email(FromName, FromEmail),
    Options = [
        {id,PageId}, {list_id, ListId}, {email_from, From}
    ],
    
	[
		z_email:send_render(Email, {cat, "mailing_page.tpl"}, [{email,Email}|Options], Context)
		|| Email <- Direct
	],
	[
		z_email:sendq_render(Email, {cat, "mailing_page.tpl"}, [{email,Email}|Options], Context)
		|| Email <- Queued
	],
	ok.

	split_list(N, List) ->
		split_list(N, List, []).

		split_list(0, List, Acc) ->
			{Acc, List};
		split_list(_N, [], Acc) ->
			{Acc, []};
		split_list(N, [H|T], Acc) ->
			split_list(N-1, T, [H|Acc]).

