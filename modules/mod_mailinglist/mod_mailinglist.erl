%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-11-23
%% @doc Mailinglist implementation. Enables to send pages to a list of recipients.

%% Copyright 2009-2011 Marc Worrell
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
	observe_email_bounced/2,
	event/2,
	datamodel/1,
	page_attachments/2
]).

-include_lib("zotonic.hrl").

-record(state, {context}).


%% @doc Install the tables needed for the mailinglist and return the rsc datamodel.
datamodel(Context) ->
    m_mailinglist:init_tables(Context),
    [
        {categories, [
            {mailinglist, undefined, [
                            {title, "Mailing list"},
                            {summary, "Mailing lists are used to send pages to groups of people."}
                        ]}
        ]},

        % Any resource with an e-mail address can be a subscriber of a mailinglist
        {predicates, [
            {subscriberof,
                 [{title, <<"Subscriber of">>}],
                 [{person, mailinglist}, {location, mailinglist}]},
             {exsubscriberof,
                  [{title, <<"Ex-subscriber of">>}],
                  [{person, mailinglist}, {location, mailinglist}]}
        ]},

        {resources, [
            {mailinglist_test, mailinglist, [
                            {visible_for, 1},
                            {title, "Test mailing list"},
                            {summary, "This list is used for testing. Anyone who can see this mailing list can post to it. It should not be visible for the world."}
                        ]}
        ]}
    ].


observe_search_query({search_query, {mailinglist_recipients, [{id,Id}]}, _OffsetLimit}, _Context) ->
    #search_sql{
        select="id, email, is_enabled",
        from="mailinglist_recipient",
		where="mailinglist_id = $1",
		args=[Id],
        order="email",
        tables=[]
    };
observe_search_query({search_query, {mailinglist_recipients, [{id,Id}, {is_bounced, Bounced}]}, _OffsetLimit}, _Context) ->
    #search_sql{
        select="id, email, is_enabled",
        from="mailinglist_recipient",
		where="mailinglist_id = $1 AND is_bounced = $2",
		args=[Id, Bounced],
        order="email",
        tables=[]
    };
observe_search_query(_, _) ->
	undefined.


%% @doc Send status messages to a recipient.
observe_mailinglist_message(#mailinglist_message{what=silent}, _Context) ->
	ok;
observe_mailinglist_message(#mailinglist_message{what=send_goodbye, list_id=ListId, recipient=Email}, Context) ->
	z_email:send_render(Email, "email_mailinglist_goodbye.tpl", [{list_id, ListId}, {email, Email}], Context),
	ok;
observe_mailinglist_message(#mailinglist_message{what=Message, list_id=ListId, recipient=RecipientId}, Context) ->
	Template = case Message of
		send_welcome -> "email_mailinglist_welcome.tpl";
		send_confirm -> "email_mailinglist_confirm.tpl"
	end,
	Props = m_mailinglist:recipient_get(RecipientId, Context),
	z_email:send_render(proplists:get_value(email, Props), Template, [{list_id, ListId}, {recipient, Props}], Context),
	ok.


%% @doc When an e-mail bounces, disable the corresponding recipients and mark them as bounced.
observe_email_bounced(B=#email_bounced{}, Context) ->
    Recipients = m_mailinglist:get_recipients_by_email(B#email_bounced.recipient, Context),
    lists:foreach(fun(Id) -> m_mailinglist:update_recipient(Id, [{is_enabled, false}, {is_bounced, true}, {bounce_time, calendar:local_time()}], Context) end, Recipients),
    undefined. %% Let other bounce handlers do their thing


%% @doc Request confirmation of canceling this mailing.
event({postback, {dialog_mailing_cancel_confirm, Args}, _TriggerId, _TargetId}, Context) ->
	MailingId = proplists:get_value(list_id, Args),
	case z_acl:rsc_editable(MailingId, Context) of
		true ->
			z_render:dialog("Confirm mailing cancelation.", "_dialog_mailing_cancel_confirm.tpl", Args, Context);
		false ->
			z_render:growl_error("You are not allowed to cancel this mailing.", Context)
	end;
event({postback, {mailing_cancel, Args}, _TriggerId, _TargetId}, Context) ->
	MailingId = proplists:get_value(list_id, Args),
	PageId = proplists:get_value(page_id, Args),
	case z_acl:rsc_editable(MailingId, Context) of
		true ->
			m_mailinglist:delete_scheduled(MailingId, PageId, Context),
            mod_signal:emit({update_mailinglist_scheduled, [{id, PageId}]}, Context),
			z_render:growl("The mailing has been canceled.", Context);
		false ->
			z_render:growl_error("You are not allowed to cancel this mailing.", Context)
	end;
event({postback, {mailinglist_reset, Args}, _TriggerId, _TargetId}, Context) ->
	MailingId = proplists:get_value(list_id, Args),
	PageId = proplists:get_value(page_id, Args),
	case z_acl:rsc_editable(MailingId, Context) of
		true ->
			m_mailinglist:reset_log_email(MailingId, PageId, Context),
            mod_signal:emit({update_mailinglist_scheduled, [{id, PageId}]}, Context),
			z_render:growl("The statistics have been cleared.", Context);
		false ->
			z_render:growl_error("You are not allowed to reset this mailing.", Context)
	end;

%% @doc Handle upload of a new recipients list
event({submit, {mailinglist_upload,[{id,Id}]}, _TriggerId, _TargetId}, Context) ->
    #upload{tmpfile=TmpFile} = z_context:get_q_validated("file", Context),
    case import_file(TmpFile, Id, Context) of
        ok ->
            z_render:wire([{dialog_close, []}, {reload, []}], Context);
        {error, Msg} ->
            z_render:growl(Msg, "error", true, Context)
    end;

%% @doc Handle the test-sending of a page to a single address.
event({submit, {mailing_testaddress, [{id, PageId}]}, _, _}, Context) ->
    Email = z_context:get_q_validated("email", Context),
    z_notifier:notify(#mailinglist_mailing{list_id={single_test_address, Email}, page_id=PageId}, Context),
    Context1 = z_render:growl(?__("Sending the page to ", Context) ++ Email ++ "...", Context),
    z_render:wire([{dialog_close, []}], Context1);


%% @doc Handle the test-sending of a page to a single address.
event({postback, {resend_bounced, [{list_id, ListId}, {id, PageId}]}, _, _}, Context) ->
    z_notifier:notify(#mailinglist_mailing{list_id={resend_bounced, ListId}, page_id=PageId}, Context),
    case length(m_mailinglist:get_bounced_recipients(ListId, Context)) of
        0 ->
            z_render:growl_error(?__("No addresses selected", Context), Context);
        _ ->
            Context1 = z_render:growl(?__("Resending bounced addresses...", Context), Context),
            z_render:wire([{dialog_close, []}], Context1)
    end.



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
handle_cast({#mailinglist_mailing{list_id=ListId, page_id=PageId}, SenderContext}, State) ->
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
    z_utils:flush_message(poll),    
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
%% short lists like test e-mail lists. All other e-mails are queued and sent later.
send_mailing(ListId, PageId, Context) ->
    spawn(fun() -> send_mailing_process(ListId, PageId, z_acl:sudo(Context)) end).


send_mailing_process({single_test_address, Email}, PageId, Context) ->
    {ok, ListId} = m_rsc:name_to_id(mailinglist_test, Context),
    send_mailing_process(ListId, [Email], PageId, Context);

send_mailing_process({resend_bounced, ListId}, PageId, Context) ->
    send_mailing_process(ListId, m_mailinglist:get_bounced_recipients(ListId, Context), PageId, Context);

send_mailing_process(ListId, PageId, Context) ->
    Recipients = m_mailinglist:get_enabled_recipients(ListId, Context) ++ m_edge:subjects(ListId, subscriberof, Context),
    send_mailing_process(ListId, Recipients, PageId, Context).

send_mailing_process(ListId, Recipients, PageId, Context) ->
    m_mailinglist:reset_bounced(ListId, Context),
    {Direct,Queued} = split_list(20, Recipients),
    From = m_mailinglist:get_email_from(ListId, Context),
    Options = [
        {id,PageId}, {list_id, ListId}, {email_from, From}
    ],
    [ send(true, Email, From, Options, Context) || Email <- Direct ],
    [ send(false, Email, From, Options, Context) || Email <- Queued ],
    ok.


    send(_IsDirect, undefined, _From, _Options, _Context) ->
        skip;
    send(IsDirect, Id, From, Options, Context) when is_integer(Id) ->
        send(IsDirect, m_rsc:p_no_acl(Id, email, Context), From, [{recipient_id,Id}|Options], Context);
    send(IsDirect, Email, From, Options, Context) ->
        case z_convert:to_list(z_string:trim(Email)) of
            [] -> skip;
            Email1 ->
                Id = proplists:get_value(id, Options),
                Attachments = mod_mailinglist:page_attachments(Id, Context),
                z_email_server:send(#email{queue=not(IsDirect), to=Email1, from=From,
                                           html_tpl={cat, "mailing_page.tpl"}, vars=[{email,Email1}|Options], attachments=Attachments}, Context)
        end.


	split_list(N, List) ->
		split_list(N, List, []).

		split_list(0, List, Acc) ->
			{Acc, List};
		split_list(_N, [], Acc) ->
			{Acc, []};
		split_list(N, [H|T], Acc) ->
			split_list(N-1, T, [H|Acc]).


%% @doc Return list of attachments for this page as a list of files. Attachments are outgoing 'hasdocument' edges.
page_attachments(Id, Context) ->
    AttIds = m_edge:objects(Id, hasdocument, Context),
    [as_upload(AId, Context) || AId <- AttIds].


as_upload(Id, Context) ->
    M = m_media:get(Id, Context),
    #upload{
             tmpfile=z_media_archive:abspath(proplists:get_value(filename, M), Context),
             mime=proplists:get_value(mime, M),
             filename=proplists:get_value(original_filename, M)
           }.
