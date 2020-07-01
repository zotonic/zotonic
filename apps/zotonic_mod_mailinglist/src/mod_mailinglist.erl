%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2017 Marc Worrell
%% @doc Mailinglist implementation. Enables to send pages to a list of recipients.

%% Copyright 2009-2017 Marc Worrell
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
-mod_schema(2).
-mod_depends([ admin, mod_wires, mod_logging, mod_email_status ]).
-mod_provides([ mailinglist ]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
         manage_schema/2,
         observe_search_query/2,
         observe_mailinglist_message/2,
         observe_tick_24h/2,
         event/2,
         observe_admin_menu/3
        ]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-record(state, {context}).

%% @doc Install the tables needed for the mailinglist and return the rsc datamodel.
-spec manage_schema(install | {upgrade, pos_integer()}, z:context()) -> #datamodel{}.
manage_schema(Version, Context) ->
    z_mailinglist_schema:manage_schema(Version, Context).


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
observe_mailinglist_message(#mailinglist_message{what=silent}, _Context) ->
    ok;
observe_mailinglist_message(#mailinglist_message{what=send_goodbye, list_id=ListId, recipient=Props}, Context) ->
    Email = proplists:get_value(email, Props),
    z_email:send_render(Email, "email_mailinglist_goodbye.tpl", [{list_id, ListId}, {email, Email}, {recipient, Props}], Context),
    ok;
observe_mailinglist_message(#mailinglist_message{what=Message, list_id=ListId, recipient=RecipientId}, Context) ->
    Template = case Message of
        send_welcome -> "email_mailinglist_welcome.tpl";
        send_confirm -> "email_mailinglist_confirm.tpl"
    end,
    Props = m_mailinglist:recipient_get(RecipientId, Context),
    z_email:send_render(proplists:get_value(email, Props), Template, [{list_id, ListId}, {recipient, Props}], Context),
    ok.

%% @doc Every 24h cleanup the mailinglists recipients.
observe_tick_24h(tick_24h, Context) ->
    m_mailinglist:periodic_cleanup(Context).


%% @doc Request confirmation of canceling this mailing.
event(#postback{message={dialog_mailing_cancel_confirm, Args}}, Context) ->
    {list_id, MailingId} = proplists:lookup(list_id, Args),
    case is_allowed_mailing(MailingId, Context) of
        true ->
            z_render:dialog(
                ?__("Confirm mailing cancelation.", Context),
                "_dialog_mailing_cancel_confirm.tpl",
                Args,
                Context);
        false ->
            z_render:growl_error(?__("You are not allowed to cancel this mailing.", Context), Context)
    end;
event(#postback{message={mailing_cancel, Args}}, Context) ->
    MailingId = proplists:get_value(list_id, Args),
    PageId = proplists:get_value(page_id, Args),
    case is_allowed_mailing(MailingId, Context) and z_acl:rsc_visible(MailingId, Context) of
        true ->
            m_mailinglist:delete_scheduled(MailingId, PageId, Context),
            z_render:growl(?__("The mailing has been canceled.", Context), Context);
        false ->
            z_render:growl_error(?__("You are not allowed to cancel this mailing.", Context), Context)
    end;
event(#postback{message={mailinglist_reset, Args}}, Context) ->
    MailingId = proplists:get_value(list_id, Args),
    PageId = proplists:get_value(page_id, Args),
    case is_allowed_mailing(MailingId, Context) of
        true ->
            m_mailinglist:reset_log_email(MailingId, PageId, Context),
            z_render:growl(?__("The statistics have been cleared.", Context), Context);
        false ->
            z_render:growl_error(?__("You are not allowed to reset this mailing.", Context), Context)
    end;

%% @doc Handle upload of a new recipients list
event(#submit{message={mailinglist_upload,[{id,MailingId}]}}, Context) ->
    case is_allowed_mailing(MailingId, Context) of
        true ->
            #upload{tmpfile=TmpFile} = z_context:get_q_validated(<<"file">>, Context),
            IsTruncate = z_convert:to_bool(z_context:get_q(<<"truncate">>, Context)),
            case import_file(TmpFile, IsTruncate, MailingId, Context) of
                ok ->
                    z_render:wire([{dialog_close, []}, {reload, []}], Context);
                {error, Msg} ->
                    z_render:growl(Msg, "error", true, Context)
            end;
        false ->
            z_render:growl_error(?__("You are not allowed to reset this mailing.", Context), Context)
    end;

%% @doc Handle the test-sending of a page to a single address.
event(#submit{message={mailing_testaddress, [{id, PageId}]}}, Context) ->
    case z_acl:is_allowed(use, mod_mailinglist, Context) andalso z_acl:rsc_visible(PageId, Context) of
        true ->
            Email = z_context:get_q_validated(<<"email">>, Context),
            z_notifier:notify(#mailinglist_mailing{list_id={single_test_address, Email}, page_id=PageId}, Context),
            Context1 = z_render:growl([?__("Sending the page to", Context), " ", Email, "..."], Context),
            z_render:wire([{dialog_close, []}], Context1);
        false ->
            z_render:growl_error(?__("You are not allowed to send this page.", Context), Context)
    end;

%% @doc Combine lists
event(#submit{message={mailinglist_combine,[{id,Id}]}}, Context) ->
    lager:warning("Id: ~p", [Id]),
    TargetId = z_convert:to_integer(z_context:get_q(<<"list_id">>, Context)),
    Operation = operation(z_context:get_q(<<"operation">>, Context)),
    case m_mailinglist:recipient_set_operation(Operation, Id, TargetId, Context) of
        ok ->
            z_render:wire([{dialog_close, []}, {reload, []}], Context);
        {error, Msg} ->
            z_render:growl(Msg, "error", true, Context)
    end.

operation(<<"union">>) -> union;
operation(<<"subtract">>) -> subtract;
operation(<<"intersection">>) -> intersection.

is_allowed_mailing(MailingId, Context) ->
    z_acl:rsc_editable(MailingId, Context)
    andalso z_acl:is_allowed(use, mod_mailinglist, Context).

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
    lager:md([
        {site, z_context:site(Context)},
        {module, ?MODULE}
      ]),
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
                case import_file(File, true, ListId, C) of
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
import_file(TmpFile, IsTruncate, Id, Context) ->
    {ok, Data} = file:read_file(TmpFile),
    file:delete(TmpFile),
    try
        ok = m_mailinglist:insert_recipients(Id, Data, IsTruncate, Context)
    catch
        _:{badmatch, {rollback, {{case_clause, {error, #error{ codename = character_not_in_repertoire }}},_}}}->
            {error, "The encoding of the input file is not right. Please upload a file with UTF-8 encoding."};
        _:_ ->
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


%% @doc Send the page to the mailinglist.
send_mailing(ListId, PageId, Context) ->
    spawn(fun() -> send_mailing_process(ListId, PageId, z_acl:sudo(Context)) end).


send_mailing_process({single_test_address, Email}, PageId, Context) ->
    Email1 = m_mailinglist:normalize_email(Email),
    {ok, ListId} = m_rsc:name_to_id(mailinglist_test, Context),
    Recipients = #{
        Email1 => #{
            <<"is_enabled">> => true,
            <<"email">> => Email1
        }
    },
    send_mailing_process(ListId, Recipients, PageId, Context);
send_mailing_process(ListId, PageId, Context) ->
    {ok, Recipients} = z_mailinglist_recipients:list_recipients(ListId, Context),
    send_mailing_process(ListId, Recipients, PageId, Context).

send_mailing_process(ListId, Recipients, PageId, Context) when is_map(Recipients) ->
    From = m_mailinglist:get_email_from(ListId, Context),
    Options = [
        {id, PageId},
        {list_id, ListId},
        {email_from, From}
    ],
    maps:fold(
        fun(Email, Recipient, _Acc) ->
            send(Email, Recipient, From, Options, Context)
        end,
        ok,
        Recipients).

send(undefined, _R, _From, _Options, _Context) ->
    skip;
send(<<>>, _R, _From, _Options, _Context) ->
    skip;
send(_Email, RecipientId, From, Options, Context) when is_integer(RecipientId) ->
    Email = m_rsc:p(RecipientId, email_raw, Context),
    PageId = proplists:get_value(id, Options),
    Attachments = m_edge:objects(PageId, hasdocument, Context),
    z_email:send(
        #email{
            to = Email,
            from = From,
            html_tpl = {cat, "mailing_page.tpl"},
            vars = [
                {recipient_id, RecipientId},
                {email, Email}
                | Options
            ],
            attachments = Attachments
        },
        Context);
send(Email, Recipient, From, Options, Context) when is_map(Recipient) ->
    Context1 = case maps:get(<<"pref_language">>, Recipient, undefined) of
        undefined ->
            Context;
        PrefLanguage ->
            z_context:set_language(PrefLanguage, Context)
    end,
    PageId = proplists:get_value(id, Options),
    Attachments = m_edge:objects(PageId, hasdocument, Context),
    z_email:send(
        #email{
            to = Email,
            from = From,
            html_tpl = {cat, "mailing_page.tpl"},
            vars = [
                {email, Email}
                | Options
            ],
            attachments = Attachments
        },
        Context1).

observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
        #menu_item{
            id = admin_mailinglist,
            parent = admin_content,
            label = ?__("Mailing lists", Context),
            url = {admin_mailinglist},
            visiblecheck = {acl, use, ?MODULE}
        }
        | Acc
    ].
