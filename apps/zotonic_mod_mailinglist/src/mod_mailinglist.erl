%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2026 Marc Worrell
%% @doc Mailinglist implementation. Mailings are pages sent to a list of recipients.
%% Recipients are either email addresses in the recipients table, resources matching
%% the mailinglist query, or resources subscribed to the mailinglist using an edge.
%% @end

%% Copyright 2009-2026 Marc Worrell
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
-moduledoc("
This module implements a mailing list system. You can make as many mailing lists as you like and send any page to any
mailing list, including confirm mail and unsubscribe page.

Mailing lists are pages of the category mailinglist, which is installed by the module upon activation. In the admin
there is support for managing these mailing lists where you can import, export, add or delete recipients.

For details on configuring e-mail sending and receiving in Zotonic, see [E-mail handling](/id/doc_developerguide_email#guide-email).



Including the subscribe custom tag on your pages
------------------------------------------------

The module includes the [signup tag](/id/doc_template_scomp_scomp_mailinglist_subscribe#scomp-mailinglist-subscribe) tag
(and template) that you can use on your site.

When you want to add a subscribe template to your page then you will need the following scomp include in your template:


```django
{% mailinglist_subscribe id=mailing_id %}
```

Where `mailing_id` should be set to the page id of your mailing list. This scomp includes the template
`_scomp_mailinglist_subscribe.tpl`. The subscription form itself can be found in the template
“\\_mailinglist\\_subscribe\\_form.tpl”. You should overrule the latter template when you want to add or remove
fields from the subscription form.

It is possible to simplify the subscribe form by using the option `is_email_only`. When this parameter is used the users
only have to supply their email address to be added to the mailing list. Example:


```django
{% mailinglist_subscribe id=mailing_id is_email_only %}
```



Displaying the number of subscribers
------------------------------------

It is possible to show the number of subscribers to a mailinglist by retrieving this number from the mailinglist model. Example:


```django
{{ m.mailinglist.count_recipients[mailinglist_id] }} of subscribers
```



Pages for the mailing list category
-----------------------------------

The mailinglist module predefines the following dispatch rules for the mailinglist:


```django
/mailinglist/1234
/mailinglist/1234/my-mailinglist-slug
```

Where 1234 stands for the id of your mailinglist (which is obviously another id).

The mailinglist page is very simple. It shows the title, summary and body of the mailinglist, followed by a subscribe
form and finally a list of other mailing lists.



Template used for the e-mails
-----------------------------

All e-mails use the [mailing\\_page.tpl](../templates/template_mailing_page.html#template-mailing-page) template. It is
a very simple template that just tells why the recipient received the e-mail, refers to the sent content page on the
Internet and finally shows the title, the summary and the body of the sent page.

In the footer there is a link to the unsubscribe page.

All e-mail templates extend from the [email\\_base.tpl](../templates/template_email_base.html#template-email-base)
template. The following templates are used for e-mails:

| Template                                                                         | Description                                                                      |
| -------------------------------------------------------------------------------- | -------------------------------------------------------------------------------- |
| [email\\\\_mailinglist\\\\_confirm.tpl](../templates/template_email_mailinglist_confirm.html#template-email-mailinglist-confirm) | Sent after subscribing to a mailing list, requests to click on an url to confirm the subscription. |
| [email\\\\_mailinglist\\\\_goodbye.tpl](../templates/template_email_mailinglist_goodbye.html#template-email-mailinglist-goodbye) | Sent after unsubscribing from a mailing list.                                    |
| [email\\\\_mailinglist\\\\_welcome.tpl](../templates/template_email_mailinglist_welcome.html#template-email-mailinglist-welcome) | Sent after subscribing and confirming the subscription.                          |



Sending mailings in the admin
-----------------------------

On the resource edit page of any page (remember: the mailinglist module can send any resource as a mailing!) there is an
link in the right column called Go to the mailing page.



The mailinglist recipients page
-------------------------------

Each mailinglist has a special page in the admin that lets you view the recipients that are part of the list. On that
page you can perform varions functions on the recipients of the list.

*   Add new recipient - Opens a dialog to add a single recipient to the list.
*   Download recipient list - Downloads a `.txt` file with all the e-mail addresses and name details of all recipients.
*   Upload recipient list - Upload a new file with recipients. Each e-mail address goes on its own line. There is a checkbox which lets you clear the list before the import, effectively overwriting all recipients in the list.
*   Clear recipient list - After a confirmation, this removes all recipients from the list.
*   Combine two lists - this opens a dialog which lets you combine two lists. Using this new dialog, the recipients of two lists can be combined according to the three set operations union, subtract and intersect.



The mailing status page
-----------------------

From the mailing status page you can send the current resource to a mailing list, to the test mailing list or to an
email address.



### Sending mailings

The status page lists every mailing list in the system. On each row you see how many recipients the list has, and the
status, e.g. if the mailing has already been sent to this list or not.

Todo

finish description of the mailing status page

Mailings are only send when the to be send page is published and publicly visible. The page should also be within its
publication period.

You can schedule a mailing by publishing the page but setting its publication start date to the date and time you want
your mailing to be send. The mailing list module checks every hour if there are any mailings ready for sending.

An exception is made for the test mailing list, mailings to that mailing list are always sent.
").
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

-mod_title("Mailing list").
-mod_description("Mailing lists. Send a page to a list of recipients.").
-mod_prio(600).
-mod_schema(3).
-mod_depends([ admin, mod_wires, mod_logging, mod_email_status ]).
-mod_provides([ mailinglist ]).
-mod_config([
        #{
            key => email_from,
            type => string,
            default => "",
            description => "The email address used as the 'From' of mailings, used if the "
                           "property 'mailinglist_reply_to' is not set on the mailinglist. Defaults to the smtpfrom "
                           "email address."
        },
        #{
            key => send_confirm,
            type => boolean,
            default => false,
            description => "If true, send a confirmation email to the recipient when they are added to a mailinglist. "
                           "If false, send a welcome email. Can be overridden in the signup form and mailinglist settings."
        },
        #{
            key => recipient_secret,
            type => string,
            default => "",
            description => "The secret used to encrypt the keys to manage a specific subscription for an email address."
                           "This is generated automatically and must be kept secret."
        }
    ]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
    manage_schema/2,
    observe_acl_is_allowed/2,
    observe_search_query/2,
    observe_mailinglist_message/2,
    observe_tick_24h/2,
    event/2,
    observe_admin_menu/3
]).

-export([
    send_mailing_process/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-record(state, { context :: z:context() }).

-type mailing_options() :: [ mailing_option() ].
-type mailing_option() :: {is_match_language, boolean()}
                        | {is_send_all, boolean()}.

-export_type([
    mailing_options/0,
    mailing_option/0
]).

%% Check every three minutes if there is queued mailing that can be sent.
-define(MAILING_POLL_SCHEDULED_INTERVAL, 180_000).


%% @doc Install the tables needed for the mailinglist and return the rsc datamodel.
-spec manage_schema(install | {upgrade, pos_integer()}, z:context()) -> #datamodel{}.
manage_schema(Version, Context) ->
    z_mailinglist_schema:manage_schema(Version, Context).


%% @doc Allow mailinglist admin to add or remove subscribers to mailinglists.
-spec observe_acl_is_allowed(#acl_is_allowed{}, z:context()) -> true | undefined.
observe_acl_is_allowed(#acl_is_allowed{
        object = #acl_edge{
            subject_id = RecipientId,
            predicate = Pred,
            object_id = ListId
        }
    }, Context) when
        Pred =:= subscriberof;
        Pred =:= exsubscriberof ->
    case        z_acl:is_allowed(RecipientId, view, Context)
        andalso z_acl:is_allowed(ListId, update, Context)
        andalso z_acl:is_allowed(use, mod_mailinglist, Context)
    of
        true -> true;
        false -> undefined
    end;
observe_acl_is_allowed(#acl_is_allowed{}, #context{} = _Context) ->
    undefined.


observe_search_query(#search_query{ search = {mailinglist_recipients, [{id,Id}] } }, _Context) ->
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
observe_mailinglist_message(#mailinglist_message{what=send_goodbye, list_id=ListId, recipient=Props}, Context) when is_list(Props) ->
    Email = proplists:get_value(email, Props),
    Vars = [
        {list_id, ListId},
        {email, Email},
        {recipient, Props}
    ],
    Context1 = case proplists:get_value(pref_language, Props) of
        undefined -> Context;
        Lang -> z_context:set_language(Lang, Context)
    end,
    z_email:send_render(Email, "email_mailinglist_goodbye.tpl", Vars, z_acl:sudo(Context1)),
    ok;
observe_mailinglist_message(#mailinglist_message{what=Message, list_id=ListId, recipient=RecipientId}, Context) ->
    Template = case Message of
        send_welcome -> "email_mailinglist_welcome.tpl";
        send_confirm -> "email_mailinglist_confirm.tpl"
    end,
    Props = m_mailinglist:recipient_get(RecipientId, Context),
    Email = proplists:get_value(email, Props),
    Vars = [
        {list_id, ListId},
        {email, Email},
        {recipient, Props}
    ],
    Context1 = case proplists:get_value(pref_language, Props) of
        undefined -> Context;
        Lang -> z_context:set_language(Lang, Context)
    end,
    z_email:send_render(Email, Template, Vars, z_acl:sudo(Context1)),
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
                    ?LOG_INFO(#{
                        in => zotonic_mod_mailinglist,
                        text => <<"Uploaded mailinglist recipients">>,
                        result => ok,
                        mailinglist_id => MailingId,
                        is_truncate => IsTruncate
                    }),
                    z_render:wire([{dialog_close, []}, {reload, []}], Context);
                {error, Msg} ->
                    Msg1 = iolist_to_binary(Msg),
                    ?LOG_ERROR(#{
                        in => zotonic_mod_mailinglist,
                        text => <<"Uploaded mailinglist recipients failed">>,
                        result => error,
                        reason => Msg1,
                        mailinglist_id => MailingId,
                        is_truncate => IsTruncate
                    }),
                    z_render:growl(Msg1, <<"error">>, true, Context)
            end;
        false ->
            z_render:growl_error(?__("You are not allowed to reset this mailing.", Context), Context)
    end;

%% @doc Handle the test-sending of a page to a single address.
event(#submit{message={mailing_testaddress, [{id, PageId}]}}, Context) ->
    case z_acl:is_allowed(use, mod_mailinglist, Context) andalso z_acl:rsc_visible(PageId, Context) of
        true ->
            Email = z_context:get_q_validated(<<"email">>, Context),
            z_notifier:notify(#mailinglist_mailing{
                    email = Email,
                    page_id = PageId
                }, Context),
            Context1 = z_render:growl([?__("Sending the page to", Context), " ", Email, "..."], Context),
            z_render:wire([{dialog_close, []}], Context1);
        false ->
            z_render:growl_error(?__("You are not allowed to send this page.", Context), Context)
    end;

%% @doc Combine lists
event(#submit{message={mailinglist_combine,[{id,Id}]}}, Context) ->
    TargetId = z_convert:to_integer(z_context:get_q(<<"list_id">>, Context)),
    Operation = operation(z_context:get_q(<<"operation">>, Context)),
    case m_mailinglist:recipient_set_operation(Operation, Id, TargetId, Context) of
        ok ->
            z_render:wire([{dialog_close, []}, {reload, []}], Context);
        {error, Msg} ->
            z_render:growl(Msg, "error", true, Context)
    end;

event(#postback{ message = {mailinglist_unsubscribe, Args} }, Context) ->
    {mailinglist_id, MailingId} = proplists:lookup(mailinglist_id, Args),
    OnSuccess = proplists:get_all_values(on_success, Args),
    OnError = proplists:get_all_values(on_error, Args),
    case m_rsc:is_a(MailingId, mailinglist, Context) of
        true ->
            case proplists:get_value(rsc_id, Args) of
                undefined ->
                    ok;
                RscId when is_integer(RscId) ->
                    % Even if not subscribed, register as ex-subscriber to prevent
                    % receiving email in the future. This enables unsubscribing from
                    % mailinglists that find recipients with queries.
                    m_edge:delete(RscId, subscriberof, MailingId, z_acl:sudo(Context)),
                    m_edge:insert(RscId, exsubscriberof, MailingId, z_acl:sudo(Context)),
                    RecipientProps = [
                        {email, m_rsc:p_no_acl(RscId, <<"email_raw">>, Context)},
                        {pref_language, m_rsc:p_no_acl(RscId, <<"pref_language">>, Context)}
                    ],
                    z_notifier:notify1(
                        #mailinglist_message{
                            what = send_goodbye,
                            list_id = MailingId,
                            recipient = RecipientProps
                        }, Context)
            end,
            case proplists:get_value(recipient_id, Args) of
                undefined ->
                    z_render:wire(OnSuccess, Context);
                RecipientId when is_integer(RecipientId) ->
                    case m_mailinglist:recipient_delete(RecipientId, Context) of
                        ok ->
                            z_render:wire(OnSuccess, Context);
                        {error, _Reason} ->
                            z_render:wire(OnError, Context)
                    end
            end;
        false ->
            z_render:wire(OnError, Context)
    end;

event(#submit{ message = {mailinglist_optout, Args} }, Context) ->
    {id, Id} = proplists:lookup(id, Args),
    case m_rsc:rid(Id, Context) of
        undefined ->
            Context;
        RscId ->
            IsOptOut = z_convert:to_bool(z_context:get_q(<<"is_mailing_opt_out">>, Context)),
            case m_rsc:update(RscId, #{ <<"is_mailing_opt_out">> => IsOptOut }, [ {is_acl_check, false} ], Context) of
                {ok, _} ->
                    z_render:growl(?__("Saved the opt-out preference.", Context), Context);
                {error, _} ->
                    z_render:growl_error(?__("Could not save the opt-out preference.", Context), Context)
            end
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

%% @doc Initiates the server.
init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    logger:set_process_metadata(#{
        site => z_context:site(Context),
        module => ?MODULE
    }),
    z_notifier:observe(mailinglist_mailing, self(), Context),
    z_notifier:observe(dropbox_file, self(), 100, Context),
    timer:send_interval(?MAILING_POLL_SCHEDULED_INTERVAL, poll),
    {ok, #state{ context = z_context:new(Context) }}.


%% @doc Handle a drop folder file with recipients.
handle_call({#dropbox_file{ filename = File }, _SenderContext}, _From, State = #state{ context = Context }) ->
    GetFiles = fun() ->
        #search_result{ result = Ids } = z_search:search(
            <<"query">>,
            #{ <<"cat">> => mailinglist },
            1, 10000,
            Context),
        [ {m_rsc:p_no_acl(Id, <<"mailinglist_dropbox_filename">>, Context), Id} || Id <- Ids ]
    end,
    Files = z_depcache:memo(GetFiles, mailinglist_dropbox_filenames, ?WEEK, [mailinglist], Context),
    Basename = filename:basename(unicode:characters_to_binary(File)),
    case proplists:get_value(Basename, Files) of
        undefined ->
            {reply, undefined, State};
        ListId ->
            HandleF = fun() ->
                C = z_acl:sudo(Context),
                case import_file(File, true, ListId, C) of
                    ok ->
                        Title = case z_trans:lookup_fallback(m_rsc:p_no_acl(ListId, <<"title">>, C), C) of
                            <<>> -> integer_to_binary(ListId);
                            T -> T
                        end,
                        z_email:send_admin(
                          "mod_mailinglist: Import from drop folder",
                          ["Replaced all recipients of ", Title, " with the contents of ", File, "."], Context);
                    {error, Msg} ->
                        z_email:send_admin("mod_mailinglist: Import from drop folder FAILED", Msg, Context)
                end
            end,
            spawn(HandleF),
            {reply, ok, State}
    end;

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

%% @doc Send a mailing.
handle_cast({#mailinglist_mailing{
        list_id = List,
        page_id = Page,
        options = Options
    }, SenderContext}, State) when List =/= undefined ->
    ListId = m_rsc:rid(List, SenderContext),
    PageId = m_rsc:rid(Page, SenderContext),
    ?LOG_INFO(#{
        in => zotonic_mod_mailinglist,
        text => <<"Mailing page to mailing list">>,
        list_id => ListId,
        page_id => PageId,
        sender_id => z_acl:user(SenderContext),
        options => Options
    }),
    case proplists:get_bool(is_send_all, Options) of
        true ->
            % Reset the recipient stats so that the mailing can be
            % sent again to all recipients.
            m_mailinglist:reset_log_email(ListId, PageId, SenderContext);
        false ->
            ok
    end,
    send_mailing(ListId, PageId, Options, SenderContext),
    {noreply, State};
handle_cast({#mailinglist_mailing{
        list_id = undefined,
        email = Email,
        page_id = Page,
        options = Options
    }, SenderContext}, State) when Email =/= undefined ->
    PageId = m_rsc:rid(Page, SenderContext),
    Email1 = unicode:characters_to_binary(Email),
    ?LOG_INFO(#{
        in => zotonic_mod_mailinglist,
        text => <<"Mailing page to test mailing address">>,
        email => Email1,
        page_id => PageId,
        sender_id => z_acl:user(SenderContext),
        options => Options
    }),
    send_mailing({single_test_address, Email}, PageId, Options, SenderContext),
    {noreply, State};
handle_cast({#mailinglist_mailing{} = Mailing, _Context}, State) ->
    ?LOG_ERROR(#{
        in => zotonic_mod_mailinglist,
        text => <<"Unmatched mailinglist_mailing notification">>,
        result => error,
        reason => nomatch,
        mailing => Mailing
    }),
    {noreply, State};

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

%% @doc Poll the database for scheduled mailings.
handle_info(poll, State) ->
    poll_scheduled(z_acl:sudo(State#state.context)),
    z_utils:flush_message(poll),
    {noreply, State};

%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    z_notifier:detach(mailinglist_mailing, self(), State#state.context),
    z_notifier:detach(dropbox_file, self(), State#state.context),
    ok.

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
            {error, <<"The encoding of the input file is not right. Please upload a file with UTF-8 encoding.">>};
        _:_ ->
            {error, <<"Something unexpected went wrong while importing the recipients list.">>}
    end.



%% @doc Check if there are any scheduled mailings waiting.
poll_scheduled(Context) ->
    case m_mailinglist:check_scheduled(Context) of
        {ListId, PageId, Options} ->
            m_mailinglist:delete_scheduled(ListId, PageId, Context),
            send_mailing(ListId, PageId, Options, Context);
        undefined ->
            ok
    end.


%% @doc Send the page to the mailinglist.
send_mailing(undefined, _PageId, _Options, _Context) ->
    ok;
send_mailing(_ListId, undefined, _Options, _Context) ->
    ok;
send_mailing(ListId, PageId, Options, Context) ->
    z_sidejob:start(?MODULE, send_mailing_process, [ ListId, PageId, Options ], Context).

send_mailing_process({single_test_address, Email}, PageId, Options, Context) ->
    Email1 = m_mailinglist:normalize_email(Email),
    {ok, ListId} = m_rsc:name_to_id(mailinglist_test, Context),
    Recipients = #{
        Email1 => #{
            <<"is_enabled">> => true,
            <<"email">> => Email1
        }
    },
    send_mailing_process(ListId, Recipients, PageId, Options, Context);
send_mailing_process(ListId, PageId, Options, Context) ->
    Recipients = z_mailinglist_recipients:list_recipients(ListId, Context),
    send_mailing_process(ListId, Recipients, PageId, Options, Context).

send_mailing_process(ListId, Recipients, PageId, Options, Context) when is_map(Recipients) ->
    From = m_mailinglist:get_email_from(ListId, Context),
    Options1 = [
        {id, PageId},
        {list_id, ListId},
        {email_from, From}
        | Options
    ],
    maps:fold(
        fun(Email, Recipient, _Acc) ->
            send(Email, Recipient, From, Options1, Context)
        end,
        ok,
        Recipients).

send(_Email, RecipientId, From, Options, Context) when is_integer(RecipientId) ->
    case m_rsc:p_no_acl(RecipientId, <<"email_raw">>, Context) of
        undefined ->
            skip;
        Email ->
            IsMatchLanguage = proplists:get_bool(is_match_language, Options),
            PageId = proplists:get_value(id, Options),
            ListId = proplists:get_value(list_id, Options),
            PrefLanguage = m_rsc:p_no_acl(RecipientId, <<"pref_language">>, Context),
            case is_matching_language(IsMatchLanguage, PrefLanguage, PageId, Context) of
                true ->
                    Attachments = m_edge:objects(PageId, hasdocument, Context),
                    {ok, RecipientKey} = z_mailinglist_recipients:recipient_key_encode(RecipientId, ListId, Context),
                    z_email:send(
                        #email{
                            to = Email,
                            from = From,
                            html_tpl = {cat, "mailing_page.tpl"},
                            vars = [
                                {recipient_id, RecipientId},
                                {recipient_key, RecipientKey},
                                {email, Email}
                                | Options
                            ],
                            attachments = Attachments
                        },
                        Context);
                false ->
                    skip
            end
    end;
send(Email, #{ <<"rsc_id">> := RscId }, From, Options, Context) when is_integer(RscId) ->
    send(Email, RscId, From, Options, Context);
send(undefined, _R, _From, _Options, _Context) ->
    skip;
send(<<>>, _R, _From, _Options, _Context) ->
    skip;
send(Email, Recipient, From, Options, Context) when is_map(Recipient) ->
    PrefLanguage = maps:get(<<"pref_language">>, Recipient, undefined),
    IsMatchLanguage = proplists:get_bool(is_match_language, Options),
    PageId = proplists:get_value(id, Options),
    ListId = proplists:get_value(list_id, Options),
    case is_matching_language(IsMatchLanguage, PrefLanguage, PageId, Context) of
        true ->
            Context1 = if
                PrefLanguage =:= undefined ->
                    Context;
                true ->
                    z_context:set_language(PrefLanguage, Context)
            end,
            Attachments = m_edge:objects(PageId, hasdocument, Context),
            {ok, RecipientKey} = z_mailinglist_recipients:recipient_key_encode(Email, ListId, Context),
            z_email:send(
                #email{
                    to = Email,
                    from = From,
                    html_tpl = {cat, "mailing_page.tpl"},
                    vars = [
                        {recipient_id, undefined},
                        {recipient_key, RecipientKey},
                        {email, Email}
                        | Options
                    ],
                    attachments = Attachments
                },
                Context1);
        false ->
            skip
    end.

%% @doc Check if the recipient's preferred language matches the languages the page is in.
%% This is not exact science, as there are also language variations. For now we check also
%% the "fallback language" of the preferred language against the provided languages. We do not
%% check the fallback languages of the provided languages against the preferred (fallback) language
%% as it is up to the writer of the email to decide if they write specific language content or
%% more generic language content.
is_matching_language(false, _PrefLanguage, _PageId, _Context) ->
    true;
is_matching_language(true, undefined, _PageId, _Context) ->
    true;
is_matching_language(true, PrefLanguage, PageId, Context) ->
    case z_language:to_language_atom(PrefLanguage) of
        {ok, PrefLang} ->
            case m_rsc:p_no_acl(PageId, <<"language">>, Context) of
                undefined ->
                    true;
                PageLangs ->
                    Fallback = z_language:fallback_language(PrefLang),
                    lists:member(PrefLang, PageLangs) orelse lists:member(Fallback, PageLangs)
            end;
        {error, _} ->
            true
    end.


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
