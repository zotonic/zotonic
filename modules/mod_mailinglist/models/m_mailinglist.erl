%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-11-23
%%
%% @doc Mailing list access for templates.

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

-module(m_mailinglist).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,

	get_stats/2,
	get_enabled_recipients/2,
	replace_recipients/3,
	insert_recipient/4,
	insert_recipient/5,

	update_recipient/3,
	
	recipient_get/2,
	recipient_get/3,
	recipient_delete/2,
	recipient_delete/3,
	recipient_delete_quiet/2,
	recipient_confirm/2,
	recipient_is_enabled_toggle/2,
	recipients_clear/2,

	insert_scheduled/3,
	delete_scheduled/3,
	get_scheduled/2,
	check_scheduled/1,

	get_email_from/2,
	get_recipients_by_email/2,
    reset_log_email/3,

	reset_bounced/2,
	get_bounced_recipients/2,

	init_tables/1
]).

-include_lib("zotonic.hrl").


%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(stats, #m{value=undefined} = M, _Context) ->
   M#m{value=stats};
m_find_value(Id, #m{value=stats}, Context) ->
   get_stats(Id, Context);
m_find_value(rsc_stats, #m{value=undefined} = M, _Context) ->
   M#m{value=rsc_stats};
m_find_value(Id, #m{value=rsc_stats}, Context) ->
   get_rsc_stats(Id, Context);
m_find_value(recipient, #m{value=undefined} = M, _Context) ->
   M#m{value=recipient};
m_find_value(Id, #m{value=recipient}, Context) ->
   recipient_get(Id, Context);
m_find_value(scheduled, #m{value=undefined} = M, _Context) ->
   M#m{value=scheduled};
m_find_value(Id, #m{value=scheduled}, Context) ->
   get_scheduled(Id, Context);
m_find_value(confirm_key, #m{value=undefined} = M, _Context) ->
   M#m{value=confirm_key};
m_find_value(ConfirmKey, #m{value=confirm_key}, Context) ->
   get_confirm_key(ConfirmKey, Context);
m_find_value(subscription, #m{value=undefined} = M, _Context) ->
   M#m{value=subscription};
m_find_value(ListId, #m{value=subscription} = M, _Context) ->
   M#m{value={subscription, ListId}};
m_find_value(Email, #m{value={subscription, ListId}}, Context) ->
   recipient_get(ListId, Email, Context);
m_find_value(_Key, #m{value=undefined}, _Context) ->
   undefined.

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> list()
m_to_list(_, _Context) ->
    [].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
    undefined.


%% @doc Get the stats for the mailing. Number of recipients and list of scheduled resources.
%% @spec get_stats(int(), Context) -> {Count::int(), [RscId::int()]}
get_stats(Id, Context) ->
	Count = z_db:q1("
			select count(*) from mailinglist_recipient
			where mailinglist_id = $1", [Id], Context),
	Scheduled = z_db:q("
					select page_id
					from mailinglist_scheduled
							join rsc on id = page_id
					where mailinglist_id = $1
					order by publication_start", [Id], Context),
	{Count + length(m_edge:subjects(Id, subscriber_of, Context)), Scheduled}.


%% @doc Get the stats for all mailing lists which have been sent to a rsc (content_id)
%% @spec get_rsc_stats(int(), Context) -> [ {ListId::int(), Statuslist} ]
get_rsc_stats(Id, Context) ->
    F = fun() ->
                Stats = [ {ListId, [{created, Created}, {total, Total}]} || 
                            {ListId, Created, Total} <- z_db:q("select other_id, min(created) as sent_on, count(distinct(envelop_to)) from log_email where content_id = $1 group by other_id", [Id], Context)],
                %% merge in all mailer statuses
                lists:foldl(fun({ListId, Status, Count}, St) ->
                                    z_utils:prop_replace(ListId,
                                                         [{z_convert:to_atom(Status), Count}|proplists:get_value(ListId, St, [])],
                                                         St)
                            end,
                            Stats,
                            z_db:q("select other_id, mailer_status, count(envelop_to) from log_email where content_id = $1 group by other_id, mailer_status", [Id], Context))
        end,
    z_depcache:memo(F, {mailinglist_stats, Id}, 1, [Id], Context). %% Cache a little while to prevent database DOS while mail is sending



%% @doc Fetch all enabled recipients from a list.
get_enabled_recipients(ListId, Context) ->
	Emails = z_db:q("select email from mailinglist_recipient where mailinglist_id = $1 and is_enabled = true", [ListId], Context),
	[ E || {E} <- Emails ].


%% @doc Toggle the enabled flag of a recipient
recipient_is_enabled_toggle(RecipientId, Context) ->
	1 = z_db:q("update mailinglist_recipient set is_enabled = not is_enabled where id = $1", [RecipientId], Context),
	ok.

%% @doc Fetch the recipient record for the recipient id.
recipient_get(RecipientId, Context) ->
	z_db:assoc_row("select * from mailinglist_recipient where id = $1", [RecipientId], Context).

%% @doc Fetch the recipient record by e-mail address
recipient_get(ListId, Email, Context) ->
	z_db:assoc_row("select * from mailinglist_recipient where mailinglist_id = $1 and email = $2", [ListId, Email], Context).


%% @doc Delete a recipient without sending the recipient a goodbye e-mail.
recipient_delete_quiet(RecipientId, Context) ->
	case recipient_get(RecipientId, Context) of
		undefined -> {error, enoent};
		RecipientProps -> recipient_delete1(RecipientProps, true, Context)
	end.

%% @doc Delete a recipient and send the recipient a goodbye e-mail.
recipient_delete(RecipientId, Context) ->
	case recipient_get(RecipientId, Context) of
		undefined -> {error, enoent};
		RecipientProps -> recipient_delete1(RecipientProps, false, Context)
	end.
	
	recipient_delete1(RecipientProps, Quiet, Context) ->
		RecipientId = proplists:get_value(id, RecipientProps),
        z_db:delete(mailinglist_recipient, RecipientId, Context),
		ListId = proplists:get_value(mailinglist_id, RecipientProps),
		Email = proplists:get_value(email, RecipientProps),
		case Quiet of
			false ->
				z_notifier:notify1(#mailinglist_message{what=send_goodbye, list_id=ListId, recipient=Email}, Context);
			_ -> nop
		end,
		ok.

%% @doc Delete a recipient by list id and email
recipient_delete(ListId, Email, Context) ->
	case recipient_get(ListId, Email, Context) of
		undefined -> {error, enoent};
		RecipientProps -> recipient_delete1(RecipientProps, false, Context)
	end.

%% @doc Confirm the recipient with the given unique confirmation key.
%% @spec recipient_confirm(ConfirmKey, Context) -> {ok, RecipientId} | {error, Reason}
recipient_confirm(ConfirmKey, Context) ->
	case z_db:q_row("select id, is_enabled, mailinglist_id from mailinglist_recipient where confirm_key = $1", [ConfirmKey], Context) of
		{RecipientId, _IsEnabled, ListId} ->
			NewConfirmKey = z_ids:id(20),
			z_db:q("update mailinglist_recipient set confirm_key = $2, is_enabled = true where confirm_key = $1", [ConfirmKey, NewConfirmKey], Context),
			z_notifier:notify(#mailinglist_message{what=send_welcome, list_id=ListId, recipient=RecipientId}, Context),
			{ok, RecipientId};
		undefined ->
			{error, enoent}
	end.

%% @doc Clear all recipients of the list
%% @spec recipients_clear(ListId, Context) -> ok
recipients_clear(ListId, Context) ->
    %% TODO clear person edges to list
    z_db:q("delete from mailinglist_recipient where mailinglist_id = $1", [ListId], Context),
    ok.

%% @doc Fetch the information for a confirmation key
%% @spec get_confirm_key(ConfirmKey, Context) -> Proplist | undefined
get_confirm_key(ConfirmKey, Context) ->
	z_db:assoc_row("select id, mailinglist_id, email, confirm_key from mailinglist_recipient where confirm_key = $1", [ConfirmKey], Context).


%% @doc Insert a recipient in the mailing list, send a message to the recipient when needed.
insert_recipient(ListId, Email, WelcomeMessageType, Context) ->
    insert_recipient(ListId, Email, [], WelcomeMessageType, Context).

insert_recipient(ListId, Email, Props, WelcomeMessageType, Context) ->
	true = z_acl:rsc_visible(ListId, Context),
	Email1 = z_string:to_lower(Email),
	Rec = z_db:q_row("select id, is_enabled, confirm_key
					  from mailinglist_recipient 
					  where mailinglist_id = $1
					    and email = $2", [ListId, Email1], Context),
	ConfirmKey = z_ids:id(20),
	WelcomeMessageType1 = case Rec of
		{RecipientId, true, _OldConfirmKey} ->
			%% Present and enabled
			silent;
		{RecipientId, false, OldConfirmKey} ->
			%% Present, but not enabled
			NewConfirmKey = case OldConfirmKey of undefined -> ConfirmKey; _ -> OldConfirmKey end,
			case WelcomeMessageType of
				send_confirm -> 
					case NewConfirmKey of
						OldConfirmKey -> nop;
						_ -> z_db:q("update mailinglist_recipient 
									 set confirm_key = $2
									 where id = $1", [RecipientId, NewConfirmKey], Context)
					end,
					{send_confirm, NewConfirmKey};
				_ ->
					z_db:q("update mailinglist_recipient 
							set is_enabled = true,
							    confirm_key = $2
							where id = $1", [RecipientId, NewConfirmKey], Context),
					WelcomeMessageType
			end;
		undefined ->
			%% Not present
			IsEnabled = case WelcomeMessageType of
				send_welcome -> true;
				send_confirm -> false;
				silent -> true
			end,
			Cols = [
				{mailinglist_id, ListId},
				{is_enabled, IsEnabled},
				{email, Email1},
				{confirm_key, ConfirmKey}
			] ++ [ {K, case is_list(V) of true-> z_convert:to_binary(V); false -> V end} || {K,V} <- Props ],
			{ok, RecipientId} = z_db:insert(mailinglist_recipient, Cols, Context),
			WelcomeMessageType
	end,
	case WelcomeMessageType1 of
		none -> nop;
		_ -> z_notifier:notify(#mailinglist_message{what=WelcomeMessageType1, list_id=ListId, recipient=RecipientId}, Context)
	end,
	ok.


%% @doc Update a single recipient; changing e-mail address or name details.
update_recipient(RcptId, Props, Context) ->
    {ok, _} = z_db:update(mailinglist_recipient, RcptId, Props, Context),
    ok.


%% @doc Replace all recipients found in the binary file. One recipient per line.
replace_recipients(ListId, Bin, Context) when is_binary(Bin) ->
	Lines = z_string:split_lines(Bin),
    Rcpts = lines_to_recipients(Lines),
	replace_recipients(ListId, Rcpts, Context);

%% @doc Replace all recipients of the mailinglist. Do not send welcome messages to the recipients.
%% @spec replace_recipients(ListId::int(), Recipients::list(), Context) -> ok | {error, Error}
replace_recipients(ListId, Recipients, Context) ->
	case z_acl:rsc_editable(ListId, Context) of
		true ->
			ok = z_db:transaction(fun(Ctx) -> replace_recipients1(ListId, Recipients, Ctx) end, Context);
		false ->
			{error, eacces}
	end.
	
	replace_recipients1(ListId, Recipients, Context) ->
		Now = erlang:localtime(),
		[ replace_recipient(ListId, R, Now, Context) || R <- Recipients ],
		z_db:q("
			delete from mailinglist_recipient 
			where mailinglist_id = $1 
			  and timestamp < $2", [ListId, Now], Context),
		ok.
	
replace_recipient(ListId, Recipient, Now, Context) when is_binary(Recipient) ->
    replace_recipient(ListId, Recipient, [], Now, Context);
replace_recipient(ListId, Recipient, Now, Context) ->
    replace_recipient(ListId, proplists:get_value(email, Recipient), proplists:delete(email, Recipient), Now, Context).


replace_recipient(ListId, Email, Props, Now, Context) ->
    case z_string:trim(z_string:to_lower(Email)) of
        "" ->
            skip;
        Email1 ->
            case z_db:q1("select id from mailinglist_recipient where mailinglist_id = $1 and email = $2", 
                         [ListId, Email1], Context) of
                undefined -> 
                    ConfirmKey = z_ids:id(20),
                    Props1 = [{confirm_key, ConfirmKey},
                              {email, Email1},
                              {timestamp, Now},
                              {mailinglist_id, ListId},
                              {is_enabled, true}] ++ Props,
                    z_db:insert(mailinglist_recipient, Props1, Context);
                EmailId ->
                    z_db:update(mailinglist_recipient, EmailId, [{timestamp, Now}, {is_enabled, true}] ++ Props, Context)
            end
    end.


lines_to_recipients(Lines) ->
    lines_to_recipients(Lines, []).
lines_to_recipients([], Acc) -> Acc;
lines_to_recipients([Line|Lines], Acc) ->
    %% Split every line on tab
    case string:tokens(z_string:trim(binary_to_list(Line)), [9]) of
        [] ->
            %% Skip
            lines_to_recipients(Lines, Acc);
        Items0 ->
            Items = [z_string:unquote(z_string:unquote(I, $'), $") || I <- Items0],
            R = case length(Items) of
                    1 -> [{email, hd(Items)}];
                    2 -> [{email, hd(Items)}, {name_first, lists:nth(2, Items)}];
                    3 -> [{email, hd(Items)}, {name_first, lists:nth(2, Items)}, {name_surname, lists:nth(3, Items)}];
                    _ -> [{email, hd(Items)}, {name_first, lists:nth(2, Items)}, {name_surname, lists:nth(3, Items)}, {name_surname_prefix, lists:nth(4, Items)}]
                end,
            lines_to_recipients(Lines, [R|Acc])
    end.
            
%% @doc Insert a mailing to be send when the page becomes visible
insert_scheduled(ListId, PageId, Context) ->
	true = z_acl:rsc_editable(ListId, Context),
	Exists = z_db:q1("
				select count(*) 
				from mailinglist_scheduled 
				where page_id = $1 and mailinglist_id = $2", [PageId,ListId], Context),
	case Exists of
		0 ->
			z_db:q("insert into mailinglist_scheduled (page_id, mailinglist_id) values ($1,$2)",
					[PageId, ListId], Context);
		1 -> 
			nop
	end.
	
%% @doc Delete a scheduled mailing
delete_scheduled(ListId, PageId, Context) ->
	true = z_acl:rsc_editable(ListId, Context),
	z_db:q("delete from mailinglist_scheduled where page_id = $1 and mailinglist_id = $2", [PageId,ListId], Context).

%% @doc Get the list of scheduled mailings for a page.
get_scheduled(Id, Context) ->
	[ListId || {ListId} <- z_db:q("select mailinglist_id
                			from mailinglist_scheduled
                			where page_id = $1", [Id], Context)].


%% @doc Fetch the next scheduled mailing that is publicly visible, published and in the publication date range.
check_scheduled(Context) ->
	z_db:q_row("
		select m.mailinglist_id, m.page_id 
		from mailinglist_scheduled m
			join rsc r on m.page_id = r.id
		where r.is_published
		  and r.visible_for = 0
		  and r.publication_start <= now()
		  and r.publication_end >= now()
		limit 1", Context).


%% @doc Reset the email log for given list/page combination, allowing one to send the same page again to the given list.
reset_log_email(ListId, PageId, Context) ->
    z_db:q("delete from log_email where other_id = $1 and content_id = $2", [ListId, PageId], Context),
    z_depcache:flush({mailinglist_stats, PageId}, Context),
    ok.


%% @doc Get the "from" address used for this mailing list. Looks first in the mailinglist rsc for a ' mailinglist_reply_to' field; falls back to site.email_from config variable.
get_email_from(ListId, Context) ->
    FromEmail = case m_rsc:p(ListId, mailinglist_reply_to, Context) of
                    Empty when Empty =:= undefined; Empty =:= <<>> ->
                        z_convert:to_list(m_config:get_value(site, email_from, Context));
                    RT ->
                        z_convert:to_list(RT)
                end,
    FromName = case m_rsc:p(ListId, mailinglist_sender_name, Context) of
                  undefined -> [];
                  <<>> -> []; 
                  SenderName -> z_convert:to_list(SenderName)
               end,
    z_email:combine_name_email(FromName, FromEmail).


% Install the SQL tables to track recipients and scheduled mailings.
init_tables(Context) ->
	case z_db:table_exists(mailinglist_recipient, Context) of
		true ->
		    case z_db:column_names(mailinglist_recipient, Context) of
		        [confirm_key,email,id,is_enabled,mailinglist_id,timestamp] ->
		            z_db:q("alter table mailinglist_recipient
		                        add column props bytea", Context),
		            z_db:flush(Context),
			    ok;
		        [confirm_key,email,id,is_enabled,mailinglist_id,props,timestamp,user_id] ->
		            z_db:q("alter table mailinglist_recipient
		                        drop column user_id", Context),
		            z_db:flush(Context),
			    ok;
                [confirm_key,email,id,is_enabled,mailinglist_id,props,timestamp] ->
		            z_db:q("alter table mailinglist_recipient
                                add column is_bounced boolean not null default false", Context),
		            z_db:flush(Context),
                    ok;
                [confirm_key,email,id,is_bounced,is_enabled,mailinglist_id,props,timestamp] ->
		            ok
		    end;
		false ->
			z_db:q("
				CREATE TABLE mailinglist_recipient (
					id serial NOT NULL,
					mailinglist_id INT NOT NULL,
					email character varying (200) NOT NULL,
					is_enabled boolean NOT NULL default true,
                    is_bounced boolean NOT NULL default false,
					props bytea,
					confirm_key character varying (32) NOT NULL,
					timestamp timestamp with time zone NOT NULL DEFAULT now(),

					CONSTRAINT mailinglist_recipient_pkey PRIMARY KEY (id),
					CONSTRAINT mailinglist_recipient_mailinglist_id_email_key UNIQUE (mailinglist_id, email),
			        CONSTRAINT confirm_key_key UNIQUE (confirm_key),
					CONSTRAINT fk_mailinglist_id FOREIGN KEY (mailinglist_id)
				      REFERENCES rsc (id)
				      ON UPDATE CASCADE ON DELETE CASCADE
				)", Context),

			z_db:q("
				CREATE TABLE mailinglist_scheduled (
					page_id INT NOT NULL,
					mailinglist_id INT NOT NULL,

					CONSTRAINT mailinglist_scheduled_pkey PRIMARY KEY (page_id, mailinglist_id),
					CONSTRAINT fk_mailinglist_id FOREIGN KEY (mailinglist_id)
				      REFERENCES rsc (id)
				      ON UPDATE CASCADE ON DELETE CASCADE,
					CONSTRAINT fk_page_id FOREIGN KEY (page_id)
				      REFERENCES rsc (id)
				      ON UPDATE CASCADE ON DELETE CASCADE
				)", Context)
	end.


%% @doc Given an address, get all recipients with this email.
get_recipients_by_email(Email, Context) ->
    [Id || {Id} <- z_db:q("SELECT id FROM mailinglist_recipient WHERE email = $1", [Email], Context)].


%% @doc Reset the bounced state for the given mailing list.
reset_bounced(ListId, Context) ->
    z_db:q("UPDATE mailinglist_recipient SET is_bounced = false WHERE mailinglist_id = $1 AND is_enabled = true", [ListId], Context).


%% @doc Get all email addresses for the given list which have the is_bounced flag set.
get_bounced_recipients(ListId, Context) ->
	Emails = z_db:q("SELECT email FROM mailinglist_recipient WHERE mailinglist_id = $1 AND is_bounced = true AND is_enabled = true", [ListId], Context),
	[ E || {E} <- Emails ].
