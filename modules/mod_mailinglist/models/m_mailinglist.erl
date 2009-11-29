%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-11-23
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
	
	recipient_get/2,
	recipient_get/3,
	recipient_delete/2,
	recipient_delete/3,
	recipient_confirm/2,
	recipient_is_enabled_toggle/2,
	
	insert_scheduled/3,
	delete_scheduled/3,
	get_scheduled/2,
	check_scheduled/1,
	
	install/1
]).

-include_lib("zotonic.hrl").


%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(stats, #m{value=undefined} = M, _Context) ->
   M#m{value=stats};
m_find_value(Id, #m{value=stats}, Context) ->
   get_stats(Id, Context);
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
%% @spec m_to_list(Source, Context)
m_to_list(_, _Context) ->
    [].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
    undefined.


%% @doc Get the stats for the mailing. Number of recipients and list of scheduled resources.
%% @spec get_stats(int(), Context) -> {count::int(), [rsc_id::int()]}
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
	{Count, Scheduled}.


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

%% @doc Delete a recipient.
recipient_delete(RecipientId, Context) ->
	case recipient_get(RecipientId, Context) of
		undefined -> {error, enoent};
		RecipientProps -> recipient_delete1(RecipientProps, Context)
	end.
	
	recipient_delete1(RecipientProps, Context) ->
		RecipientId = proplists:get_value(id, RecipientProps),
		z_db:q("delete from mailinglist_recipient where id = $1", [RecipientId], Context),
		ListId = proplists:get_value(mailinglist_id, RecipientProps),
		Email = proplists:get_value(email, RecipientProps),
		z_notifier:notify1({mailinglist_message, send_goodbye, ListId, Email}, Context),
		ok.

%% @doc Delete a recipient by list id and email
recipient_delete(ListId, Email, Context) ->
	case recipient_get(ListId, Email, Context) of
		undefined -> {error, enoent};
		RecipientProps -> recipient_delete1(RecipientProps, Context)
	end.

%% @doc Confirm the recipient with the given unique confirmation key.
%% @spec recipient_confirm(ConfirmKey, Context) -> {ok, RecipientId} | {error, Reason}
recipient_confirm(ConfirmKey, Context) ->
	case z_db:q_row("select id, is_enabled, mailinglist_id from mailinglist_recipient where confirm_key = $1", [ConfirmKey], Context) of
		{RecipientId, _IsEnabled, ListId} ->
			NewConfirmKey = z_ids:id(20),
			z_db:q("update mailinglist_recipient set confirm_key = $2, is_enabled = true where confirm_key = $1", [ConfirmKey, NewConfirmKey], Context),
			z_notifier:notify({mailinglist_message, send_welcome, ListId, RecipientId}, Context),
			{ok, RecipientId};
		undefined ->
			{error, enoent}
	end.
	

%% @doc Fetch the information for a confirmation key
%% @spec get_confirm_key(ConfirmKey, Context) -> Proplist | undefined
get_confirm_key(ConfirmKey, Context) ->
	z_db:assoc_row("select id, mailinglist_id, email, confirm_key from mailinglist_recipient where confirm_key = $1", [ConfirmKey], Context).


%% @doc Insert a recipient in the mailing list, send a message to the recipient when needed.
insert_recipient(ListId, Email, Message, Context) ->
	true = z_acl:rsc_visible(ListId, Context),
	Email1 = z_string:to_lower(Email),
	Rec = z_db:q_row("select id, is_enabled, confirm_key
					  from mailinglist_recipient 
					  where mailinglist_id = $1
					    and email = $2", [ListId, Email1], Context),
	ConfirmKey = z_ids:id(20),
	Message1 = case Rec of
		{RecipientId, true, _OldConfirmKey} ->
			%% Present and enabled
			silent;
		{RecipientId, false, OldConfirmKey} ->
			%% Present, but not enabled
			NewConfirmKey = case OldConfirmKey of undefined -> ConfirmKey; _ -> OldConfirmKey end,
			case Message of
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
					Message
			end;
		undefined ->
			%% Not present
			IsEnabled = case Message of
				send_welcome -> true;
				send_confirm -> false;
				silent -> true
			end,
			Cols = [
				{mailinglist_id, ListId},
				{is_enabled, IsEnabled},
				{email, Email1},
				{confirm_key, ConfirmKey}
			],
			{ok, RecipientId} = z_db:insert(mailinglist_recipient, Cols, Context),
			Message
	end,
	case Message1 of
		none -> nop;
		_ -> z_notifier:notify({mailinglist_message, Message1, ListId, RecipientId}, Context)
	end,
	ok.

%% @doc Replace all recipients of the mailinglist. Do not send welcome messages to the recipients.
%% @spec replace_recipients(ListId::int(), Recipients::list(), Context) -> ok | {error, Error}
replace_recipients(ListId, Recipients, Context) ->
	true = z_acl:rsc_editable(ListId, Context),
	z_db:transaction(fun(Ctx) -> replace_recipients1(ListId, Recipients, Ctx) end, Context).
	
	replace_recipients1(ListId, Recipients, Context) ->
		Now = erlang:localtime(),
		[ replace_recipient(ListId, R, Now, Context) || R <- Recipients ],
		z_db:q("
			delete from mailinglist_recipient 
			where mailinglist_id = $1 
			  and timestamp < $2", [ListId, Now], Context),
		ok.
	
	replace_recipient(ListId, Recipient, Now, Context) ->
		Recipient1 = z_string:to_lower(Recipient),
		case z_db:q("select id from mailinglist_recipient where mailinglist_id = $1 and email = $2", [ListId, Recipient1], Context) of
			undefined -> 
				ConfirmKey = z_ids:id(20),
				z_db:q("
					insert into mailinglist_recipient (is_enabled, mailinglist_id, email, timestamp, confirm_key)
					values (true, $1, $2, $3, $4)", [ListId, Recipient1, Now, ConfirmKey], Context);
			RecipientId ->
				z_db:q("
					update mailinglist_recipient 
					set timestamp = $1,
					    is_enabled = true
					where id = $2", [Now, RecipientId], Context)
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
	z_db:q("select mailinglist_id
			from mailinglist_scheduled
			where page_id = $1", [Id], Context).


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


%% @doc Install the tables needed for the mailinglist
%% @spec install(Context) -> void()
install(Context) ->
	% Install the mailinglist category.
	z_datamodel:manage(mod_mailinglist, datamodel(), Context),

	% Install the SQL tables to track recipients and scheduled mailings.
	case z_db:table_exists(mailinglist_recipient, Context) of
		true ->
			ok;
		false ->
			z_db:q("
				CREATE TABLE mailinglist_recipient (
					id serial NOT NULL,
					mailinglist_id INT NOT NULL,
					email character varying (200) NOT NULL,
					is_enabled boolean NOT NULL default true,
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

datamodel() ->
	[
		{categories, [
			{mailinglist, undefined, [
							{title, "Mailing list"},
							{summary, "Mailing lists are used to send pages to groups of people."}
						]}
		]},
		
		{resources, [
			{mailinglist_test, mailinglist, [
							{visible_for, 1},
							{title, "Test mailing list"},
							{summary, "This list is used for testing. Anyone who can see this mailing list can post to it. It should not be visible for the world."}
						]}
		]}
	].
	
	