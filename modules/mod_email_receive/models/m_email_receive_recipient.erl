%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Model for the e-mail recipient/handler administration.

%% Copyright 2011 Marc Worrell
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

-module(m_email_receive_recipient).
-author("Marc Worrell <marc@worrell.nl>").

-export([
	install/1,
	get_by_recipient/2,
	ensure/4,
	insert/4,
	delete/4
]).


install(Context) ->
	case z_db:table_exists(email_receive_recipient, Context) of
		true ->
			ok;
		false ->
			create_table(Context)
	end.

%% @doc Lookup the recipient
get_by_recipient(Recipient, Context) ->
	z_db:q_row("select notification, user_id, rsc_id
				from email_receive_recipient
				where recipient = lower($1)",
			   [Recipient],
			   Context).


% @doc Ensure that there is a recipient for the notification
ensure(Notification, UserId, ResourceId, Context) ->
	case get1(Notification, UserId, ResourceId, Context) of
		undefined ->
			insert(Notification, UserId, ResourceId, Context);
		Recipient ->
			{ok, Recipient}
	end.

% Find the recipient by notification, user_id and resource id.
get1(Notification, undefined, undefined, Context) ->
	z_db:q1("select recipient from email_receive_recipient
			where notification = $1
			  and user_id is null
			  and rsc_id is null",
		   [Notification],
		   Context);
get1(Notification, UserId, undefined, Context) ->
	z_db:q1("select recipient from email_receive_recipient
			where notification = $1
			  and user_id = $2
			  and rsc_id is null",
		   [Notification, UserId],
		   Context);
get1(Notification, undefined, RscId, Context) ->
	z_db:q1("select recipient from email_receive_recipient
			where notification = $1
			  and user_id is null
			  and rsc_id = $2",
		   [Notification, RscId],
		   Context);
get1(Notification, UserId, ResourceId, Context) ->
	z_db:q1("select recipient from email_receive_recipient
			where notification = $1
			  and user_id = $2
			  and rsc_id = $3",
		   [Notification, UserId, ResourceId],
		   Context).

% @doc Insert a new e-mail address, return the created unique e-mail address
insert(Notification, UserId, ResourceId, Context) ->
	Recipient = z_string:to_lower(z_ids:id(12)),
	case z_db:q1("select count(*) from email_receive_recipient where recipient = $1", [Recipient], Context) of
		1 ->
			insert(Notification, UserId, ResourceId, Context);
		0 ->
			1 = z_db:q("insert into email_receive_recipient (recipient, notification, user_id, rsc_id)
						values ($1, $2, $3, $4)",
					   [Recipient, Notification, UserId, ResourceId],
					   Context),
			{ok, Recipient}
	end.


% @doc Delete an e-mail address
delete(Notification, undefined, undefined, Context) ->
	z_db:q("delete from email_receive_recipient
			where notification = $1
			  and user_id is null
			  and rsc_id is null",
		   [Notification],
		   Context),
	ok;
delete(Notification, UserId, undefined, Context) ->
	z_db:q("delete from email_receive_recipient
			where notification = $1
			  and user_id = $2
			  and rsc_id is null",
		   [Notification, UserId],
		   Context),
	ok;
delete(Notification, undefined, RscId, Context) ->
	z_db:q("delete from email_receive_recipient
			where notification = $1
			  and user_id is null
			  and rsc_id = $2",
		   [Notification, RscId],
		   Context),
	ok;
delete(Notification, UserId, any, Context) ->
	z_db:q("delete from email_receive_recipient
			where notification = $1
			  and user_id = $2",
		   [Notification, UserId],
		   Context),
	ok;
delete(Notification, UserId, ResourceId, Context) ->
	z_db:q("delete from email_receive_recipient
			where notification = $1
			  and user_id = $2
			  and rsc_id = $3",
		   [Notification, UserId, ResourceId],
		   Context),
	ok.


%% @doc Create the table for registering the unique recipients
create_table(Context) ->
    [] = z_db:q("
            create table email_receive_recipient (
				recipient varchar(20) not null,
				notification varchar(64) not null,
				user_id integer,
                rsc_id integer,
				created timestamp not null default current_timestamp,

                CONSTRAINT email_receive_recipient_pkey PRIMARY KEY (recipient),
                CONSTRAINT fk_email_receive_recipient_user_id FOREIGN KEY (user_id)
                    REFERENCES rsc (id)
                    ON UPDATE CASCADE ON DELETE CASCADE,
                CONSTRAINT fk_email_receive_recipient_rsc_id FOREIGN KEY (rsc_id)
                    REFERENCES rsc (id)
                    ON UPDATE CASCADE ON DELETE CASCADE
            )", Context),
    [] = z_db:q("CREATE INDEX fki_email_receive_recipient_rsc_id ON email_receive_recipient (rsc_id)", Context),
    [] = z_db:q("CREATE INDEX fki_email_receive_recipient_user_id ON email_receive_recipient (user_id)", Context),
    [] = z_db:q("CREATE INDEX email_receive_recipient_notification_key ON email_receive_recipient (notification)", Context),
    ok.
