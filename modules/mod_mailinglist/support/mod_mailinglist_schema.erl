%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2011-10-13

%% @doc Schema definition for mailinglist module, including upgraders

%% Copyright 2011 Arjan Scherpenisse
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

-module(mod_mailinglist_schema).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include_lib("include/zotonic.hrl").

-export([manage_schema/2]).


datamodel() ->
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



%% @doc Install the SQL tables to track recipients and scheduled mailings.
manage_schema(install, Context) ->
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
				)", Context),
    z_datamodel:manage(mod_mailinglist, datamodel(), Context),
    ok.


%% Old upgraders, pre manage_schema/2.
%%     z_db:q("alter table mailinglist_recipient add column props bytea", Context),
%%     z_db:q("alter table mailinglist_recipient drop column user_id", Context),
%%     z_db:q("alter table mailinglist_recipient add column is_bounced boolean not null default false", Context),
