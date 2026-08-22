%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011-2026 Arjan Scherpenisse <arjan@scherpenisse.net>
%% @doc Schema definition for mailinglist module, including upgraders
%% @end

%% Copyright 2011-2026 Arjan Scherpenisse
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

-module(z_mailinglist_schema).
-moduledoc("
Installs and upgrades the mailing-list recipient and scheduled-mailing tables.

Scheduled rows have type publication when waiting for the page publication
period, or type date when waiting for their due timestamp.
").
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include_lib("zotonic_core/include/zotonic.hrl").

-export([manage_schema/2]).


datamodel() ->
    #datamodel{
        categories = [
            {mailinglist, undefined, #{
                <<"title">> => #trans{ tr = [
                    {en, <<"Mailing List">>},
                    {nl, <<"Mailinglijst">>}
                ]},
                <<"summary">> => #trans{ tr = [
                    {en, <<"Mailing lists are used to send pages to groups of people.">>},
                    {nl, <<"Mailinglijsten worden gebruikt om pagina's naar groepen mensen te versturen.">>}
                ]}
            }}
        ],

        % Any resource with an e-mail address can be a subscriber of a mailinglist
        predicates = [
            {subscriberof,
                #{
                    <<"title">> => #trans{ tr = [
                        {en, <<"Subscriber of">>},
                        {nl, <<"Abonnee">>}
                    ]}
                },
                [ {person, mailinglist}, {location, mailinglist} ]},
            {exsubscriberof,
                #{
                    <<"title">> => #trans{ tr = [
                        {en, <<"Ex-subscriber of">>},
                        {nl, <<"Oud abonnee">>}
                    ]}
                },
                [ {person, mailinglist}, {location, mailinglist} ]},
            {hasattachment,
                #{
                    <<"title">> => #trans{ tr = [
                        {en, <<"Attachment">>},
                        {nl, <<"Bijlage">>}
                    ]}
                },
                [ {undefined, media} ]}
        ],

        resources = [
            {mailinglist_test, mailinglist, #{
                <<"is_published">> => false,
                <<"title">> => <<"Test mailing list">>,
                <<"summary">> => <<"This list is used for testing. Anyone who can see this mailing list can post to it. It SHOULD NOT be visible for the world.">>
            }}
        ]
    }.


%% @doc Install or upgrade the SQL tables used for recipients and waiting mailings.
manage_schema(_Upgrade, Context) ->
    case z_db:table_exists(mailinglist_recipient, Context) of
        false ->
            do_install(Context);
        true ->
            case z_db:column_exists(mailinglist_recipient, is_bounced, Context) of
                true ->
                    [] = z_db:q("
                        alter table mailinglist_recipient
                        drop column is_bounced",
                        Context),
                    z_db:flush(Context);
                false ->
                    ok
            end,
            case z_db:column_exists(mailinglist_scheduled, props, Context) of
                false ->
                    [] = z_db:q("
                        alter table mailinglist_scheduled
                        add column props bytea,
                        add column timestamp timestamp with time zone NOT NULL DEFAULT now()",
                        Context),
                    z_db:flush(Context);
                true ->
                    ok
            end,
            ensure_scheduled_columns(Context)
    end,
    datamodel().

ensure_scheduled_columns(Context) ->
    case z_db:column_exists(mailinglist_scheduled, due, Context) of
        true ->
            ok;
        false ->
            [] = z_db:q("
                alter table mailinglist_scheduled
                add column due timestamp with time zone not null default now()",
                Context)
    end,
    case z_db:column_exists(mailinglist_scheduled, type, Context) of
        true ->
            ok;
        false ->
            [] = z_db:q("
                alter table mailinglist_scheduled
                add column type character varying(20) not null default 'publication',
                add constraint mailinglist_scheduled_type_check
                    check (type in ('date', 'publication'))",
                Context)
    end,
    [] = z_db:q("
        create index if not exists mailinglist_scheduled_type_due_key
        on mailinglist_scheduled (type, due)",
        Context),
    _ = z_db:q("
        update mailinglist_scheduled m
        set due = coalesce(r.publication_start, $1)
        from rsc r
        where m.page_id = r.id
          and m.type = 'publication'
          and m.due is distinct from coalesce(r.publication_start, $1)",
        [?ST_JUTTEMIS],
        Context),
    z_db:flush(Context).

do_install(Context) ->
    z_db:q("
				CREATE TABLE mailinglist_recipient (
					id serial NOT NULL,
					mailinglist_id INT NOT NULL,
					email character varying (200) NOT NULL,
					is_enabled boolean NOT NULL default true,
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
                    props bytea,
                    type character varying(20) NOT NULL DEFAULT 'publication',
                    due timestamp with time zone NOT NULL DEFAULT now(),
                    timestamp timestamp with time zone NOT NULL DEFAULT now(),

					CONSTRAINT mailinglist_scheduled_pkey PRIMARY KEY (page_id, mailinglist_id),
					CONSTRAINT mailinglist_scheduled_type_check CHECK (type IN ('date', 'publication')),
					CONSTRAINT fk_mailinglist_id FOREIGN KEY (mailinglist_id)
				      REFERENCES rsc (id)
				      ON UPDATE CASCADE ON DELETE CASCADE,
					CONSTRAINT fk_page_id FOREIGN KEY (page_id)
				      REFERENCES rsc (id)
				      ON UPDATE CASCADE ON DELETE CASCADE
				)", Context),
    z_db:q("
                CREATE INDEX mailinglist_scheduled_type_due_key
                ON mailinglist_scheduled (type, due)", Context),
    z_db:flush(Context),
    ok.
