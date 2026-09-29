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
-moduledoc(#{
    zotonic_keywords => ["reference", "backend_developer", "mailing_lists", "database", "maintainability"]
}).
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
    ensure_runs(Context),
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

%% Durable runs replace the disposable page/list schedule. Keep the old table
%% during migration so upgrades are repeatable and do not discard queued work.
ensure_runs(Context) ->
    z_db:q(
        "create table if not exists mailinglist_run (
        id bigserial primary key,
        page_id integer not null references rsc(id) on delete cascade,
        mailinglist_id integer not null references rsc(id) on delete cascade,
        sender_id integer references rsc(id) on delete set null,
        request_key varchar(64) unique,
        parent_id bigint references mailinglist_run(id) on delete set null,
        language varchar(32) not null default '',
        fallback_language varchar(32) not null default 'en',
        audience varchar(20) not null default 'matching',
        send_mode varchar(20) not null default 'new',
        is_test boolean not null default false,
        type varchar(20) not null default 'date',
        due timestamptz not null default now(),
        status varchar(32) not null default 'scheduled',
        prepared boolean not null default false,
        created timestamptz not null default now(),
        started timestamptz,
        finished timestamptz,
        modified timestamptz not null default now(),
        notified timestamptz,
        error text,
        props bytea
    )",
        Context
    ),
    z_db:q(
        "alter table mailinglist_run add column if not exists details_expired timestamptz", Context
    ),
    z_db:q(
        "alter table mailinglist_run add column if not exists first_submitted timestamptz", Context
    ),
    z_db:q(
        "create index if not exists mailinglist_run_retention_key on mailinglist_run(finished) where details_expired is null",
        Context
    ),
    z_db:q(
        "create index if not exists mailinglist_run_due_key
        on mailinglist_run(status, due)",
        Context
    ),
    z_db:q(
        "create index if not exists mailinglist_run_page_key
        on mailinglist_run(page_id, mailinglist_id, created)",
        Context
    ),
    z_db:q(
        "create index if not exists mailinglist_run_list_key
        on mailinglist_run(mailinglist_id,created)",
        Context
    ),
    z_db:q(
        "create table if not exists mailinglist_run_content (
        run_id bigint not null references mailinglist_run(id) on delete cascade,
        language varchar(32) not null,
        html text not null,
        created timestamptz not null default now(),
        primary key(run_id,language)
    )",
        Context
    ),
    z_db:q(
        "create table if not exists mailinglist_run_recipient (
        id bigserial primary key,
        run_id bigint not null references mailinglist_run(id) on delete cascade,
        email varchar(200) not null,
        recipient_id integer references rsc(id) on delete set null,
        language varchar(32) not null,
        status varchar(32) not null default 'pending',
        reason text,
        modified timestamptz not null default now(),
        unique(run_id, email)
    )",
        Context
    ),
    z_db:q(
        "create index if not exists mailinglist_run_recipient_status_key
        on mailinglist_run_recipient(run_id, status)",
        Context
    ),
    z_db:q(
        "create index if not exists mailinglist_run_recipient_email_key
        on mailinglist_run_recipient(email, language, run_id)",
        Context
    ),
    z_db:q(
        "create table if not exists mailinglist_run_message (
        message_nr varchar(100) primary key,
        recipient_id bigint not null references mailinglist_run_recipient(id) on delete cascade,
        status varchar(32) not null default 'submitting',
        retry_count integer not null default 0,
        is_final boolean not null default false,
        detail text,
        created timestamptz not null default now(),
        modified timestamptz not null default now()
    )",
        Context
    ),
    z_db:q(
        "create index if not exists mailinglist_run_message_recipient_key
        on mailinglist_run_message(recipient_id)",
        Context
    ),
    z_db:q(
        "create table if not exists mailinglist_run_stats (
        run_id bigint not null references mailinglist_run(id) on delete cascade,
        language varchar(32) not null,
        status varchar(32) not null,
        total integer not null default 0 check (total >= 0),
        primary key(run_id, language, status)
    )",
        Context
    ),
    z_db:flush(Context),
    ok = z_db:transaction(
        fun(Ctx) ->
            Rows = z_db:assoc_props("select * from mailinglist_scheduled for update", Ctx),
            lists:foreach(
                fun(Row) ->
                    m_mailinglist_run:import_scheduled(Row, Ctx)
                end,
                Rows
            ),
            z_db:q("delete from mailinglist_scheduled", Ctx),
            ok
        end,
        Context
    ),
    ok.
