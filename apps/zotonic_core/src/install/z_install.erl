%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2021 Marc Worrell
%%
%% @doc Install Zotonic, loads the datamodel into the database
%% Assumes the database has already been created (which normally needs superuser permissions anyway)
%%
%% CREATE DATABASE zotonic WITH OWNER = zotonic ENCODING = 'UTF8';
%% CREATE LANGUAGE "plpgsql";

%% Copyright 2009-2021 Marc Worrell
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

-module(z_install).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
         install/1,

         hierarchy_table/0,
         hierarchy_index_1/0,
         hierarchy_index_2/0,

         medium_log_table/0,
         medium_update_function/0,
         medium_update_trigger/0,

         edge_log_table/0,
         edge_log_function/0,
         edge_log_trigger/0,

         rsc_page_path_log/0,
         rsc_page_path_log_fki/0
        ]).

-include_lib("zotonic.hrl").

%% @doc Install the core database tables, triggers and data for the given site.
-spec install( z:context() ) -> ok.
install(Context) ->
    ok = z_db:transaction(
        fun(Context1) ->
            ok = install_sql_list(Context1, model_pgsql()),
            ok = z_install_data:install(z_context:site(Context1), Context1)
        end,
        Context).


-spec install_sql_list(#context{}, list()) -> ok.
install_sql_list(Context, Model) ->
    C = z_db_pgsql:get_raw_connection(Context),
    [ {ok, [], []} = epgsql:squery(C, Sql) || Sql <- Model ],
    ok.


%% @doc Return a list containing the SQL statements to build the database model
model_pgsql() ->
    [

    % Table config
    % Holds all configuration keys
    "CREATE TABLE config
    (
      id serial NOT NULL,
      module character varying(80) NOT NULL DEFAULT 'zotonic'::character varying,
      key character varying(80) NOT NULL DEFAULT ''::character varying,
      value text NOT NULL DEFAULT ''::character varying,
      props bytea,
      created timestamp with time zone NOT NULL DEFAULT now(),
      modified timestamp with time zone NOT NULL DEFAULT now(),

      CONSTRAINT config_pkey PRIMARY KEY (id),
      CONSTRAINT config_module_key_key UNIQUE (module, key)
    )",


    % Table module
    % Holds install state of all known modules

    "CREATE TABLE module
    (
      id serial NOT NULL,
      name character varying(80) NOT NULL DEFAULT ''::character varying,
      uri character varying(2048) NOT NULL DEFAULT ''::character varying,
      is_active boolean NOT NULL DEFAULT false,
      created timestamp with time zone NOT NULL DEFAULT now(),
      modified timestamp with time zone NOT NULL DEFAULT now(),
      schema_version int NULL,

      CONSTRAINT module_pkey PRIMARY KEY (id),
      CONSTRAINT module_name_key UNIQUE (name)
    )",


    % Table: rsc
    % Holds all resources (posts, persons etc.)
    "CREATE TABLE rsc
    (
      id serial NOT NULL,
      uri character varying(2048),
      name character varying(80),
      page_path character varying(80),
      is_authoritative boolean NOT NULL DEFAULT true,
      is_published boolean NOT NULL DEFAULT false,
      is_featured boolean NOT NULL DEFAULT false,
      is_protected boolean NOT NULL DEFAULT false,
      is_dependent boolean NOT NULL DEFAULT false,
      publication_start timestamp with time zone,
      publication_end timestamp with time zone NOT NULL DEFAULT '9999-06-01 00:00:00'::timestamp with time zone,
      content_group_id int,
      creator_id int,
      modifier_id int,
      version int NOT NULL DEFAULT 1,
      category_id int NOT NULL,
      visible_for int NOT NULL DEFAULT 0, -- 0 = public, > 1 defined by ACL module
      language character varying(16)[],
      slug character varying(80) NOT NULL DEFAULT ''::character varying,
      props bytea,
      created timestamp with time zone NOT NULL DEFAULT now(),
      modified timestamp with time zone NOT NULL DEFAULT now(),

      -- pivot fields for searching
      pivot_category_nr int,
      pivot_tsv tsvector,       -- texts
      pivot_rtsv tsvector,      -- related ids (cat, prop, rsc)

      pivot_first_name character varying(100),
      pivot_surname character varying(100),
      pivot_gender character varying(1),

      pivot_date_start timestamp with time zone,
      pivot_date_end timestamp with time zone,
      pivot_date_start_month_day int,  -- used for birthdays
      pivot_date_end_month_day int,    -- used for decease dates

      pivot_street character varying(120),
      pivot_city character varying(100),
      pivot_state character varying(50),
      pivot_postcode character varying(30),
      pivot_country character varying(80),
      pivot_geocode bigint,
      pivot_geocode_qhash bytea,
      pivot_title character varying(100),

      pivot_location_lat float,
      pivot_location_lng float,

      CONSTRAINT rsc_pkey PRIMARY KEY (id),
      CONSTRAINT rsc_uri_key UNIQUE (uri),
      CONSTRAINT rsc_name_key UNIQUE (name),
      CONSTRAINT rsc_page_path_key UNIQUE (page_path)
    )",

     "ALTER TABLE rsc ADD CONSTRAINT fk_rsc_content_group_id FOREIGN KEY (content_group_id)
      REFERENCES rsc (id)
      ON UPDATE CASCADE ON DELETE SET NULL",
     "ALTER TABLE rsc ADD CONSTRAINT fk_rsc_creator_id FOREIGN KEY (creator_id)
      REFERENCES rsc (id)
      ON UPDATE CASCADE ON DELETE SET NULL",
     "ALTER TABLE rsc ADD CONSTRAINT fk_rsc_modifier_id FOREIGN KEY (modifier_id)
      REFERENCES rsc (id)
      ON UPDATE CASCADE ON DELETE SET NULL",
     "ALTER TABLE rsc ADD CONSTRAINT fk_rsc_category_id FOREIGN KEY (category_id)
      REFERENCES rsc (id)
      ON UPDATE CASCADE ON DELETE RESTRICT",

     "CREATE INDEX fki_rsc_content_group_id ON rsc (content_group_id)",
     "CREATE INDEX fki_rsc_creator_id ON rsc (creator_id)",
     "CREATE INDEX fki_rsc_modifier_id ON rsc (modifier_id)",
     "CREATE INDEX fki_rsc_category_id ON rsc (category_id)",

     "CREATE INDEX rsc_language_key ON rsc USING gin(language)",
     "CREATE INDEX rsc_pivot_tsv_key ON rsc USING gin(pivot_tsv)",
     "CREATE INDEX rsc_pivot_rtsv_key ON rsc USING gin(pivot_rtsv)",

     "CREATE INDEX rsc_pivot_category_nr ON rsc (pivot_category_nr)",
     "CREATE INDEX rsc_pivot_surname_key ON rsc (pivot_surname)",
     "CREATE INDEX rsc_pivot_first_name_key ON rsc (pivot_first_name)",
     "CREATE INDEX rsc_pivot_date_start_month_day_key ON rsc (pivot_date_start_month_day)",
     "CREATE INDEX rsc_pivot_date_end_month_day_key ON rsc (pivot_date_end_month_day)",
     "CREATE INDEX rsc_pivot_city_street_key ON rsc (pivot_city, pivot_street)",
     "CREATE INDEX rsc_pivot_country_key ON rsc (pivot_country)",
     "CREATE INDEX rsc_pivot_postcode_key ON rsc (pivot_postcode)",
     "CREATE INDEX rsc_pivot_geocode_key ON rsc (pivot_geocode)",
     "CREATE INDEX rsc_pivot_title_key ON rsc (pivot_title)",
     "CREATE INDEX rsc_pivot_location_key ON rsc (pivot_location_lat, pivot_location_lng)",
     "CREATE INDEX rsc_modified_category_nr_key ON rsc (modified, pivot_category_nr)",
     "CREATE INDEX rsc_created_category_nr_key ON rsc (created, pivot_category_nr)",
     "CREATE INDEX rsc_pivot_date_start_category_nr_key ON rsc (pivot_date_start, pivot_category_nr)",
     "CREATE INDEX rsc_pivot_date_end_category_nr_key ON rsc (pivot_date_end, pivot_category_nr)",
     "CREATE INDEX rsc_publication_start_category_nr_key ON rsc (publication_start, pivot_category_nr)",
     "CREATE INDEX rsc_publication_end_category_nr_key ON rsc (publication_end, pivot_category_nr)",


    % Table: rsc_gone
    % Tracks deleted or moved resources, adding "410 gone" support
    % Also contains new id or new url for 301 moved permanently replies.
    % mod_backup is needed to recover a deleted resource's content.

    "CREATE TABLE rsc_gone
     (
        id bigint not null,
        new_id bigint,
        new_uri character varying(2048),
        version int not null,
        uri character varying(2048),
        name character varying(80),
        page_path character varying(80),
        is_authoritative boolean NOT NULL DEFAULT true,
        creator_id bigint,
        modifier_id bigint,
        created timestamp with time zone NOT NULL DEFAULT now(),
        modified timestamp with time zone NOT NULL DEFAULT now(),
        CONSTRAINT rsc_gone_pkey PRIMARY KEY (id)
     )",
    "CREATE INDEX rsc_gone_name ON rsc_gone(name)",
    "CREATE INDEX rsc_gone_page_path ON rsc_gone(page_path)",
    "CREATE INDEX rsc_gone_modified ON rsc_gone(modified)",


    % Table: protect
    % By making an entry in this table we protect a rsc from being deleted.
    % This table is maintained by the update/insert trigger.

    "CREATE TABLE protect
    (
        id int NOT NULL,

        CONSTRAINT protect_id PRIMARY KEY (id),
        CONSTRAINT fk_protect_id FOREIGN KEY (id)
          REFERENCES rsc(id)
          ON UPDATE CASCADE ON DELETE RESTRICT
    )",

    % Table: edge
    % All relations between resources, forming a directed graph

    "CREATE TABLE edge
    (
      id serial NOT NULL,
      subject_id int NOT NULL,
      predicate_id int NOT NULL,
      object_id int NOT NULL,
      seq int NOT NULL DEFAULT 1000000,
      creator_id int,
      created timestamp with time zone NOT NULL DEFAULT now(),

      CONSTRAINT edge_pkey PRIMARY KEY (id),
      CONSTRAINT edge_ops_key UNIQUE (object_id, predicate_id, subject_id),
      CONSTRAINT edge_spo_key UNIQUE (subject_id, predicate_id, object_id),
      CONSTRAINT fk_edge_subject_id FOREIGN KEY (subject_id)
        REFERENCES rsc (id)
        ON UPDATE CASCADE ON DELETE CASCADE,
      CONSTRAINT fk_edge_object_id FOREIGN KEY (object_id)
        REFERENCES rsc (id)
        ON UPDATE CASCADE ON DELETE CASCADE,
      CONSTRAINT fk_edge_predicate_id FOREIGN KEY (predicate_id)
        REFERENCES rsc (id)
        ON UPDATE CASCADE ON DELETE CASCADE,
      CONSTRAINT fk_edge_creator_id FOREIGN KEY (creator_id)
        REFERENCES rsc (id)
        ON UPDATE CASCADE ON DELETE SET NULL
    )",

    "CREATE INDEX fki_edge_subject_id ON edge (subject_id)",
    "CREATE INDEX fki_edge_predicate_id ON edge (predicate_id)",
    "CREATE INDEX fki_edge_object_id ON edge (object_id)",
    "CREATE INDEX fki_edge_creator_id ON edge (creator_id)",
    "CREATE INDEX edge_sp_seq_key ON edge (subject_id, predicate_id, seq)",


    % Table medium
    % Holds all references to media (files), used in the context of resources
    % Every medium is a resource.
    % The preview might have been generated from the original file and is always a jpeg.

    "CREATE TABLE medium
    (
      id int NOT NULL,
      filename character varying(400),
      rootname character varying(100),
      mime character varying(128) NOT NULL DEFAULT 'application/octet-stream'::character varying,
      width int NOT NULL DEFAULT 0,
      height int NOT NULL DEFAULT 0,
      orientation int NOT NULL DEFAULT 1,
      sha1 character varying(40),
      size int NOT NULL DEFAULT 0,
      preview_filename character varying(400),
      preview_width int NOT NULL DEFAULT 0,
      preview_height int NOT NULL DEFAULT 0,
      is_deletable_file boolean NOT NULL DEFAULT false,
      is_deletable_preview boolean NOT NULL DEFAULT false,
      props bytea,
      created timestamp with time zone NOT NULL DEFAULT now(),

      CONSTRAINT medium_pkey PRIMARY KEY (id),
      CONSTRAINT medium_filename_key UNIQUE (filename),
      CONSTRAINT fk_medium_rsc_id FOREIGN KEY (id)
        REFERENCES rsc (id)
        ON UPDATE CASCADE ON DELETE CASCADE
    )",

    "CREATE INDEX medium_rootname_key ON medium (rootname)",

    % Table: predicate_category
    % Defines which categories are valid for a predicate as subject or object

    "CREATE TABLE predicate_category
    (
      id serial NOT NULL,
      is_subject boolean NOT NULL DEFAULT true,
      predicate_id int NOT NULL,
      category_id int NOT NULL,

      CONSTRAINT predicate_category_pkey PRIMARY KEY (id),
      CONSTRAINT predicate_category_key UNIQUE (predicate_id, is_subject, category_id),
      CONSTRAINT fk_predicate_category_predicate_id FOREIGN KEY (predicate_id)
        REFERENCES rsc(id)
        ON UPDATE CASCADE
        ON DELETE CASCADE,
      CONSTRAINT fk_predicate_category_category_id FOREIGN KEY (category_id)
        REFERENCES rsc(id)
        ON UPDATE CASCADE
        ON DELETE CASCADE
    )",

    "CREATE INDEX fki_predicate_category_predicate_id ON predicate_category (predicate_id)",
    "CREATE INDEX fki_predicate_category_category_id ON predicate_category (category_id)",

    % Table identity
    % Identities of an user, used for authentication.  Examples are password, openid, msn, xmpp etc.

    "CREATE TABLE identity
    (
      id serial NOT NULL,
      rsc_id int NOT NULL,
      type character varying(32) NOT NULL DEFAULT ''::character varying,
      key character varying(200) NOT NULL DEFAULT ''::character varying,
      is_unique boolean NOT NULL DEFAULT false,          -- set to true when the type/key should be unique
      is_verified boolean not null default false,
      verify_key character varying(32),
      propb bytea,
      prop1 character varying(200) NOT NULL DEFAULT ''::character varying,
      prop2 character varying(200) NOT NULL DEFAULT ''::character varying,
      prop3 character varying(200) NOT NULL DEFAULT ''::character varying,
      created timestamp with time zone NOT NULL DEFAULT now(),
      modified timestamp with time zone NOT NULL DEFAULT now(),
      visited timestamp with time zone,

      CONSTRAINT auth_pkey PRIMARY KEY (id),
      CONSTRAINT pk_auth_rsc_id FOREIGN KEY (rsc_id)
        REFERENCES rsc (id)
        ON UPDATE CASCADE ON DELETE CASCADE,
      CONSTRAINT identity_verify_key_unique UNIQUE (verify_key)
    )",

    "CREATE INDEX fki_identity_rsc_id ON identity (rsc_id)",
    "CREATE INDEX identity_visited_key ON identity (visited)",
    "CREATE INDEX identity_created_key ON identity (created)",
    "CREATE UNIQUE INDEX identity_type_key_unique ON identity (type, key) WHERE (is_unique)",
    "CREATE INDEX identity_type_key_key ON identity using btree (type, key collate ucs_basic text_pattern_ops)",

    % Email send queue and log
    "CREATE TABLE emailq
    (
        id serial NOT NULL,
        status character varying(10) not null default 'new', -- new, sent, fail
        retry_on timestamp with time zone default (now() + '00:10:00'::interval),
        retry int not null default 0,
        sender character varying(100),
        recipient character varying(100),
        props bytea,
        sent timestamp with time zone,
        created timestamp with time zone not null default now(),
        CONSTRAINT email_pkey PRIMARY KEY (id)
    )",

    "CREATE INDEX email_recipient_key ON emailq (recipient)",
    "CREATE INDEX email_created_key ON emailq (created)",
    "CREATE INDEX email_status_retry_key ON emailq (status, retry_on)",

    % pivot queue for rsc, all things that are updated are queued here for later full text indexing
    "CREATE TABLE rsc_pivot_queue
    (
        rsc_id int NOT NULL,
        serial int NOT NULL DEFAULT 1,
        due timestamp with time zone,
        is_update boolean NOT NULL default true,

        CONSTRAINT rsc_pivot_queue_pkey PRIMARY KEY (rsc_id),
        CONSTRAINT fk_rsc_pivot_queue_rsc_id FOREIGN KEY (rsc_id)
          REFERENCES rsc(id)
          ON UPDATE CASCADE ON DELETE CASCADE
    )",

    "CREATE INDEX fki_rsc_pivot_queue_rsc_id ON rsc_pivot_queue (rsc_id)",
    "CREATE INDEX fki_rsc_pivot_queue_due ON rsc_pivot_queue (is_update, due)",

    % queue for slow pivoting queries, for example syncing category nrs after the categories are changed.
    "CREATE TABLE pivot_task_queue
    (
        id serial NOT NULL,
        module character varying(80) NOT NULL,
        function character varying(64) NOT NULL,
        key character varying(100) NOT NULL DEFAULT ''::character varying,
        due timestamp with time zone ,
        props bytea,

        CONSTRAINT pivot_task_queue_pkey PRIMARY KEY (id),
        CONSTRAINT pivot_task_queue_module_funcion_key_key UNIQUE (module, function, key)
    )
    ",

    % Update/insert trigger on rsc to fill the update queue
    % The text indexing is delayed until the updates are stable
    % Also checks if the rsc is set to protected, if so makes an entry in the 'protect' table.
    "
    CREATE FUNCTION rsc_pivot_update() RETURNS trigger AS $$
    declare
        duetime timestamp;
        do_queue boolean;
    begin
        if (tg_op = 'INSERT') then
            do_queue := true;
        elseif (new.version <> old.version or new.modified <> old.modified) then
            do_queue := true;
        else
            do_queue := false;
        end if;

        if (do_queue) then
            <<insert_update_queue>>
            loop
                update rsc_pivot_queue
                set due = (case when now() < due then now() else due end),
                    serial = serial + 1
                where rsc_id = new.id;

                exit insert_update_queue when found;

                begin
                    insert into rsc_pivot_queue (rsc_id, due, is_update) values (new.id, now(), tg_op = 'UPDATE');
                    exit insert_update_queue;
                exception
                    when unique_violation then
                        -- do nothing
                end;
            end loop insert_update_queue;
        end if;

        if (new.is_protected) then
            begin
                insert into protect (id) values (new.id);
            exception
                when unique_violation then
                    -- do nothing
            end;
        else
            delete from protect where id = new.id;
        end if;
        return null;
    end;
    $$ LANGUAGE plpgsql
    ",
    "
    CREATE TRIGGER rsc_update_queue_trigger AFTER INSERT OR UPDATE
    ON rsc FOR EACH ROW EXECUTE PROCEDURE rsc_pivot_update()
    ",

    % Queue for deleted medium files, periodically checked for deleting files that are not referenced anymore
    "CREATE TABLE medium_deleted
    (
        id serial NOT NULL,
        filename character varying (400) NOT NULL,
        deleted timestamp with time zone NOT NULL default now(),

        CONSTRAINT medium_deleted_pkey PRIMARY KEY (id)
    )",

    "CREATE INDEX medium_deleted_deleted_key ON medium_deleted (deleted)",

    % Update/insert trigger on medium to fill the deleted files queue
    "
    CREATE FUNCTION medium_delete() RETURNS trigger AS $$
    begin
        if (tg_op = 'DELETE') then
            if (old.filename <> '' and old.filename is not null and old.is_deletable_file) then
                insert into medium_deleted (filename) values (old.filename);
            end if;
            if (old.preview_filename <> '' and old.preview_filename is not null and old.is_deletable_preview) then
                insert into medium_deleted (filename) values (old.preview_filename);
            end if;
        end if;
        return null;
    end;
    $$ LANGUAGE plpgsql
    ",
    "
    CREATE TRIGGER medium_deleted_trigger AFTER DELETE
    ON medium FOR EACH ROW EXECUTE PROCEDURE medium_delete()
    ",

    %% Table with hierarchies for menus and the category tree
    hierarchy_table(),
    hierarchy_index_1(),
    hierarchy_index_2(),

    % Table with all uploaded filenames, used to ensure unique filenames in the upload archive
    medium_log_table(),

    % Update/insert trigger on medium to fill the deleted files queue
    medium_update_function(),
    medium_update_trigger(),

    %% Holds administration of previous page paths
    rsc_page_path_log(),
    rsc_page_path_log_fki(),

    %% Track deletion/insert/changes of edges
    edge_log_table(),
    edge_log_function(),
    edge_log_trigger()
    ].


hierarchy_table() ->
    "CREATE TABLE hierarchy (
        name character varying (80),
        id int NOT NULL,
        parent_id int,
        nr int NOT NULL DEFAULT 0,
        lvl int NOT NULL DEFAULT 0,
        lft int NOT NULL DEFAULT 0,
        rght int NOT NULL DEFAULT 0,

        CONSTRAINT hierarchy_pkey PRIMARY KEY (name, id),
        CONSTRAINT fk_hierarchy_id FOREIGN KEY (id)
          REFERENCES rsc(id)
          ON UPDATE CASCADE ON DELETE CASCADE
          DEFERRABLE INITIALLY DEFERRED
    )".

hierarchy_index_1() ->
    "CREATE INDEX hierarchy_nr_key ON hierarchy (name, nr)".

hierarchy_index_2() ->
    "CREATE INDEX fki_hierarchy_id ON hierarchy (id)".


edge_log_table() ->
    "CREATE TABLE edge_log
    (
        id bigserial NOT NULL,
        op character varying(6),
        edge_id int not null,
        subject_id int not null,
        predicate_id int not null,
        predicate character varying (80),
        object_id int not null,
        seq integer not null,
        logged timestamp with time zone NOT NULL default now(),
        created timestamp with time zone,

        CONSTRAINT edge_log_pkey PRIMARY KEY (id)
    )".

edge_log_function() ->
    "
    CREATE OR REPLACE FUNCTION edge_update() RETURNS trigger AS $$
    declare
        new_predicate character varying(80);
    begin
        if (tg_op = 'INSERT') then
            select into new_predicate r.name from rsc r where r.id = new.predicate_id;
            insert into edge_log (op, edge_id, subject_id, object_id, predicate_id, predicate, seq)
            values (tg_op, new.id, new.subject_id, new.object_id, new.predicate_id, new_predicate, new.seq);
        elseif (tg_op = 'UPDATE') then
            select into new_predicate r.name from rsc r where r.id = new.predicate_id;
            insert into edge_log (op, edge_id, subject_id, object_id, predicate_id, predicate, seq)
            values (tg_op, new.id, new.subject_id, new.object_id, new.predicate_id, new_predicate, new.seq);
        elseif (tg_op = 'DELETE') then
            select into new_predicate r.name from rsc r where r.id = old.predicate_id;
            insert into edge_log (op, edge_id, subject_id, object_id, predicate_id, predicate, seq, created)
            values (tg_op, old.id, old.subject_id, old.object_id, old.predicate_id, new_predicate, old.seq, old.created);
        end if;
        return null;
    end;
    $$ LANGUAGE plpgsql
    ".


edge_log_trigger() ->
    "
    CREATE TRIGGER edge_update_trigger AFTER INSERT OR UPDATE OR DELETE
    ON edge FOR EACH ROW EXECUTE PROCEDURE edge_update()
    ".


medium_log_table() ->
    "CREATE TABLE medium_log
    (
        id serial NOT NULL,
        usr_id int,
        filename character varying (400) NOT NULL,
        created timestamp with time zone NOT NULL default now(),

        CONSTRAINT medium_log_pkey PRIMARY KEY (id),
        CONSTRAINT medium_log_filename_key UNIQUE (filename)
    )".


medium_update_function() ->
    "
    CREATE FUNCTION medium_update() RETURNS trigger AS $$
    declare
        user_id integer;
    begin
        select into user_id r.creator_id from rsc r where r.id = new.id;
        if (tg_op = 'INSERT') then
            if (new.filename <> '' and new.filename is not null and new.is_deletable_file) then
                insert into medium_log (filename, usr_id)
                values (new.filename, user_id);
            end if;
            if (new.preview_filename <> '' and new.preview_filename is not null and new.is_deletable_preview) then
                insert into medium_log (filename, usr_id)
                values (new.preview_filename, user_id);
            end if;
        elseif (tg_op = 'UPDATE') then
            if (new.filename <> '' and new.filename is not null and new.is_deletable_file and new.filename != old.filename) then
                insert into medium_log (filename, usr_id)
                values (new.filename, user_id);
            end if;
            if (new.preview_filename <> '' and new.preview_filename is not null and new.is_deletable_preview and new.preview_filename != old.preview_filename) then
                insert into medium_log (filename, usr_id)
                values (new.preview_filename, user_id);
            end if;
            -- Insert files into the medium_deleted queue table
            if (old.filename <> '' and old.filename is not null and old.is_deletable_file and new.filename != old.filename) then
                insert into medium_deleted (filename) values (old.filename);
            end if;
            if (old.preview_filename <> '' and old.preview_filename is not null and old.is_deletable_preview and new.preview_filename != old.preview_filename) then
                insert into medium_deleted (filename) values (old.preview_filename);
            end if;
        end if;
        return null;
    end;
    $$ LANGUAGE plpgsql
    ".

medium_update_trigger() ->
    "
    CREATE TRIGGER medium_update_trigger AFTER INSERT OR UPDATE
    ON medium FOR EACH ROW EXECUTE PROCEDURE medium_update()
    ".

rsc_page_path_log() ->
   "CREATE TABLE rsc_page_path_log (
      page_path character varying(80),
      id int not null,
      created timestamp with time zone NOT NULL DEFAULT now(),
      CONSTRAINT rsc_page_path_log_pkey PRIMARY KEY (page_path),
      CONSTRAINT fk_rsc_page_path_log_id FOREIGN KEY (id)
        REFERENCES rsc(id)
        ON UPDATE CASCADE ON DELETE CASCADE
    )".

rsc_page_path_log_fki() ->
    "CREATE INDEX fki_rsc_page_path_log_id ON rsc_page_path_log (id)".

