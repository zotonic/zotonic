%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% Date: 2009-10-02
%% @doc OAuth.

%% Copyright 2009 Arjan Scherpenisse
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

-module(oauth_install_data).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([install/1]).

install(Context) ->
    z_acl:sudo(fun(Ctx) -> install1(Ctx) end, Context).

install1(Context) ->
    F = fun(Ctx) ->
                ok = install_tables(Ctx)
        end,
    ok = z_db:transaction(F, Context),
    z_depcache:flush(Context),
    ok.
    


install_tables(Context) ->
    [ [] = z_db:q(Sql, Context) || Sql <- tables_sql() ],
    ok.


tables_sql() ->
    [
     
     %%  ////////////////// SERVER SIDE /////////////////

     %% Table holding consumer key/secret combos an user issued to consumers. 
     %% Used for verification of incoming requests.
     "
    CREATE TABLE oauth_application_registry 
    (
    id                      serial NOT NULL,
    user_id                 integer,
    consumer_key            varchar(64) not null,
    consumer_secret         varchar(64) not null,
    enabled                 boolean not null default true,
    callback_uri            varchar(255) not null,
    application_uri         varchar(255) not null,
    application_title       varchar(80) not null,
    application_descr       text not null,
    application_notes       text not null,
    application_type        varchar(20) not null,
    timestamp               timestamp with time zone NOT NULL default now(),

    CONSTRAINT oauth_application_registry_pkey PRIMARY KEY (id),
    CONSTRAINT oauth_application_registry_ckey UNIQUE (consumer_key),

    CONSTRAINT oauth_application_registry_user FOREIGN KEY (user_id)
        REFERENCES rsc(id)
        ON UPDATE CASCADE ON DELETE CASCADE
    )",


     %% Nonce used by a certain consumer, every used nonce should be unique, this prevents
     %% replaying attacks.  We need to store all timestamp/nonce combinations for the
     %% maximum timestamp received.

     "
    CREATE TABLE oauth_nonce 
    (
    id                  serial NOT NULL,
    consumer_key        varchar(64) not null,
    token               varchar(64) not null,
    timestamp           timestamp with time zone NOT NULL default now(),
    nonce               varchar(80) not null,

    CONSTRAINT oauth_nonce_pkey PRIMARY KEY (id),
    CONSTRAINT oauth_nonce_unique UNIQUE (consumer_key, token, timestamp, nonce)
    )",

     %% Table used to verify signed requests sent to a server by the consumer
     %% When the verification is succesful then the associated user id is returned.

     "CREATE TYPE oauth_token_type AS ENUM('request', 'access')",

     "
    CREATE TABLE oauth_application_token 
    (
    id                  serial NOT NULL,
    application_id      integer not null,
    user_id             integer not null,
    token               varchar(64) not null,
    token_secret        varchar(64) not null,
    token_type          oauth_token_type,
    authorized          boolean not null default false,
	callback_uri		varchar(255) not null,
    token_ttl           timestamp with time zone NOT NULL default '9999-12-31'::timestamp,
    timestamp           timestamp with time zone NOT NULL default now(),

	CONSTRAINT oauth_application_token_pkey PRIMARY KEY (id),
    CONSTRAINT oauth_application_token_token UNIQUE (token),

	CONSTRAINT oauth_application_appid 
    FOREIGN KEY (application_id) REFERENCES oauth_application_registry (id)
        ON UPDATE CASCADE
        ON DELETE CASCADE,

    CONSTRAINT oauth_application_token_user FOREIGN KEY (user_id)
        REFERENCES rsc(id)
        ON UPDATE CASCADE ON DELETE CASCADE
    )
    ",

     "
    CREATE TABLE oauth_application_perm 
    (
    application_id      integer not null,
    perm                varchar(64) not null,

	CONSTRAINT oauth_application_perm_pkey PRIMARY KEY (application_id, perm),

	CONSTRAINT oauth_application_perm_appid 
    FOREIGN KEY (application_id) REFERENCES oauth_application_registry (id)
        ON UPDATE CASCADE
        ON DELETE CASCADE
    )
    "

    ].
