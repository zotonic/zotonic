%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%%
%% @doc Model for email log messages.

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

-module(m_log_email).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    install/1
]).


install(Context) ->
    case z_db:table_exists(log_email, Context) of
        true -> 
            ok;
        false ->
            z_db:q("
                create table log_email (
                    id bigserial not null,
                    severity int not null default 1,
                    message_nr character varying(32),
                    mailer_status character varying(8),
                    mailer_message bytea,
                    mailer_host character varying(128),
                    envelop_to character varying(128) not null,
                    envelop_from character varying(128) not null,
                    to_id int,
                    from_id int,
                    content_id int,
                    other_id int,
                    message_template character varying(64),
                    props bytea,
                    created timestamp with time zone not null default now(), 

                    constraint log_email_pkey primary key (id)
                )
            ", Context),
            Indices = [
                       {"log_email_severity", "severity, created"},
                       {"log_email_envelop_to", "envelop_to, created"},
                       {"log_email_to_id", "to_id, created"},
                       {"log_email_from_id", "from_id, created"},
                       {"log_email_content_id", "content_id, created"},
                       {"log_email_other_id", "other_id, created"},
                       {"log_email_created", "created"}
                      ],
            [ z_db:q("create index "++Name++" on log_email ("++Cols++")", Context) || {Name, Cols} <- Indices ]
    end.

