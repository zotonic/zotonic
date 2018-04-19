%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2010-06-01
%%
%% @doc Model for log messages.

%% Copyright 2010 Arjan Scherpenisse
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

-module(m_log).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/2,
    get/2,
    install/1
]).


-include_lib("zotonic_core/include/zotonic.hrl").


%% @doc Fetch the value for the key from a model source
-spec m_get( list(), z:context() ) -> {term(), list()}.
m_get([], Context) ->
    {list(Context), []};
m_get([ Index | Rest ], Context) ->
    {get(Index, Context), Rest};
m_get(Vs, _Context) ->
    lager:error("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {undefined, []}.


get(Id, Context) ->
    {ok, R} = z_db:select(log, Id, Context),
    R.


list(Context) ->
    All = z_db:assoc("SELECT * FROM log ORDER BY created DESC", Context),
    [merge_props(R) || R <- All].

merge_props(R) ->
    proplists:delete(props, R) ++ proplists:get_value(props, R, []).


install(Context) ->
    case z_db:table_exists(log, Context) of
        true -> ok;
        false ->
            z_db:q("
                create table log (
                    id bigserial not null,
                    rsc_id int,
                    user_id int,
                    type character varying(80) not null default ''::character varying,
                    module character varying(160) not null default ''::character varying,
                    props bytea,
                    created timestamp with time zone not null default now(),

                    constraint log_pkey primary key (id),
                    constraint fk_log_rsc_id foreign key (rsc_id)
                        references rsc(id)
                        on delete set null on update cascade,
                    constraint fk_log_user_id foreign key (user_id)
                        references rsc(id)
                        on delete set null on update cascade
                )
            ", Context),
            Indices = [
                       {"fki_log_rsc_id", "rsc_id"},
                       {"fki_log_user_id", "user_id"},
                       {"log_module_created_key", "module, created"},
                       {"log_type_created_key", "type, created"},
                       {"log_created_key", "created"}
                      ],
            [ z_db:q("create index "++Name++" on log ("++Cols++")", Context) || {Name, Cols} <- Indices ]
    end.

