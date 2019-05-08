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

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    get/2,
    search_query/2,
    periodic_cleanup/1,
    install/1
]).


-include_lib("zotonic.hrl").


m_find_value(Index, #m{value=undefined}, _Context) ->
    get(Index, _Context).


%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> Value
m_to_list(#m{value=undefined}, Context) ->
    list(Context);
m_to_list(#m{value={log, Id}}, Context) ->
    get(Id, Context);
m_to_list(_, _Context) ->
    [].


m_value(#m{value=#m{value={log, Id}}}, Context) ->
    get(Id, Context).


get(Id, Context) ->
    {ok, R} = z_db:select(log, Id, Context),
    R.


list(Context) ->
    All = z_db:assoc("SELECT * FROM log ORDER BY created DESC", Context),
    [merge_props(R) || R <- All].

merge_props(R) ->
    proplists:delete(props, R) ++ proplists:get_value(props, R, []).


-spec search_query( list(), z:context() ) -> #search_sql{}.
search_query(Args, _Context) ->
    % Filter on log type
    W1 = case z_convert:to_binary( proplists:get_value(type, Args, "warning") ) of
        <<"error">> -> " type = 'error' ";
        <<"info">> -> " type <> 'debug' ";
        <<"debug">> -> "";
        _ -> " type in ('warning', 'error') "
    end,
    As1 = [],
    % Filter on user-id
    {W2, As2} = case proplists:get_value(user, Args) of
        undefined -> {W1, As1};
        "" -> {W1, As1};
        <<>> -> {W1, As1};
        User ->
            try
                WU = case W1 of
                    "" -> " user_id = $" ++ integer_to_list(length(As1) + 1);
                    _ -> W1 ++ " and user_id = $" ++ integer_to_list(length(As1) + 1)
                end,
                {WU, As1 ++ [ z_convert:to_integer(User) ]}
            catch
                _:_ ->
                    {W1, As1}
            end
    end,
    % SQL search question
    #search_sql{
        select = "id",
        from = "log",
        where = W2,
        order = "id DESC",
        args = As2,
        assoc = false
    }.


%% @doc Periodic cleanup of max 10K items older than 3 months
periodic_cleanup(Context) ->
    z_db:q("
        delete from log
        where id in (
            select id
            from log
            where created < now() - interval '3 months'
            limit 10000
        )",
        Context,
        300000).

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

