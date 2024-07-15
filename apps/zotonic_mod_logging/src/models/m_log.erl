%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010-2024 Arjan Scherpenisse <arjan@scherpenisse.net>
%% @doc Model for log messages.
%% @end

%% Copyright 2010-2024 Arjan Scherpenisse
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
    m_get/3,
    get/2,
    search_query/2,
    periodic_cleanup/1,
    install/1
]).


-include_lib("zotonic_core/include/zotonic.hrl").


%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"is_log_client_allowed">> | Rest ], _Msg, Context) ->
    {ok, {mod_logging:is_log_client_allowed(Context), Rest}};
m_get([ <<"is_log_client_session">> | Rest ], _Msg, Context) ->
    case mod_logging:is_log_client_allowed(Context) of
        true -> {ok, {mod_logging:is_log_client_session(Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"is_log_client_active">> | Rest ], _Msg, Context) ->
    case mod_logging:is_log_client_allowed(Context) of
        true -> {ok, {mod_logging:is_log_client_active(Context), Rest}};
        false -> {error, eacces}
    end;
m_get([], _Msg, Context) ->
    case z_acl:is_allowed(use, mod_logging, Context) of
        true -> {ok, {list(Context), []}};
        false -> {error, eacces}
    end;
m_get([ Index | Rest ], _Msg, Context) ->
    case z_acl:is_allowed(use, mod_logging, Context) of
        true -> {ok, {get(z_convert:to_integer(Index), Context), Rest}};
        false -> {error, eacces}
    end;
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.


get(Id, Context) ->
    case z_db:select(log, Id, Context) of
        {ok, R} -> R;
        {error, enoent} -> undefined
    end.


list(Context) ->
    All = z_db:assoc("SELECT * FROM log ORDER BY created DESC", Context),
    [merge_props(R) || R <- All].

merge_props(R) ->
    proplists:delete(props, R) ++ proplists:get_value(props, R, []).


-spec search_query( map(), z:context() ) -> #search_sql{}.
search_query(Query, Context) ->
    Terms = maps:get(<<"q">>, Query, []),
    Args = lists:foldl(
        fun(#{ <<"term">> := Term, <<"value">> := Value }, Acc) ->
            Acc#{
                Term => Value
            }
        end,
        #{},
        Terms),
    % Filter on log type
    W1 = case z_convert:to_binary( maps:get(<<"type">>, Args, <<"warning">>) ) of
        <<"error">> -> " type = 'error' ";
        <<"warning">> -> " type in ('warning', 'error') ";
        <<"notice">> -> " type in ('notice', 'warning', 'error') ";
        <<"info">> -> " type <> 'debug' ";
        <<"debug">> -> "";
        _ -> " type in ('warning', 'error') "
    end,
    As1 = [],
    % Filter on user-id
    {W2, As2} = case maps:get(<<"user">>, Args, undefined) of
        undefined -> {W1, As1};
        "" -> {W1, As1};
        <<>> -> {W1, As1};
        User ->
            try
                WU = case W1 of
                    "" -> " user_id = $" ++ integer_to_list(length(As1) + 1);
                    _ -> W1 ++ " and user_id = $" ++ integer_to_list(length(As1) + 1)
                end,
                {WU, As1 ++ [ m_rsc:rid(User, Context) ]}
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

