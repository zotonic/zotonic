%% @author Marc Worrell
%% @copyright 2019 Marc Worrell
%% @doc Model for ui log messages.

%% Copyright 2019 Marc Worrell
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

-module(m_log_ui).
-author("Marc Worrell").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    insert_event/2,
    insert/2,
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


m_value(#m{value=#m{value={log_ui, Id}}}, Context) ->
    get(Id, Context).


get(Id, Context) ->
    {ok, R} = z_db:select(log_ui, Id, Context),
    R.

insert(Props, Context) ->
    z_db:insert(log_ui, Props, Context).

insert_event(Props, Context) when is_list(Props) ->
    UserId = z_acl:user(Context),
    Props1 = lists:filter( fun filter_prop/1, Props ),
    Props2 = lists:map( fun map_prop/1, Props1 ),
    Type = case proplists:get_value(type, Props2) of
        undefined -> <<"error">>;
        T -> T
    end,
    Message = [
        {user_id, UserId},
        {type, Type}
    ] ++ proplists:delete(type, Props2),
    MsgUserProps = maybe_add_user_props(Message, Context),
    z_db:insert(log_ui, MsgUserProps, Context).

maybe_add_user_props(Props, Context) ->
    case proplists:get_value(user_id, Props) of
        undefined ->
            Props;
        UserId ->
            [
                {user_name_first, m_rsc:p_no_acl(UserId, name_first, Context)},
                {user_name_surname, m_rsc:p_no_acl(UserId, name_surname, Context)},
                {user_email_raw, m_rsc:p_no_acl(UserId, email_raw, Context)}
                | Props
            ]
    end.


filter_prop({_, V}) when not is_binary(V), not is_number(V) -> false;
filter_prop({<<"type">>, _}) -> true;
filter_prop({<<"message">>, _}) -> true;
filter_prop({<<"file">>, _}) -> true;
filter_prop({<<"line">>, _}) -> true;
filter_prop({<<"col">>, _}) -> true;
filter_prop({<<"stack">>, _}) -> true;
filter_prop({<<"user_agent">>, _}) -> true;
filter_prop({<<"url">>, _}) -> true;
filter_prop(_) -> false.

map_prop({K, null}) -> map_prop({K, undefined});
map_prop({<<"type">>, T}) ->
    T1 = case T of
        <<"fatal">> -> <<"fatal">>;
        <<"error">> -> <<"error">>;
        <<"warning">> -> <<"warning">>;
        <<"info">> -> <<"info">>;
        <<"debug">> -> <<"debug">>;
        _ -> <<"error">>
    end,
    {type, T1};
map_prop({<<"message">>, M}) when is_binary(M) -> {message, z_string:truncate(M, 500)};
map_prop({<<"stack">>, M}) when is_binary(M) -> {stack, z_string:truncate(M, 1000)};
map_prop({<<"file">>, M}) when is_binary(M) -> {file, z_string:truncate(M, 500)};
map_prop({<<"line">>, M}) -> {line, z_convert:to_integer(M)};
map_prop({<<"col">>, M}) -> {col, z_convert:to_integer(M)};
map_prop({<<"user_agent">>, M}) when is_binary(M) -> {user_agent, z_string:truncate(M, 500)};
map_prop({<<"url">>, M}) when is_binary(M) -> {url, z_string:truncate(M, 500)}.


list(Context) ->
    All = z_db:assoc("SELECT * FROM log_ui ORDER BY created DESC", Context),
    [ merge_props(R) || R <- All ].

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
        from = "log_ui",
        where = W2,
        order = "id DESC",
        args = As2,
        assoc = false
    }.


%% @doc Periodic cleanup of max 10K items older than 1 week
periodic_cleanup(Context) ->
    z_db:q("
        delete from log_ui
        where id in (
            select id
            from log_ui
            where created < now() - interval '1 weeks'
            limit 10000
        )",
        Context,
        300000).

install(Context) ->
    case z_db:table_exists(log_ui, Context) of
        true -> ok;
        false ->
            z_db:q("
                create table log_ui (
                    id bigserial not null,
                    rsc_id int,
                    user_id int,
                    type character varying(80) not null default ''::character varying,
                    props bytea,
                    created timestamp with time zone not null default now(),

                    constraint log_ui_pkey primary key (id),
                    constraint fk_log_ui_rsc_id foreign key (rsc_id)
                        references rsc(id)
                        on delete set null on update cascade,
                    constraint fk_log_ui_user_id foreign key (user_id)
                        references rsc(id)
                        on delete set null on update cascade
                )
            ", Context),
            Indices = [
                       {"fki_log_ui_rsc_id", "rsc_id"},
                       {"fki_log_ui_user_id", "user_id"},
                       {"log_ui_type_created_key", "type, created"},
                       {"log_ui_created_key", "created"}
                      ],
            [ z_db:q("create index "++Name++" on log ("++Cols++")", Context) || {Name, Cols} <- Indices ]
    end.

