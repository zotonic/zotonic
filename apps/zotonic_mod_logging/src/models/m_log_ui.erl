%% @author Marc Worrell
%% @copyright 2019-2021 Marc Worrell
%% @doc Model for ui log messages.

%% Copyright 2019-2021 Marc Worrell
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
-moduledoc("
Not yet documented.
").
-author("Marc Worrell").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3,
    log_event/2,
    insert_event/2,
    insert/2,
    get/2,
    ringbuffer/1,
    search_query/2,
    periodic_cleanup/1,
    install/1
]).


-include_lib("zotonic_core/include/zotonic.hrl").

m_get([ Index | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {get(z_convert:to_integer(Index), Context), Rest}};
        false -> {error, eacces}
    end;
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.

%% @doc Fetch a specific log entry from the database UI log.
-spec get(Id, Context) -> map() | undefined when
    Id :: integer(),
    Context :: z:context().
get(Id, Context) ->
    case z_db:select(log_ui, Id, Context) of
        {ok, R} -> R;
        {error, enoent} -> undefined
    end.

%% @doc Return the name of the UI event ringbuffer for this site.
-spec ringbuffer(Context) -> Name when
    Context :: z:context(),
    Name :: atom().
ringbuffer(Context) ->
    z_utils:name_for_site(ui_log_buffer, Context).

%% @doc Log a UI event in the ringbuffer. The buffer is slowly consumed
%% and the surviving events are written to the UI log.
-spec log_event(Event, Context) -> ok | {error, Reason} when
    Event :: map(),
    Context :: z:context(),
    Reason :: ignored.
log_event(Event, Context) ->
    case is_event_loggable(Event) of
        true ->
            LogEvent = #{
                event => Event,
                user_id => z_acl:user(Context),
                peer => m_req:get(peer, Context),
                timestamp => z_datetime:timestamp()
            },
            ringbuffer:write(ringbuffer(Context), LogEvent);
        false ->
            {error, ignored}
    end.

%% @doc Write an event row to the database log table.
insert(Props, Context) ->
    z_db:insert(log_ui, Props, Context).

%% @doc Write a queued event to the database log table. Decorate with
%% information about the user.
insert_event(#{ event := Event, user_id := UserId, timestamp := Timestamp, peer := Peer }, Context) ->
    Props1 = lists:filter( fun filter_prop/1, maps:to_list(Event) ),
    Props2 = lists:map( fun map_prop/1, Props1 ),
    Type = case proplists:get_value(type, Props2) of
        undefined -> <<"error">>;
        T -> T
    end,
    Message = [
        {user_id, UserId},
        {type, Type},
        {remote_ip, Peer},
        {created, z_datetime:timestamp_to_datetime(Timestamp)}
    ] ++ proplists:delete(type, Props2),
    MsgUserProps = maybe_add_user_props(Message, Context),
    z_db:insert(log_ui, MsgUserProps, Context).

%% @doc Only log UI events with a known user_agent. The user-agent must
%% not be a known bot.
is_event_loggable(#{ <<"user_agent">> := UserAgent }) ->
    not is_ignored_bot(UserAgent);
is_event_loggable(Props) ->
    not is_ignored_bot(proplists:get_value(<<"user_agent">>, Props));
is_event_loggable(_Props) ->
    false.

is_ignored_bot(undefined) ->
    false;
is_ignored_bot(UserAgent) ->
    binary:match(UserAgent, <<"YandexMobileBot">>) /= nomatch
    orelse binary:match(UserAgent, <<"YandexBot">>) /= nomatch
    orelse binary:match(UserAgent, <<"pingbot">>) /= nomatch
    orelse binary:match(UserAgent, <<"BingPreview">>) /= nomatch.

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
        <<"notice">> -> <<"notice">>;
        <<"info">> -> <<"info">>;
        <<"debug">> -> <<"debug">>;
        _ -> <<"error">>
    end,
    {type, T1};
map_prop({<<"message">>, M}) when is_binary(M) -> {message, z_string:truncatechars(M, 500)};
map_prop({<<"stack">>, M}) when is_binary(M) -> {stack, z_string:truncatechars(M, 1000)};
map_prop({<<"file">>, M}) when is_binary(M) -> {file, z_string:truncatechars(M, 500)};
map_prop({<<"line">>, M}) -> {line, z_convert:to_integer(M)};
map_prop({<<"col">>, M}) -> {col, z_convert:to_integer(M)};
map_prop({<<"user_agent">>, M}) when is_binary(M) -> {user_agent, z_string:truncatechars(M, 500)};
map_prop({<<"url">>, M}) when is_binary(M) -> {url, z_string:truncatechars(M, 500)}.


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
        true ->
            case z_db:column(log_ui, remote_ip, Context) of
                {ok, #column_def{ length = N }} when N < 42 ->
                    z_db:q("alter table log_ui alter column remote_ip type character varying(42)", Context),
                    z_db:flush(Context);
                _ ->
                    ok
            end,
            ok;
        false ->
            z_db:q("
                create table log_ui (
                    id bigserial not null,
                    rsc_id int,
                    user_id int,
                    type character varying(80) not null default ''::character varying,
                    remote_ip character varying(42),
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

