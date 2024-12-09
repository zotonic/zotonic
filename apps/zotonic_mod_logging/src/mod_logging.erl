%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010-2024 Arjan Scherpenisse
%% @doc Simple database logging.
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

-module(mod_logging).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").
-behaviour(gen_server).

-mod_title("Logging to the database").
-mod_description("Logs debug/info/warning messages into the site's database.").
-mod_prio(1000).
-mod_schema(2).
-mod_depends([ cron ]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).
-export([
    observe_acl_is_allowed/2,
    observe_search_query/2,
    pid_observe_tick_1m/3,
    observe_tick_1h/2,
    pid_observe_zlog/3,
    observe_admin_menu/3,
    is_ui_ratelimit_check/1,
    is_log_client_allowed/1,
    is_log_client_session/1,
    is_log_client_active/1,
    log_client_start/1,
    log_client_stop/1,
    log_client_ping/4,
    manage_schema/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").

-record(state, {
    site :: atom() | undefined,
    dedup :: map(),
    last_ui_event = 0 :: integer(),
    log_event_timestamp = 0 :: integer(),
    log_event_count = 0 :: integer(),
    log_client_id = undefined,
    log_client_topic = undefined,
    log_client_pong = 0
}).

-define(DEDUP_SECS, 600).

% Max number of published log events per second.
-define(LOG_EVENT_RATE, 20).
-define(LOG_CLIENT_TIMEOUT, 900).
-define(LOG_CLIENT_PONG_RECENT, 120).

%% interface functions

observe_acl_is_allowed(#acl_is_allowed{
        action = subscribe,
        object = #acl_mqtt{
            topic = [ <<"model">>,<<"log">>,<<"event">>, <<"console">> ]
        }
    }, Context) ->
    is_log_client_allowed(Context);
observe_acl_is_allowed(_AclIsAllowed, _Context) ->
    undefined.

observe_search_query(#search_query{ name = <<"log">>, args = Args }, Context) ->
    case z_acl:is_allowed(use, mod_logging, Context) of
        true -> m_log:search_query(Args, Context);
        false -> []
    end;
observe_search_query(#search_query{ name = <<"log_email">>, args = Args }, Context) ->
    case z_acl:is_allowed(use, mod_logging, Context) of
        true -> m_log_email:search(Args, Context);
        false -> []
    end;
observe_search_query(#search_query{ name = <<"log_ui">>, args = Args }, Context) ->
    case z_acl:is_allowed(use, mod_logging, Context) of
        true -> m_log_ui:search_query(Args, Context);
        false -> []
    end;
observe_search_query(_Query, _Context) ->
    undefined.

pid_observe_zlog(Pid, #zlog{ user_id = LogUser, props = #log_message{ props = MsgProps } = Msg }, Context) ->
    case proplists:lookup(user_id, MsgProps) of
        {user_id, UserId} when LogUser =:= undefined ->
            gen_server:cast(Pid, {log, Msg#log_message{ user_id = UserId }});
        _ when LogUser =:= undefined ->
            gen_server:cast(Pid, {log, Msg#log_message{ user_id = z_acl:user(Context) }});
        _ ->
            gen_server:cast(Pid, {log, Msg})
    end;
pid_observe_zlog(Pid, #zlog{ props = #log_email{} = Msg }, _Context) ->
    gen_server:cast(Pid, {log, Msg});
pid_observe_zlog(_Pid, #zlog{}, _Context) ->
    undefined.

%% @doc Check the db_pool_health every minute
pid_observe_tick_1m(Pid, tick_1m, _Context) ->
    gen_server:cast(Pid, check_db_pool_health),
    gen_server:cast(Pid, log_client_check).

observe_tick_1h(tick_1h, Context) ->
    m_log:periodic_cleanup(Context),
    m_log_email:periodic_cleanup(Context),
    m_log_ui:periodic_cleanup(Context).


observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
     #menu_item{id=admin_log,
                parent=admin_system,
                label=?__("Log", Context),
                url={admin_log},
                visiblecheck={acl, use, mod_logging}},
     #menu_item{id=admin_log_email,
                parent=admin_system,
                label=?__("Email log", Context),
                url={admin_log_email},
                visiblecheck={acl, use, mod_logging}},
     #menu_item{id=admin_log_ui,
                parent=admin_system,
                label=?__("User interface log", Context),
                url={admin_log_ui},
                visiblecheck={acl, use, mod_logging}},
     #menu_separator{parent=admin_system}
     |Acc].

manage_schema(_, Context) ->
    m_log:install(Context),
    m_log_email:install(Context),
    m_log_ui:install(Context),
    ok.

%% @doc Return true if ok to insert an UI log entry (max 1 per second)
is_ui_ratelimit_check(Context) ->
    case z_convert:to_bool( m_config:get_value(mod_logging, ui_log_disabled, Context) ) of
        true ->
            false;
        false ->
            case z_module_manager:whereis(?MODULE, Context) of
                {ok, Pid} ->
                    gen_server:call(Pid, is_ui_ratelimit_check);
                {error, _} ->
                    % Edge case - can happen when (re)starting of shutting down.
                    false
            end
    end.

%% @doc Subscribe to event logs. The user subscribing must be an admin, or the
%% environment must be development.
-spec log_client_start(Context) -> ok | {error, eacces | no_client} when
    Context :: z:context().
log_client_start(#context{ client_id = undefined }) ->
    {error, no_client};
log_client_start(#context{ client_topic = undefined }) ->
    {error, no_client};
log_client_start(Context) ->
    case is_log_client_allowed(Context) of
        true ->
            ClientId = Context#context.client_id,
            ClientTopic = Context#context.client_topic,
            {ok, Pid} = z_module_manager:whereis(mod_logging, Context),
            ok = gen_server:call(Pid, {log_client_start, ClientId, ClientTopic}),
            ?LOG_INFO(#{
                in => zotonic_mod_logging,
                text => <<"Enabled logging to client console log">>,
                result => ok
            }),
            ok;
        false ->
            {error, eacces}
    end.

%% @doc Unsubscribe from event logs. The user unsubscribing must be an admin, or the
%% environment must be development.
-spec log_client_stop(Context) -> ok | {error, eacces} when
    Context :: z:context().
log_client_stop(Context) ->
    case is_log_client_allowed(Context) of
        true ->
            {ok, Pid} = z_module_manager:whereis(mod_logging, Context),
            ?LOG_INFO(#{
                in => zotonic_mod_logging,
                text => <<"Disabled logging to client console log">>,
                result => ok
            }),
            gen_server:cast(Pid, log_client_stop);
        false ->
            {error, eacces}
    end.

%% @doc Check if the client is allowed to subscribe to the erlang console log.
%% Only the admin or a development site is allowed to subscribe.
-spec is_log_client_allowed(Context) -> boolean() when
    Context :: z:context().
is_log_client_allowed(Context) ->
    case z_acl:user(Context) of
        ?ACL_ADMIN_USER_ID -> true;
        _ ->
            ZotonicEnv = z_config:get(environment),
            SiteEnv = m_site:environment(Context),
            SiteEnv =:= development andalso ZotonicEnv =:= development
    end.

%% @doc Check if this session (clientId) is receiving the logs.
-spec is_log_client_session(Context) -> boolean() when
    Context :: z:context().
is_log_client_session(Context) ->
    case z_context:client_id(Context) of
        {ok, ClientId} ->
            case z_module_manager:whereis(mod_logging, Context) of
                {ok, Pid} ->
                    {ok, #{
                        log_client_id := LogClientId
                    }} = gen_server:call(Pid, log_client_status),
                    ClientId =:= LogClientId;
                {error, _} ->
                    false
            end;
        {error, _} ->
            false
    end.

%% @doc Check if any session (clientId) is receiving the logs.
-spec is_log_client_active(Context) -> boolean() when
    Context :: z:context().
is_log_client_active(Context) ->
    case z_module_manager:whereis(mod_logging, Context) of
        {ok, Pid} ->
            {ok, #{
                log_client_id := ClientId,
                is_pong_recent := IsRecent
            }} = gen_server:call(Pid, log_client_status),
            is_binary(ClientId) andalso IsRecent;
        {error, _} ->
            false
    end.

%%====================================================================
%% API
%%====================================================================
%% @spec start_link(Args) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    Context1 = z_acl:sudo(z_context:new(Context)),
    Site = z_context:site(Context1),
    z_context:logger_md(Context1),
    case m_site:environment(Context1) of
        development -> z_logging_logger_handler:install(Site, self());
        _ -> ok
    end,
    {ok, #state{ site = Site, dedup = #{} }}.

%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
handle_call(is_ui_ratelimit_check, _From, #state{ last_ui_event = LastUI } = State ) ->
    Now = z_datetime:timestamp(),
    {reply, Now > LastUI, State#state{ last_ui_event = Now }};

handle_call({log_client_start, ClientId, ClientTopic}, _From, #state{ site = Site } = State) ->
    z_logging_logger_handler:install(Site, self()),
    State1 = State#state{
        log_client_id = ClientId,
        log_client_topic = ClientTopic,
        log_client_pong = z_datetime:timestamp()
    },
    {reply, ok, State1};
handle_call(log_client_status, _From, State) ->
    Now = z_datetime:timestamp(),
    Status = #{
        log_client_id => State#state.log_client_id,
        log_client_topic => State#state.log_client_topic,
        log_client_pong => State#state.log_client_pong,
        is_pong_recent => (Now - State#state.log_client_pong) < ?LOG_CLIENT_PONG_RECENT
    },
    {reply, {ok, Status}, State};
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast({log, #log_message{} = Log}, State) ->
    handle_simple_log(Log, State),
    {noreply, State};
handle_cast({log, OtherLog}, State) ->
    handle_other_log(OtherLog, State),
    {noreply, State};

handle_cast(check_db_pool_health, #state{ site = Site, dedup = Dedup } = State) ->
    Dedup1 = check_db_pool_health(Dedup, Site),
    {noreply, State#state{ dedup = Dedup1 }};

handle_cast(log_client_stop, State) ->
    State1 = State#state{
        log_client_id = undefined,
        log_client_topic = undefined
    },
    {noreply, State1};
handle_cast(log_client_check, #state{ log_client_id = undefined } = State) ->
    {noreply, State};
handle_cast(log_client_check, #state{ log_client_pong = LastPong } = State) ->
    Now = z_datetime:timestamp(),
    State1 = if
        LastPong + ?LOG_CLIENT_TIMEOUT < Now ->
            State#state{ log_client_id = undefined, log_client_topic = undefined };
        true ->
            Context = z_acl:sudo(z_context:new(State#state.site)),
            ClientId = State#state.log_client_id,
            ClientTopic = State#state.log_client_topic,
            z_sidejob:start(?MODULE, log_client_ping, [ ClientId, ClientTopic, self() ], Context),
            State
    end,
    {noreply, State1};
handle_cast({log_client_pong, PongClientId, true}, #state{ log_client_id = ClientId } = State) when PongClientId =:= ClientId ->
    {noreply, State#state{ log_client_pong = z_datetime:timestamp() }};
handle_cast({log_client_pong, PongClientId, false}, #state{ log_client_id = ClientId } = State) when PongClientId =:= ClientId ->
    {noreply, State#state{ log_client_id = undefined, log_client_topic = undefined }};
handle_cast({log_client_pong, _, _}, State) ->
    {noreply, State};

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
handle_info({logger, Data}, #state{ site = Site, log_client_topic = ClientTopic } = State) ->
    Context = z_acl:sudo(z_context:new(Site)),
    case log_event_ratelimit(State) of
        {true, State1} ->
            ZotonicEnv = z_config:get(environment),
            SiteEnv = m_site:environment(Context),
            if
                ZotonicEnv =:= development andalso SiteEnv =:= development ->
                    z_mqtt:publish(<<"model/log/event/console">>, Data, Context);
                ClientTopic =/= undefined ->
                    z_mqtt:publish(
                        ClientTopic ++ [ <<"model">>, <<"console">>, <<"post">>, <<"log">> ],
                        Data, Context);
                true ->
                    ok
            end,
            {noreply, State1};
        {false, State1} ->
            {noreply, State1}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.


%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

%% @doc Check if the log client is still Active.
log_client_ping(ClientId, ClientTopic, Pid, Context) ->
    Topic = ClientTopic ++ [ <<"model">>, <<"console">>, <<"post">>, <<"ping">> ],
    case z_mqtt:call(Topic, <<"ping">>, Context) of
        {ok, <<"pong">>} ->
            gen_server:cast(Pid, {log_client_pong, ClientId, true});
        {error, _} ->
            % Retry in a minute
            ok
    end.

%% @doc Check if we are rate limiting publishing log events to MQTT. Simple limiting
%% of max number of messages in the current second window.
log_event_ratelimit(#state{ log_event_count = Count, log_event_timestamp = Tm } = State) ->
    Now = z_datetime:timestamp(),
    if
        Now =:= Tm, Count =< ?LOG_EVENT_RATE ->
            {true, State#state{ log_event_count = Count + 1 }};
        Now =:= Tm ->
            {false, State};
        true ->
            {true, State#state{ log_event_count = 1, log_event_timestamp = Now }}
    end.

%% @private Check the health of the db pool. When usage is to high a warning will be
%% put in the log. The warning is deduplicated every hour.
check_db_pool_health(Dedup, Site) ->
    Dedup1 = case exometer:get_value([site, Site, db, pool_full], one) of
        {ok, [{one, FullCounts}]} when FullCounts > 0 ->
            case is_dup(pool_full, Dedup) of
                {true, D1} ->
                    D1;
                {false, D1} ->
                    ?LOG_WARNING(#{
                        in => zotonic_core,
                        text => <<"Database pool is busy, all connections used. Increase site.db_max_connections">>,
                        result => error,
                        reason => database_pool,
                        count => FullCounts,
                        site => Site
                    }),
                    D1
            end;
        {ok, _} ->
            Dedup;
        {error, not_found} ->
            Dedup
    end,
    case exometer:get_value([site, Site, db, pool_high_usage], one) of
        {ok, [{one, HighCounts}]} when HighCounts > 0 ->
            case is_dup(pool_high_usage, Dedup1) of
                {true, D2} ->
                    D2;
                {false, D2} ->
                    ?LOG_INFO(#{
                        in => zotonic_core,
                        text => <<"Database pool usage is high. Increase site.db_max_connections">>,
                        result => warning,
                        reason => database_pool,
                        count => HighCounts,
                        site => Site
                    }),
                    D2
            end;
        {ok, _} ->
            Dedup1;
        {error, not_found} ->
            Dedup1
    end.

is_dup(Event, Dup) ->
    Now = z_datetime:timestamp(),
    case maps:get(Event, Dup, undefined) of
        undefined ->
            {false, Dup#{ Event => Now }};
        T ->
            case T < Now - ?DEDUP_SECS of
                true ->
                    {false, Dup#{ Event => Now }};
                false ->
                    {true, Dup}
            end
    end.

handle_simple_log(#log_message{ user_id = UserId, type = Type, message = Msg, props = Props }, State) ->
    Context = z_acl:sudo(z_context:new(State#state.site)),
    Message = [
        {user_id, UserId},
        {type, Type},
        {message, Msg}
    ] ++ proplists:delete(user_id, Props),
    MsgUserProps = maybe_add_user_props(Message, Context),
    {ok, Id} = z_db:insert(log, MsgUserProps, Context),
    SeverityB = z_convert:to_binary(Type),
    LogTypeB = z_convert:to_binary( proplists:get_value(log_type, Props, log) ),
    z_mqtt:publish(
        [ <<"model">>, <<"logging">>, <<"event">>, LogTypeB, SeverityB ],
        #{
            log_id => Id,
            user_id => UserId
        },
        Context),
    ok.

% All non #log_message{} logs are sent to their own log table. If the severity of the log entry is high enough then
% it is also sent to the main log.
handle_other_log(Record, State) ->
    Context = z_acl:sudo(z_context:new(State#state.site)),
    LogType = element(1, Record),
    Fields = record_to_proplist(Record),
    case z_db:table_exists(LogType, Context) of
        true ->
            {ok, Id} = z_db:insert(LogType, flatten(Fields), Context),
            Log = record_to_log_message(Record, Fields, LogType, Id),
            Severity = proplists:get_value(severity, Fields),
            case Severity of
                ?LOG_LEVEL_FATAL ->
                    handle_simple_log(Log#log_message{type=fatal}, State);
                ?LOG_LEVEL_ERROR ->
                    handle_simple_log(Log#log_message{type=error}, State);
                _Other ->
                    nop
            end,
            ok;
        false ->
            Log = #log_message{
                message=z_convert:to_binary(proplists:get_value(message, Fields, LogType)),
                props=[ {log_type, LogType} | Fields ]
            },
            handle_simple_log(Log, State)
    end.

flatten(Fields) ->
     lists:map( fun flatten_prop/1, Fields ).

flatten_prop({_, undefined} = Prop) ->
    Prop;
flatten_prop({_, V} = Prop) when is_binary(V); is_list(V); is_number(V); is_boolean(V); is_atom(V) ->
    Prop;
flatten_prop({_, {term, _}} = Prop) ->
    Prop;
flatten_prop({props, _} = Prop) ->
    Prop;
flatten_prop({K, {cat, T}}) when is_list(T); is_binary(T) ->
    {K, iolist_to_binary([ "cat:", T ])};
flatten_prop({K, V} = Prop) ->
    case is_date(V) of
        true -> Prop;
        false -> {K, z_convert:to_binary(V)}
    end.

is_date({{_, _, _}, {_, _, _}}) -> true;
is_date({Y, M, D}) when is_integer(Y); is_integer(M); is_integer(D) -> true;
is_date(_) -> false.

record_to_proplist(#log_email{} = Rec) ->
    lists:zip(record_info(fields, log_email), tl(tuple_to_list(Rec))).

record_to_log_message(#log_email{} = R, _Fields, LogType, Id) ->
    #log_message{
        message=iolist_to_binary(["SMTP: ",
                    z_convert:to_list(R#log_email.mailer_status), ": ", to_list(R#log_email.mailer_message), $\n,
                    "To: ", z_convert:to_list(R#log_email.envelop_to), opt_user(R#log_email.to_id), $\n,
                    "From: ", z_convert:to_list(R#log_email.envelop_from), opt_user(R#log_email.from_id)
                ]),
        props=[
            {log_type, LogType},
            {log_id, Id}
        ]
    }.

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

to_list({error, timeout}) ->
    "timeout";
to_list(R) when is_tuple(R) ->
    io_lib:format("~p", [R]);
to_list(V) ->
    z_convert:to_list(V).

opt_user(undefined) -> [];
opt_user(Id) -> [" (", integer_to_list(Id), ")"].

