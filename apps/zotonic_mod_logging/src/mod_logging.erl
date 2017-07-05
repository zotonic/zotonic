%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse
%% @doc Simple database logging.

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

-module(mod_logging).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").
-behaviour(gen_server).

-mod_title("Logging to the database").
-mod_description("Logs debug/info/warning messages into the site's database.").
-mod_prio(1000).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).
-export([
    observe_search_query/2,
    pid_observe_zlog/3,
    observe_admin_menu/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").

-record(state, {site :: atom() | undefined, admin_log_pages=[] :: list()}).

%% interface functions

observe_search_query({search_query, Req, OffsetLimit}, Context) ->
    search(Req, OffsetLimit, Context).

pid_observe_zlog(Pid, #zlog{props=#log_message{}=Msg}, Context) ->
    case Msg#log_message.user_id of
        undefined -> gen_server:cast(Pid, {log, Msg#log_message{user_id=z_acl:user(Context)}});
        _UserId -> gen_server:cast(Pid, {log, Msg})
    end;
pid_observe_zlog(Pid, #zlog{props=#log_email{}=Msg}, _Context) ->
    gen_server:cast(Pid, {log, Msg});
pid_observe_zlog(_Pid, #zlog{}, _Context) ->
    undefined.



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
    install_check(Context1),
    Site = z_context:site(Context1),
    lager:md([
            {site, Site},
            {module, ?MODULE}
        ]),
    {ok, #state{site=Site}}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast({log, #log_message{} = Log}, State) ->
    handle_simple_log(Log, State),
    {noreply, State};
handle_cast({log, OtherLog}, State) ->
    handle_other_log(OtherLog, State),
    {noreply, State};

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
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


install_check(Context) ->
    m_log:install(Context),
    m_log_email:install(Context).



search({log, []}, _OffsetLimit, _Context) ->
    #search_sql{
        select="l.id",
        from="log l",
        tables=[{log, "l"}],
        order="created DESC",
        args=[],
        assoc=false
    };
search({log_email, Filter}, _OffsetLimit, Context) ->
    m_log_email:search(Filter, Context);
search(_, _, _) ->
    undefined.


%% @doc Insert a simple log entry. Send an update to all UA's displaying the log.
handle_simple_log(#log_message{user_id=UserId, type=Type, message=Msg, props=Props}, State) ->
    Context = z_acl:sudo(z_context:new(State#state.site)),
    {ok, Id} = z_db:insert(log, [
                    {user_id, UserId},
                    {type, Type},
                    {message, Msg}
                ] ++ Props, Context),
    mod_signal:emit({log_message, [{log_id, Id}, {user_id, UserId}, {type, Type}, {message, Msg}, {props, Props}]}, Context).

% All non #log_message{} logs are sent to their own log table. If the severity of the log entry is high enough then
% it is also sent to the main log.
handle_other_log(Record, State) ->
    Context = z_acl:sudo(z_context:new(State#state.site)),
    LogType = element(1, Record),
    Fields = record_to_proplist(Record),
    case z_db:table_exists(LogType, Context) of
        true ->
            {ok, Id} = z_db:insert(LogType, Fields, Context),
            Log = record_to_log_message(Record, Fields, LogType, Id),
            case proplists:get_value(severity, Fields) of
                ?LOG_FATAL -> handle_simple_log(Log#log_message{type=fatal}, State);
                ?LOG_ERROR -> handle_simple_log(Log#log_message{type=error}, State);
                _Other -> nop
            end,
            mod_signal:emit({LogType, [{log_id, Id}|Fields]}, Context);
        false ->
            Log = #log_message{
                message=z_convert:to_binary(proplists:get_value(message, Fields, LogType)),
                props=[ {log_type, LogType} | Fields ]
            },
            handle_simple_log(Log, State)
    end.

record_to_proplist(#log_email{} = Rec) ->
    lists:zip(record_info(fields, log_email), tl(tuple_to_list(Rec))).

record_to_log_message(#log_email{} = R, _Fields, LogType, Id) ->
    #log_message{
        message=iolist_to_binary(["SMTP: ",
                    z_convert:to_list(R#log_email.mailer_status), ": ", to_list(R#log_email.mailer_message), $\n,
                    "To: ", z_convert:to_list(R#log_email.envelop_to), opt_user(R#log_email.to_id), $\n,
                    "From: ", z_convert:to_list(R#log_email.envelop_from), opt_user(R#log_email.from_id)
                ]),
        props=[{log_type, LogType}, {log_id, Id}]
    };
record_to_log_message(_, Fields, LogType, Id) ->
    #log_message{
        message=z_convert:to_binary(proplists:get_value(message, Fields, LogType)),
        props=[ {log_type, LogType}, {log_id, Id} | Fields ]
    }.

to_list({error, timeout}) ->
    "timeout";
to_list(R) when is_tuple(R) ->
    io_lib:format("~p", [R]);
to_list(V) ->
    z_convert:to_list(V).

opt_user(undefined) -> [];
opt_user(Id) -> [" (", integer_to_list(Id), ")"].


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
     #menu_separator{parent=admin_system}
     |Acc].

