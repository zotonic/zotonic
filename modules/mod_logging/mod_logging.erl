%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse
%% @date 2010-06-01
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

-mod_title("Message logging").
-mod_description("Logs debug/info/warning messages into the site's database.").
-mod_prio(1000).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).
-export([
    add_admin_log_page/1,
    observe_search_query/2, 
    pid_observe_add_admin_log_page/3,
    pid_observe_log/3
]).

-include("zotonic.hrl").
-include("zotonic_log.hrl").

-record(state, {context, admin_log_pages=[]}).

%% interface functions

add_admin_log_page(C=#context{page_pid=Pid}) ->
    z_notifier:first({add_admin_log_page, Pid}, C).


observe_search_query({search_query, Req, OffsetLimit}, Context) ->
    search(Req, OffsetLimit, Context).

pid_observe_add_admin_log_page(Pid, {add_admin_log_page, _Pid} = Msg, _Context) ->
    gen_server:call(Pid, Msg).

pid_observe_log(Pid, {log, #log_message{}=Msg}, Context) ->
    case Msg#log_message.user_id of
        undefined -> gen_server:cast(Pid, {log, Msg#log_message{user_id=z_acl:user(Context)}});
        _UserId -> gen_server:cast(Pid, {log, Msg})
    end;
pid_observe_log(Pid, {log, _} = LogMsg, _Context) ->
    gen_server:cast(Pid, LogMsg).



%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
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
    {ok, #state{context=Context1}}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
handle_call({add_admin_log_page, Pid}, _From, State) ->
    Pids = lists:filter(fun erlang:is_process_alive/1, [Pid|State#state.admin_log_pages]),
    {reply, ok, State#state{admin_log_pages=Pids}};
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
search(_, _, _) ->
    undefined.



%% @doc Insert a simple log entry. Send an update to all UA's displaying the log.
handle_simple_log(#log_message{user_id=UserId, type=Type, message=Msg, props=Props}, State) ->
    {ok, Id} = z_db:insert(log, [
                    {user_id, UserId},
                    {type, Type},
                    {message, Msg}
                ] ++ Props, State#state.context),

    % Notify admins of any updates
    case State#state.admin_log_pages of
        [] -> 
            nop;
        AdminPages ->
            case catch z_template:render_to_iolist("_admin_log_row.tpl", [{id, Id}], State#state.context) of
                {error, {template_not_found,"_admin_log_row.tpl",enoent}} ->
                    % We can get a template_not_found error when the system is still starting.
                    error;
                {error, Reason} ->
                    error_logger:info_msg("[~p] Render error of _admin_log_row.tpl: ~p~n", 
                                          [(State#state.context)#context.host, Reason]),
                    error;
                {Tpl, _Ctx} ->
                    Tpl2 = lists:flatten(z_string:line(erlang:iolist_to_binary(Tpl))),
                    F = fun(Pid) ->
                                z_session_page:add_script([
                                    "$('", z_utils:js_escape(Tpl2), 
                                    "').hide().insertBefore('#log-area li:first').slideDown().css({backgroundColor:'", 
                                   log_color(Type), "'}).animate({backgroundColor:'", 
                                   log_color(bg), "'}, 8000, 'linear');"], Pid)
                        end,
                    [F(P) || P <- AdminPages],
                    ok
            end
    end.


% All non #log_message{} logs are sent to their own log table. If the severity of the log entry is high enough then
% it is also sent to the  
handle_other_log(Record, State) ->
    LogType = element(1, Record),
    Fields = record_to_proplist(Record),
    case z_db:table_exists(LogType, State#state.context) of
        true ->
            {ok, Id} = z_db:insert(LogType, Fields, State#state.context),
            Log = #log_message{
                message=z_convert:to_binary(proplists:get_value(message, Fields, LogType)),
                props=[ {log_type, LogType}, {log_id, Id} | Fields ]
            },
            case proplists:get_value(severity, Fields) of
                ?LOG_FATAL -> handle_simple_log(Log#log_message{type=fatal}, State);
                ?LOG_ERROR -> handle_simple_log(Log#log_message{type=error}, State);
                _Other -> nop
            end;
        false ->
            Log = #log_message{
                message=z_convert:to_binary(proplists:get_value(message, Fields, LogType)),
                props=[ {log_type, LogType} | Fields ]
            },
            handle_simple_log(Log, State)
    end.


record_to_proplist(#log_email{} = Rec) ->
    lists:zip(record_info(fields, log_email), tl(tuple_to_list(Rec))).





log_color(debug) -> "#ffffff";
log_color(info) -> "#ffff99";
log_color(warning) -> "#ffcc99";
log_color(bg) -> "#f1f1f1";
log_color(_) -> "#f1f1f1".
