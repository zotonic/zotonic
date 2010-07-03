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
    observe_search_query/2, 
    add_admin_log_page/1
]).

%% interface functions

-include("zotonic.hrl").

-record(state, {context, admin_log_pages=[]}).




observe_search_query({search_query, Req, OffsetLimit}, Context) ->
    search(Req, OffsetLimit, Context).


add_admin_log_page(C=#context{page_pid=Pid}) ->
    z_notifier:first({add_admin_log_page, Pid}, C).


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
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    Context1 = z_acl:sudo(z_context:new(Context)),
    z_notifier:observe(add_admin_log_page, self(), Context),

    install_check(Context1),

    %% Watch for log events
    z_notifier:observe(log, self(), Context),
    {ok, #state{context=Context1}}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @doc Trap unknown calls
handle_call({{add_admin_log_page, Pid}, _Ctx}, _From, State) ->
    ?DEBUG("Adding admin log.."),
    Pids = lists:filter(fun erlang:is_process_alive/1, [Pid|State#state.admin_log_pages]),
    {reply, ok, State#state{admin_log_pages=Pids}};
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast({{log, Type, Msg, Props}, Ctx}, State=#state{context=Context}) ->
    {ok, Id} = z_db:insert(log, [
                                 {user_id, z_acl:user(Ctx)},
                                 {type, Type},
                                 {message, Msg}] ++ Props, Context),

    {Tpl, _Ctx} = z_template:render_to_iolist("_admin_log_row.tpl", [{id, Id}], Context),
    Tpl2 = lists:reverse(lists:flatten(z_string:line(erlang:iolist_to_binary(Tpl)))),
    F = fun(Pid) ->
                z_session_page:add_script(["$('", Tpl2, "').hide().insertBefore('#log-area li:first').slideDown().css({backgroundColor:'", 
                                           log_color(Type), "'}).animate({backgroundColor:'", log_color(bg), "'}, 8000, 'linear');"], Pid)
        end,
    [F(P) || P <- State#state.admin_log_pages],

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
terminate(_Reason, State) ->
    Context = State#state.context,
    z_notifier:detach(add_admin_log_page, self(), Context),
    z_notifier:detach(log, self(), Context),
    ok.


%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================


install_check(Context) ->
    case z_db:table_exists(log, Context) of
        true -> ok;
        false ->
            ?DEBUG("Creating log table."),
            z_db:q("
                create table log (
                    id serial not null,
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


log_color(debug) -> "#ffffff";
log_color(info) -> "#ffff99";
log_color(warning) -> "#ffcc99";
log_color(bg) -> "#f1f1f1";
log_color(_) -> "#f1f1f1".
