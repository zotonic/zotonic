%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2015 Marc Worrell

%% @doc Module supervisor. Uses a z_supervisor.  Starts/restarts module processes.

%% Copyright 2009-2015 Marc Worrell
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

-module(z_module_manager).
-author('Marc Worrell <marc@worrell.nl>').
-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% External exports
%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% API exports
-export([
         upgrade/1,
         deactivate/2,
         activate/2,
         activate_await/2,
         await_upgrade/1,
         restart/2,
         module_reloaded/2,
         active/1,
         active/2,
         active_dir/1,
         get_provided/1,
         get_modules/1,
         get_modules_status/1,
         get_upgrade_status/1,
         whereis/2,
         all/1,
         scan/1,
         scan_core/1,
         prio/1,
         prio_sort/1,
         dependency_sort/1,
         dependencies/1,
         startable/2,
         module_exists/1,
         title/1,
         reinstall/2
        ]).

-include_lib("zotonic.hrl").

%% The default module priority
-define(MOD_PRIO, 500).

%% Give a module a minute to start
-define(MODULE_START_TIMEOUT, 60000).

%% Module manager state
-record(state, {
          context :: #context{},
          sup :: pid(),
          module_exports = [] :: list({atom(), list()}),
          module_schema = [] :: list({atom(), integer()|undefined}),
          start_wait  = none :: none | {atom(), pid(), erlang:timestamp()},
          start_queue = [] :: list(),
          start_error = [] :: list(),
          upgrade_waiters = [] :: list()
      }).


%%====================================================================
%% API
%%====================================================================
%% @spec start_link(SiteProps::proplist()) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the module manager
start_link(SiteProps) ->
    Context = z_acl:sudo(z_context:new(proplists:get_value(site, SiteProps))),
    gen_server:start_link({local, name(Context)}, ?MODULE, [{context, Context} | SiteProps], []).


%% @doc Reload the list of all modules, add processes if necessary.
-spec upgrade(#context{}) -> ok.
upgrade(Context) ->
    upgrade(false, Context).

-spec upgrade(boolean(), #context{}) -> ok.
upgrade(false, Context) ->
    flush(Context),
    gen_server:cast(name(Context), upgrade);
upgrade(true, Context) ->
    flush(Context),
    gen_server:call(name(Context), upgrade).


%% @doc Deactivate a module. The module is marked as deactivated and stopped when it was running.
-spec deactivate(atom(), #context{}) -> ok.
deactivate(Module, Context) ->
    flush(Context),
    case z_db:q("update module set is_active = false, modified = now() where name = $1", [Module], Context) of
        1 -> upgrade(false, Context);
        0 -> ok
    end.


%% @doc Activate a module. The module is marked as active and started as a child of the module z_supervisor.
%% The module manager can be checked later to see if the module started or not.
-spec activate(atom(), #context{}) -> ok | {error, not_found}.
activate(Module, Context) when is_atom(Module) ->
    activate(Module, false, Context).

-spec activate_await(atom(), #context{}) -> ok | {error, not_active} | {error, not_found}.
activate_await(Module, Context) when is_atom(Module) ->
    case activate(Module, true, Context) of
        ok ->
            case whereis(Module, Context) of
                {ok, Pid} ->
                    case erlang:is_process_alive(Pid) of
                        true -> ok;
                        false -> {error, not_active}
                    end;
                {error, not_running} ->
                    {error, not_active}
            end;
        {error, _} = Error ->
            Error
    end.

activate(Module, IsSync, Context) ->
    flush(Context),
    case proplists:lookup(Module, scan(Context)) of
        {Module, _Dirname} ->
            F = fun(Ctx) ->
                        case z_db:q("update module
                                     set is_active = true,
                                         modified = now()
                                     where name = $1",
                                    [Module], Ctx)
                        of
                            0 ->
                                z_db:q("insert into module (name, is_active)
                                        values ($1, true)",
                                       [Module], Ctx);
                            1 -> 1
                        end
                end,
            1 = z_db:transaction(F, Context),
            upgrade(IsSync, Context);
        none ->
            lager:error("Could not find module '~p'", [Module]),
            {error, not_found}
    end.

%% @doc Wait till all modules are started, used when starting up a new or test site.
-spec await_upgrade(#context{}) -> ok | {error, timeout}.
await_upgrade(Context) ->
    await_upgrade(Context, 20).

await_upgrade(_Context, 0) ->
    {error, timeout};
await_upgrade(Context, RetryCt) ->
    case erlang:whereis(name(Context)) of
        undefined ->
            timer:sleep(500),
            await_upgrade(Context, RetryCt-1);
        Pid ->
            gen_server:call(Pid, await_upgrade, infinity)
    end.

%% @doc Restart a module, activates the module if it was not activated.
-spec restart(Module::atom(), #context{}) -> ok | {error, not_found}.
restart(Module, Context) ->
    case z_db:q1("select is_active from module where name = $1", [Module], Context) of
        true -> gen_server:cast(name(Context), {restart_module, Module});
        _ -> activate(Module, Context)
    end.

%% @doc Check all observers of a module, ensure that they are all active. Used after a module has been reloaded
-spec module_reloaded(Module::atom(), #context{}) -> ok.
module_reloaded(Module, Context) ->
    case z_db:q1("select is_active from module where name = $1", [Module], Context) of
        true -> gen_server:cast(name(Context), {module_reloaded, Module});
        _ -> ok
    end.

%% @doc Return the list of active modules.
-spec active(#context{}) -> list(Module::atom()).
active(Context) ->
    case z_db:has_connection(Context) of
        true ->
            F = fun() ->
                        Modules = z_db:q("select name from module where is_active = true order by name", Context),
                        [ z_convert:to_atom(M) || {M} <- Modules ]
                end,
            z_depcache:memo(F, {?MODULE, active, z_context:site(Context)}, Context);
        false ->
            case m_site:get(modules, Context) of
                L when is_list(L) -> L;
                _ -> []
            end
    end.



%% @doc Return whether a specific module is active.
-spec active(Module::atom(), #context{}) -> boolean().
active(Module, Context) ->
    case z_db:has_connection(Context) of
        true ->
            F = fun() ->
                        case z_db:q("select true from module where name = $1 and is_active = true", [Module], Context) of
                            [{true}] -> true;
                            _ -> false
                        end
                end,
            z_depcache:memo(F, {?MODULE, {active, Module}, z_context:site(Context)}, Context);
        false ->
            lists:member(Module, active(Context))
    end.


%% @doc Return the list of all active modules and their directories
%% @spec active_dir(#context{}) -> [ {atom, Dir} ]
active_dir(Context) ->
    Active = active(Context),
    All    = scan(Context),
    [ {M, proplists:get_value(M, All)} || M <- Active ].


%% @doc Return the list of all modules running.
get_modules(Context) ->
    gen_server:call(name(Context), get_modules).


%% @doc Return the list of all provided functionalities in running modules.
get_provided(Context) ->
    gen_server:call(name(Context), get_provided).


%% @doc Return the status of all running modules.
get_modules_status(Context) ->
    gen_server:call(name(Context), get_modules_status).


%% @doc Return the status of any ongoing upgrade
get_upgrade_status(Context) ->
    gen_server:call(name(Context), get_upgrade_status).


%% @doc Return the pid of a running module
-spec whereis(atom(), #context{}) -> {ok, pid()} | {error, not_running}.
whereis(Module, Context) ->
    gen_server:call(name(Context), {whereis, Module}).


%% @doc Return the list of all modules in the database.
%% @spec all(#context{}) -> [ atom() ]
all(Context) ->
    Modules = z_db:q("select name from module order by name", Context),
    [ z_convert:to_atom(M) || {M} <- Modules ].


%% @doc Scan for a list of modules present in the site's module directories. A module is always a directory,
%% the name of the directory is the same as the name of the module.
%% @spec scan(#context{}) -> [ {atom(), dirname()} ]
scan(#context{site=Site}) ->
    All = [
           %% Zotonic modules
           [z_utils:lib_dir(modules), "mod_*"],

           %% User-installed Zotonic sites
           [z_path:user_sites_dir(), Site, "modules", "mod_*"],
           [z_path:user_sites_dir(), Site],

           %% User-installed modules
           [z_path:user_modules_dir(), "mod_*"],

           %% Backward compatibility
           [z_utils:lib_dir(priv), "sites", Site, "modules", "mod_*"],
           [z_utils:lib_dir(priv), "sites", Site],
           [z_utils:lib_dir(priv), "modules", "mod_*"]

          ],
    scan_paths(All).

%% @doc Get a list of Zotonic core modules.
-spec scan_core(#context{}) -> list({ModuleName :: atom(), Path :: string()}).
scan_core(#context{}) ->
    scan_paths([
        [z_utils:lib_dir(modules), "mod_*"]
    ]).

scan_paths(Paths) ->
    Files = lists:foldl(fun(L, Acc) -> L ++ Acc end, [], [z_utils:wildcard(filename:join(P)) || P <- Paths]),
    [ {z_convert:to_atom(filename:basename(F)), F} ||  F <- Files ].

%% @doc Return the priority of a module. Default priority is 500, lower is higher priority.
%% Never crash on a missing module.
%% @spec prio(Module) -> integer()
prio(Module) ->
    try
        Info = erlang:get_module_info(Module, attributes),
        case proplists:get_value(mod_prio, Info) of
            [Prio] -> Prio;
            _ -> ?MOD_PRIO
        end
    catch
        _M:_E -> ?MOD_PRIO
    end.


%% @doc Sort the results of a scan on module priority first, module name next.
%% The list is made up of {module, Values} tuples
%% @spec prio_sort(proplist()) -> proplist()
prio_sort([{_,_}|_]=ModuleProps) ->
    WithPrio = [ {z_module_manager:prio(M), {M, X}} || {M, X} <- ModuleProps ],
    Sorted = lists:sort(WithPrio),
    [ X || {_Prio, X} <- Sorted ];

%% @doc Sort the results of a scan on module priority first, module name next.
%% The list is made up of module atoms.
%% @spec prio_sort(proplist()) -> proplist()
prio_sort(Modules) ->
    WithPrio = [ {z_module_manager:prio(M), M} || M <- Modules ],
    Sorted = lists:sort(WithPrio),
    [ M || {_Prio, M} <- Sorted ].


%% @doc Sort all modules on their dependencies (with sub sort the module's priority)
dependency_sort(#context{} = Context) ->
    dependency_sort(active(Context));
dependency_sort(Modules) when is_list(Modules) ->
    Ms = [ dependencies(M) || M <- prio_sort(Modules) ],
    z_toposort:sort(Ms).


%% @doc Return a module's dependencies as a tuple usable for z_toposort:sort/1.
dependencies({M, X}) ->
    {_, Ds, Ps} = dependencies(M),
    {{M,X}, Ds, Ps};
dependencies(M) when is_atom(M) ->
    try
        Info = erlang:get_module_info(M, attributes),
        Depends = proplists:get_value(mod_depends, Info, [base]),
        Provides = [ M | proplists:get_value(mod_provides, Info, []) ],
        {M, Depends, Provides}
    catch
        _M:_E -> {M, [], []}
    end.


startable(M, #context{} = Context) ->
    Provided = get_provided(Context),
    startable(M, Provided);
startable(Module, Dependencies) when is_list(Dependencies) ->
    case is_module(Module) of
        true ->
            case dependencies(Module) of
                {Module, Depends, _Provides} ->
                    Missing = lists:foldl(fun(Dep, Ms) ->
                                                  case lists:member(Dep, Dependencies) of
                                                      true -> Ms;
                                                      false -> [Dep|Ms]
                                                  end
                                          end,
                                          [],
                                          Depends),
                    case Missing of
                        [] -> ok;
                        _ -> {error, {missing_dependencies, Missing}}
                    end;
                _ ->
                    {error, could_not_derive_dependencies}
            end;
        false ->
            {error, not_found}
    end.


get_start_error_reason({error, not_found}) ->
    "Module not found";
get_start_error_reason({error, {missing_dependencies, Missing}}) ->
    "Missing dependencies: " ++ binary_to_list(iolist_to_binary(io_lib:format("~p", [Missing])));
get_start_error_reason({error, could_not_derive_dependencies}) ->
    "Could not derive dependencies".




%% @doc Check if the code of a module exists. The database can hold module references to non-existing modules.
module_exists(M) ->
    case code:ensure_loaded(M) of
        {module,M} -> true;
        {error, _} -> false
    end.


%% @doc Get the title of a module.
title(M) ->
    try
        proplists:get_value(mod_title, M:module_info(attributes))
    catch
        _M:_E -> undefined
    end.


%% @doc Get the schema version of a module.
mod_schema(M) ->
    try
        {mod_schema, [S]} = proplists:lookup(mod_schema, M:module_info(attributes)),
        S
    catch
        _M:_E -> undefined
    end.

db_schema_version(M, Context) ->
    z_db:q1("SELECT schema_version FROM module WHERE name = $1", [M], Context).

set_db_schema_version(M, V, Context) ->
    1 = z_db:q("UPDATE module SET schema_version = $1 WHERE name = $2", [V, M], Context),
    ok.



%%====================================================================
%% gen_server callbacks
%%====================================================================


%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    Context = proplists:get_value(context, Args),
    lager:md([
        {site, z_context:site(Context)},
        {module, ?MODULE}
      ]),
    {ok, Sup} = z_supervisor:start_link([]),
    z_supervisor:set_manager_pid(Sup, self()),
    {ok, #state{context=Context, sup=Sup}}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Return a list of all enabled modules managed by the z_supervisor
handle_call(get_modules, _From, State) ->
    All = handle_get_modules(State),
    {reply, All, State};

%% @doc Return the list of all provided services by the modules.
handle_call(get_provided, _From, State) ->
    Provided = handle_get_provided(State),
    {reply, Provided, State};

%% @doc Return all running modules and their status
handle_call(get_modules_status, _From, State) ->
    {reply, z_supervisor:which_children(State#state.sup), State};

%% @doc Return the pid of a running module
handle_call({whereis, Module}, _From, State) ->
    Running = proplists:get_value(running, z_supervisor:which_children(State#state.sup)),
    Ret = case lists:keysearch(Module, 1, Running) of
              {value, {Module, _, Pid, _}} -> {ok, Pid};
              false -> {error, not_running}
          end,
    {reply, Ret, State};

%% @doc Synchronous upgrade
handle_call(upgrade, From, State) ->
    State1 = State#state{upgrade_waiters=[From|State#state.upgrade_waiters]},
    State2 = handle_upgrade(State1),
    {noreply, State2};

handle_call(get_upgrade_status, _From, State) ->
    Reply = [
        {start_wait, State#state.start_wait},
        {start_queue, State#state.start_queue},
        {start_error, State#state.start_error},
        {upgrade_waiters, State#state.upgrade_waiters}
    ],
    {reply, {ok, Reply}, State};

handle_call(await_upgrade, _From, #state{start_wait=none, start_queue=[]} = State) ->
    {reply, ok, State};
handle_call(await_upgrade, From, State) ->
    {noreply, State#state{upgrade_waiters=[From|State#state.upgrade_waiters]}};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Sync enabled modules with loaded modules
handle_cast(upgrade, State) ->
    State1 = handle_upgrade(State),
    {noreply, State1};

%% @doc Sync enabled modules with loaded modules
handle_cast(start_next, State) ->
    State1 = handle_start_next(State),
    {noreply, State1};

%% @doc Restart a running module.
handle_cast({restart_module, Module}, State) ->
    State1 = handle_restart_module(Module, State),
    {noreply, State1};

%% @doc New child process started, add the event listeners
%% @todo When this is an automatic restart, start all depending modules
handle_cast({supervisor_child_started, ChildSpec, Pid}, State) ->
    Module = ChildSpec#child_spec.name,
    lager:debug("Module ~p started", [Module]),
    State1 = handle_start_child_result(Module, {ok, Pid}, State),
    {noreply, State1};

%% @doc Handle errors, success is handled by the supervisor_child_started above.
handle_cast({start_child_result, Module, {error, _} = Error}, State) ->
    State1 = handle_start_child_result(Module, Error, State),
    lager:error("Module ~p start error ~p", [Module, Error]),
    {noreply, State1};
handle_cast({start_child_result, _Module, {ok, _}}, State) ->
    {noreply, State};

%% @doc Existing child process stopped, remove the event listeners
handle_cast({supervisor_child_stopped, ChildSpec, Pid}, State) ->
    Module = ChildSpec#child_spec.name,
    remove_observers(Module, Pid, State),
    lager:info("Module ~p stopped", [Module]),
    z_notifier:notify(#module_deactivate{module=Module}, State#state.context),
    stop_children_with_missing_depends(State),
    {noreply, State};

%% @doc Check all observers of a module. Add new ones, remove non-existing ones.
%%      This is called after a code reload of a module.
handle_cast({module_reloaded, Module}, State) ->
    lager:debug("checking observers of (re-)loaded module ~p", [Module]),
    TmpState = refresh_module_exports(Module, refresh_module_schema(Module, State)),
    OldExports = proplists:get_value(Module, State#state.module_exports),
    NewExports = proplists:get_value(Module, TmpState#state.module_exports),
    OldSchema = proplists:get_value(Module, State#state.module_schema),
    NewSchema = proplists:get_value(Module, TmpState#state.module_schema),
    case {OldExports, OldSchema} of
        {NewExports, NewSchema} ->
            {noreply, State};
        {undefined, undefined} ->
            % Assume this load is because of the first start, otherwise there would be some exports known.
            {noreply, State};
        _Changed ->
            % Exports or schema changed, assume the worst and restart the complete module
            lager:info("exports or schema of (re-)loaded module ~p changed, restarting module",
                       [Module]),
            gen_server:cast(self(), {restart_module, Module}),
            {noreply, State}
    end;

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    lager:error("z_module_manager: unknown cast ~p in state ~p", [Message, State]),
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


%% @doc Return the name for this site's module manager
name(Context) ->
    name(?MODULE, Context).
name(Module, #context{site=Site}) ->
    z_utils:name_for_site(Module, Site).

flush(Context) ->
    z_depcache:flush({?MODULE, active, z_context:site(Context)}, Context).

handle_restart_module(Module, #state{context=Context, sup=ModuleSup} = State) ->
    case z_supervisor:delete_child(ModuleSup, Module) of
        {error, Err} when Err =:= unknown_child; Err =:= no_process ->
            % Child was not running
            ok;
        ok ->
            % Child has been shut down, wait for the notification
            receive
                {'$gen_cast', {supervisor_child_stopped, #child_spec{name=Module}, Pid}} ->
                    remove_observers(Module, Pid, State),
                    z_supervisor:add_child_async(ModuleSup, module_spec(Module, Context))
            end
    end,
    handle_upgrade(State).

handle_upgrade(#state{context=Context, sup=ModuleSup} = State) ->
    ValidModules = valid_modules(Context),
    z_depcache:flush(z_modules, Context),

    ChildrenPids = handle_get_modules_pid(State),
    Old  = sets:from_list([Name || {Name, _} <- ChildrenPids]),
    New  = sets:from_list(ValidModules),
    Kill = sets:subtract(Old, New),
    Create = sets:subtract(New, Old),

    Running = z_supervisor:running_children(State#state.sup),
    Start = sets:to_list(sets:subtract(New, sets:from_list(Running))),
    {ok, StartList} = dependency_sort(Start),
    lager:debug("Stopping modules: ~p", [sets:to_list(Kill)]),
    lager:debug("Starting modules: ~p", [Start]),
    sets:fold(fun (Module, ok) ->
                      z_supervisor:delete_child(ModuleSup, Module),
                      ok
              end, ok, Kill),

    sets:fold(fun (Module, ok) ->
                      z_supervisor:add_child_async(ModuleSup, module_spec(Module, Context)),
                      ok
              end, ok, Create),

    % 1. Put all to be started modules into a start list (add to State)
    % 2. Let the module manager start them one by one (if startable)
    % 3. Log any start errors, suppress modules that have errors.
    % 4. Log non startable modules (remaining after all startable modules have started)
    case {StartList, sets:size(Kill)} of
        {[], 0} ->
            signal_upgrade_waiters(State#state{start_queue=[]});
        _ ->
            gen_server:cast(self(), start_next),
            State#state{start_queue=StartList}
    end.

signal_upgrade_waiters(#state{upgrade_waiters = Waiters} = State) ->
    lists:foreach(fun(From) ->
                      gen_server:reply(From, ok)
                  end,
                  Waiters),
    State#state{upgrade_waiters=[]}.


handle_start_next(#state{context=Context, start_queue=[]} = State) ->
    % Signal modules are loaded, and load all translations.
    z_notifier:notify(module_ready, Context),
    lager:debug("Finished starting modules"),
    spawn_link(fun() -> z_trans_server:load_translations(Context) end),
    signal_upgrade_waiters(State);
handle_start_next(#state{context=Context, sup=ModuleSup, start_queue=Starting} = State) ->
    % Filter all children on the capabilities of the loaded modules.
    Provided = handle_get_provided(State),
    case lists:filter(fun(M) -> is_startable(M, Provided) end, Starting) of
        [] ->
            [
             begin
                 StartErrorReason = get_start_error_reason(startable(M, Provided)),
                 Msg = iolist_to_binary(io_lib:format("Could not start ~p: ~s", [M, StartErrorReason])),
                 z_session_manager:broadcast(#broadcast{type="error", message=Msg, title="Module manager", stay=false}, z_acl:sudo(Context)),
                 lager:error("~s", [Msg])
             end || M <- Starting
            ],

            % Add non-started modules to the list with errors.
            CleanedUpErrors = lists:foldl(fun(M,Acc) ->
                                                  proplists:delete(M,Acc)
                                          end,
                                          State#state.start_error,
                                          Starting),

            handle_start_next(State#state{
                                start_error=[ {M, case is_module(M) of false -> not_found; true -> dependencies_error end}
                                              || M <- Starting
                                            ] ++ CleanedUpErrors,
                                start_queue=[]
                               });
        [Module|_] ->
            State1 = refresh_module_exports(Module, refresh_module_schema(Module, State)),
            {Module, Exports} = lists:keyfind(Module, 1, State1#state.module_exports),
            case start_child(self(), Module, ModuleSup, module_spec(Module, Context), Exports, Context) of
                {ok, StartHelperPid} ->
                    State1#state{
                      start_error=proplists:delete(Module, State1#state.start_error),
                      start_wait={Module, StartHelperPid, os:timestamp()},
                      start_queue=lists:delete(Module, Starting)
                    };
                {error, Reason} ->
                    handle_start_next(
                      State1#state{
                        start_error=[ {Module, Reason} | proplists:delete(Module, State1#state.start_error) ],
                        start_queue=lists:delete(Module, Starting)
                      })
            end
    end.

%% @doc Check if all module dependencies are running.
is_startable(Module, Dependencies) ->
    startable(Module, Dependencies) =:= ok.

%% @doc Check if we can load the module
is_module(Module) ->
    try
        {ok, _} = z_utils:ensure_existing_module(Module),
        true
    catch
        M:E ->
            lager:error("Can not fetch module info for module ~p, error: ~p:~p", [Module, M, E]),
            false
    end.

%% @doc Try to add and start the child, do not crash on missing modules. Run as a separate process.
%% @todo Add some preflight tests
-spec start_child(pid(), atom(), any(), any(), any(), any()) -> {ok, pid()} | {error, string()}.
start_child(ManagerPid, Module, ModuleSup, Spec, Exports, Context) ->
    StartPid = spawn_link(
                 fun() ->
                         Result = case manage_schema(Module, Exports, Context) of
                                      ok ->
                                                % Try to start it
                                          z_supervisor:start_child(ModuleSup, Spec#child_spec.name, ?MODULE_START_TIMEOUT);
                                      Error ->
                                          lager:error("Error starting module ~p, Schema initialization error:~n~p~n",
                                                      [Module, Error]),
                                          {error, {schema_init, Error}}
                                  end,
                         gen_server:cast(ManagerPid, {start_child_result, Module, Result})
                 end
                ),
    {ok, StartPid}.



handle_start_child_result(Module, Result, State) ->
                                                % Where we waiting for this child? If so, start the next.
    {State1,IsWaiting} = case State#state.start_wait of
                             {Module, _Pid, _NowStarted} ->
                                 {State#state{start_wait=none}, true};
                             _Other ->
                                 {State, false}
                         end,
    ReturnState = case Result of
                      {ok, Pid} ->
                                                % Remove any registered errors for the started module
                          State2 = State1#state{start_error=lists:keydelete(Module, 1, State1#state.start_error)},
                          add_observers(Module, Pid, State2),
                          z_notifier:notify(#module_activate{module=Module, pid=Pid}, State2#state.context),
                          State2;
                      {error, _Reason} = Error ->
                                                % Add a start error for this module
                          State1#state{
                            start_error=[ {Module, Error} | lists:keydelete(Module, 1, State1#state.start_error) ]
                           }
                  end,
    case IsWaiting of
        true -> gen_server:cast(self(), start_next);
        false -> ok
    end,
    ReturnState.


%% @doc Return the list of all provided services.
handle_get_provided(State) ->
    get_provided_for_modules(handle_get_running(State)).

get_provided_for_modules(Modules) ->
    lists:flatten(
      [ case dependencies(M) of
            {_, _, Provides} -> Provides;
            _ -> []
        end
        || M <- Modules ]).


stop_children_with_missing_depends(State) ->
    Modules = handle_get_modules(State),
    Provided = get_provided_for_modules(Modules),
    case lists:filter(fun(M) -> not is_startable(M, Provided) end, Modules) of
        [] ->
            [];
        Unstartable ->
            lager:debug("Stopping child modules ~p", [Unstartable]),
            [ z_supervisor:stop_child(State#state.sup, M) || M <- Unstartable ]
    end.


%% @doc Return the list of module names currently managed by the z_supervisor.
handle_get_modules(State) ->
    [ Name || {Name,_Pid} <- handle_get_modules_pid(State) ].

%% @doc Return the list of module names currently managed and running by the z_supervisor.
handle_get_running(State) ->
    z_supervisor:running_children(State#state.sup).

%% @doc Return the list of module names and their pid currently managed by the z_supervisor.
handle_get_modules_pid(State) ->
    lists:flatten(
      lists:map(fun({_State,Children}) ->
                        [ {Name,Pid} || {Name,_Spec,Pid,_Time} <- Children ]
                end,
                z_supervisor:which_children(State#state.sup))).

%% @doc Get a list of all valid modules
valid_modules(Context) ->
    Ms0 = lists:filter(fun module_exists/1, active(Context)),
    lists:filter(fun(Mod) -> valid(Mod, Context) end, Ms0).


%% @doc Return the z_supervisor child spec for a module
module_spec(ModuleName, Context) ->
    {ok, Cfg} = z_sites_manager:get_site_config(z_context:site(Context)),
    Args = [ {context, Context}, {module, ModuleName} | Cfg ],
    ChildModule = gen_server_module(ModuleName),
    #child_spec{
       name=ModuleName,
       mfa={ChildModule, start_link, [Args]}
      }.


%% When a module does not implement a gen_server then we use a dummy gen_server.
gen_server_module(M) ->
    case has_behaviour(M, gen_server) orelse has_behaviour(M, supervisor) of
        true -> M;
        false -> z_module_dummy
    end.

has_behaviour(M, Behaviour) ->
    case proplists:get_value(behaviour, erlang:get_module_info(M, attributes)) of
        L when is_list(L) ->
            lists:member(Behaviour, L);
        undefined ->
            false
    end.


%% @doc Check whether given module is valid for the given site
%% @spec valid(atom(), #context{}) -> bool()
valid(M, Context) ->
    lists:member(M, [Mod || {Mod,_} <- scan(Context)]).


%% @doc Manage the upgrade/install of this module.
manage_schema(Module, Exports, Context) ->
    Target = mod_schema(Module),
    Current = db_schema_version(Module, Context),
    case {proplists:get_value(manage_schema, Exports), Target =/= undefined} of
        {undefined, false} ->
            ok; %% No manage_schema function, and no target schema
        {undefined, true} ->
            {error, {"Schema version defined in module but no manage_schema/2 function.", Module}};
        {2, _} ->
            %% Module has manage_schema function
            manage_schema_if_db(z_db:has_connection(Context), Module, Current, Target, Context)
    end.

%% @doc Fetch the list of exported functions of a module.
refresh_module_exports(Module, #state{module_exports=Exports} = State) ->
    Exports1 = lists:keydelete(Module, 1, Exports),
    State#state{
        module_exports=[{Module, erlang:get_module_info(Module, exports)} | Exports1]
    }.

%% @doc Fetch the list of exported functions of a module.
refresh_module_schema(Module, #state{module_schema=Schemas} = State) ->
    Schemas1 = lists:keydelete(Module, 1, Schemas),
    State#state{
        module_schema=[{Module, mod_schema(Module)} | Schemas1]
    }.

manage_schema_if_db(true, Module, Current, Target, Context) ->
    manage_schema(Module, Current, Target, Context);
manage_schema_if_db(false, Module, _Current, _Target, Context) ->
    lager:error("[~p] Skipping schema for ~p as there is no database connection.",
                [z_context:site(Context), Module]),
    ok.

%% @doc Database version equals target version; ignore
manage_schema(_Module, Version, Version, _Context) ->
    ok;

%% @doc Upgrading to undefined schema version..?
manage_schema(_Module, _Current, undefined, _Context) ->
    ok; %% or?: throw({error, {upgrading_to_empty_schema_version, Module}});

%% @doc Installing a schema
manage_schema(Module, undefined, Target, Context) ->
    SchemaRet = z_db:transaction(
                    fun(C) ->
                        Module:manage_schema(install, C)
                    end,
                    Context),
    case SchemaRet of
        #datamodel{} ->
            ok = z_datamodel:manage(Module, SchemaRet, Context);
        ok ->
            ok
    end,
    ok = set_db_schema_version(Module, Target, Context),
    z_db:flush(Context),
    ok;

%% @doc Attempting a schema downgrade.
manage_schema(Module, Current, Target, _Context) when
      is_integer(Current) andalso is_integer(Target)
      andalso Current > Target ->
    {error, {"Module downgrades currently not supported.", Module}};

%% @doc Do a single upgrade step.
manage_schema(Module, Current, Target, Context) when
      is_integer(Current) andalso is_integer(Target) ->
    SchemaRet = z_db:transaction(
                    fun(C) ->
                        Module:manage_schema({upgrade, Current+1}, C)
                    end,
                    Context),
    case SchemaRet of
        #datamodel{} ->
            ok = z_datamodel:manage(Module, SchemaRet, Context);
        ok ->
            ok
    end,
    ok = set_db_schema_version(Module, Current+1, Context),
    z_db:flush(Context),
    manage_schema(Module, Current+1, Target, Context);

%% @doc Invalid version numbering
manage_schema(_, Current, Target, _) ->
    {error, {"Invalid schema version numbering", Current, Target}}.


%% @doc Add the observers for a module, called after module has been activated
add_observers(Module, Pid, State) ->
    {Module, Exports} = lists:keyfind(Module, 1, State#state.module_exports),
    lists:foreach(fun({Message, Handler}) ->
                      z_notifier:observe(Message, Handler, State#state.context)
                  end,
                  observes(Module, Exports,Pid)).

%% @doc Remove the observers for a module, called before module is deactivated
remove_observers(Module, Pid, State) ->
    {Module, Exports} = lists:keyfind(Module, 1, State#state.module_exports),
    lists:foreach(fun({Message, Handler}) ->
                      z_notifier:detach(Message, Handler, State#state.context)
                  end,
                  lists:reverse(observes(Module, Exports, Pid))).


%% @doc Get the list of events the module observes.
%% The event functions should be called: observe_(event)
%% observe_xxx/2 functions observer map/notify and observe_xxx/3 functions observe folds.
-spec observes(atom(), list({atom(), integer()}), pid()) -> [{atom(), {atom(),atom()}|{atom(), atom(), [pid()]}}].
observes(Module, Exports, Pid) ->
    observes_1(Module, Pid, Exports, []).

observes_1(_Module, _Pid, [], Acc) ->
    Acc;
observes_1(Module, Pid, [{F,Arity}|Rest], Acc) ->
    case atom_to_list(F) of
        "observe_" ++ Message when Arity == 2; Arity == 3 ->
            observes_1(Module, Pid, Rest, [{list_to_atom(Message), {Module,F}}|Acc]);
        "pid_observe_" ++ Message when Arity == 3; Arity == 4 ->
            observes_1(Module, Pid, Rest, [{list_to_atom(Message), {Module,F,[Pid]}}|Acc]);
        _ ->
            observes_1(Module, Pid, Rest, Acc)
    end;
observes_1(Module, Pid, [_|Rest], Acc) ->
    observes_1(Module, Pid, Rest, Acc).



%% @doc Reinstall the given module's schema, calling
%% Module:manage_schema(install, Context); if that function exists.
reinstall(Module, Context) ->
    case proplists:get_value(manage_schema, erlang:get_module_info(Module, exports)) of
        undefined ->
            %% nothing to do, no manage_schema
            nop;
        2 ->
            %% has manage_schema/2
            case Module:manage_schema(install, Context) of
                D=#datamodel{} ->
                    ok = z_datamodel:manage(Module, D, Context);
                ok -> ok
            end,
            ok = z_db:flush(Context)
    end.
