%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2011 Marc Worrell
%% Date: 2009-06-03

%% @doc Module supervisor. Uses a z_supervisor.  Starts/restarts module processes.
%% @todo Take module dependencies into account when starting/restarting modules.

%% Copyright 2009-2011 Marc Worrell
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

%% External exports
%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% API exports
-export([
    upgrade/1,
    deactivate/2,
    activate/2,
    restart/2,
    active/1,
    active/2,
    active_dir/1,
    get_provided/1,
    get_modules/1,
    get_modules_status/1,
    whereis/2,
    all/1,
    scan/1,
    prio/1,
    prio_sort/1,
    dependency_sort/1,
    dependencies/1,
    startable/2,
    module_exists/1,
    title/1
]).

-include_lib("zotonic.hrl").

%% The default module priority
-define(MOD_PRIO, 500).

%% Give a module a minute to start
-define(MODULE_START_TIMEOUT, 60000).

%% Module manager state
-record(state, {context, sup, start_wait=none, start_queue=[], start_error=[]}).


%%====================================================================
%% API
%%====================================================================
%% @spec start_link(SiteProps::proplist()) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the module manager
start_link(SiteProps) ->
    Context = z_acl:sudo(z_context:new(proplists:get_value(host, SiteProps))),
    gen_server:start_link({local, name(Context)}, ?MODULE, [{context, Context} | SiteProps], []).


%% @spec upgrade(#context{}) -> ok
%% @doc Reload the list of all modules, add processes if necessary.
upgrade(Context) ->
    gen_server:cast(name(Context), upgrade).


%% @doc Deactivate a module. The module is marked as deactivated and stopped when it was running.
%% @spec deactivate(Module, #context{}) -> ok
deactivate(Module, Context) ->
    case z_db:q("update module set is_active = false, modified = now() where name = $1", [Module], Context) of
        1 -> upgrade(Context);
        0 -> ok
    end.


%% @doc Activate a module. The module is marked as active and started as a child of the module z_supervisor.
%% The module manager can be checked later to see if the module started or not.
%% @spec activate(Module, #context{}) -> void()
activate(Module, Context) ->
    Scanned = scan(Context),
    {Module, _Dirname} = proplists:lookup(Module, Scanned),
    F = fun(Ctx) ->
        case z_db:q("update module set is_active = true, modified = now() where name = $1", [Module], Ctx) of
            0 -> z_db:q("insert into module (name, is_active) values ($1, true)", [Module], Ctx);
            1 -> 1
        end
    end,
    1 = z_db:transaction(F, Context),
    upgrade(Context).


%% @doc Shortcut, restart a module by deactivating and reactivating the module.
restart(Module, Context) ->
    deactivate(Module, Context),
    activate(Module, Context).


%% @doc Return the list of active modules.
%% @spec active(#context{}) -> [ atom() ]
active(Context) ->
    case z_db:has_connection(Context) of
        true ->
            Modules = z_db:q("select name from module where is_active = true order by name", Context),
            [ z_convert:to_atom(M) || {M} <- Modules ];
        false ->
            case m_site:get(modules, Context) of
                L when is_list(L) -> L;
                _ -> []
            end
    end.



%% @doc Return whether a specific module is active.
%% @spec active(Module::atom(), #context{}) -> [ atom() ]
active(Module, Context) ->
    case z_db:has_connection(Context) of
        true ->
            case z_db:q("select true from module where name = $1 and is_active = true", [Module], Context) of
                [{true}] -> true;
                _ -> false
            end;
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


%% @doc Return the pid of a running module
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
scan(#context{host=Host}) ->
    Sites  = filename:join([z_utils:lib_dir(priv), "sites", Host, "modules", "mod_*"]),
    Priv  = filename:join([z_utils:lib_dir(priv), "modules", "mod_*"]),
    Src   = filename:join([z_utils:lib_dir(modules), "mod_*"]),
    Site  = filename:join([z_utils:lib_dir(priv), "sites", Host]),
    Files = filelib:wildcard(Sites) ++ filelib:wildcard(Priv) ++ filelib:wildcard(Src) ++ [Site],
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

%% @doc New child process started, add the event listeners
%% @todo When this is an automatic restart, start all depending modules
handle_cast({supervisor_child_started, ChildSpec, Pid}, State) ->
    Module = ChildSpec#child_spec.name,
    State1 = handle_start_child_result(Module, {ok, Pid}, State),
    {noreply, State1};

handle_cast({start_child_result, Module, Result}, State) ->
    State1 = handle_start_child_result(Module, Result, State),
    {noreply, State1};

%% @doc Existing child process stopped, remove the event listeners
handle_cast({supervisor_child_stopped, ChildSpec, Pid}, State) ->
    Module = ChildSpec#child_spec.name,
    remove_observers(Module, Pid, State#state.context),
    z_notifier:notify(#module_deactivate{module=Module}, State#state.context), 
    stop_children_with_missing_depends(State),
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


%% @doc Return the name for this site's module manager
name(Context) ->
    name(?MODULE, Context).
name(Module, #context{host=Host}) ->
    z_utils:name_for_host(Module, Host).


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
            State#state{start_queue=[]};
        _ ->
            gen_server:cast(self(), start_next),
            State#state{start_queue=StartList}
    end.

handle_start_next(#state{context=Context, start_queue=[]} = State) ->
    % Signal modules are loaded, and load all translations.
    z_notifier:notify(module_ready, Context),
    spawn_link(fun() -> z_trans_server:load_translations(Context) end),
    State;
handle_start_next(#state{context=Context, sup=ModuleSup, start_queue=Starting} = State) ->
    % Filter all children on the capabilities of the loaded modules.
    Provided = handle_get_provided(State),
    case lists:filter(fun(M) -> is_startable(M, Provided) end, Starting) of
        [] ->
            [
                ?ERROR("[~p] Error starting module ~p: ~p~n", 
                        [ z_context:site(Context), 
                          M, 
                          startable(M, Provided)
                        ])
                || M <- Starting
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
        [C|_] ->
            case start_child(self(), C, ModuleSup, module_spec(C, Context), Context) of
                {ok, StartHelperPid} -> 
                    State#state{
                        start_error=proplists:delete(C, State#state.start_error),
                        start_wait={C, StartHelperPid, now()},
                        start_queue=lists:delete(C, Starting)
                    };
                {error, Reason} -> 
                    handle_start_next(
                        State#state{
                            start_error=[ {C, Reason} | proplists:delete(C, State#state.start_error) ],
                            start_queue=lists:delete(C, Starting)
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
                ?ERROR("Can not fetch module info for module ~p, error: ~p:~p", [Module, M, E]),
                false
        end.

    %% @doc Try to add and start the child, do not crash on missing modules. Run as a separate process.
    %% @todo Add some preflight tests
    start_child(ManagerPid, Module, ModuleSup, Spec, Context) ->
        StartPid = spawn_link(
            fun() ->
                Result = case catch manage_schema(Module, Context) of
                            ok ->
                                % Try to start it
                                z_supervisor:start_child(ModuleSup, Spec#child_spec.name, ?MODULE_START_TIMEOUT);
                            Error ->
                                ?ERROR("[~p] Error starting module ~p, Schema initialization error:~n~p~n", 
                                        [z_context:site(Context), Module, Error]),
                                {error, {schema_init, Error}}
                         end,
                gen_server:cast(ManagerPid, {start_child_result, Module, Result})
            end
        ),
        {ok, StartPid}.



handle_start_child_result(Module, Result, State) ->
    % ?DEBUG({module_start_result, Module, Result}),
    % Where we waiting for this child? If so, start the next.
    State1 = case State#state.start_wait of
                {Module, _Pid, _NowStarted} ->
                    gen_server:cast(self(), start_next),
                    State#state{start_wait=none};
                _Other ->
                    % Check if there are any non-started modules that 
                    % depend on this module
                    State
             end,
    case Result of
        {ok, Pid} ->
            % Remove any registered errors for the started module
            State2 = State1#state{start_error=lists:keydelete(Module, 1, State1#state.start_error)},
            add_observers(Module, Pid, State2#state.context),
            z_notifier:notify(#module_activate{module=Module, pid=Pid}, State2#state.context),
            State2;
        {error, _Reason} = Error ->
            State1#state{
                start_error=[ {Module, Error} | lists:keydelete(Module, 1, State1#state.start_error) ]
            }
    end.


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
    Unstartable = lists:filter(fun(M) -> not is_startable(M, Provided) end, Modules),
    [ z_supervisor:stop_child(State#state.sup, M) || M <- Unstartable ].


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
    Args = [ {context, Context}, {module, ModuleName} | z_sites_manager:get_site_config(z_context:site(Context))],
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


%% @doc Check whether given module is valid for the given host
%% @spec valid(atom(), #context{}) -> bool()
valid(M, Context) ->
    lists:member(M, [Mod || {Mod,_} <- scan(Context)]).


%% @doc Manage the upgrade/install of this module.
manage_schema(Module, Context) ->
    Target = mod_schema(Module),
    Current = db_schema_version(Module, Context),
    case {proplists:get_value(manage_schema, erlang:get_module_info(Module, exports)), Target =/= undefined} of
        {undefined, false} ->
            ok; %% No manage_schema function, and no target schema
        {undefined, true} ->
            throw({error, {"Schema version defined in module but no manage_schema/2 function.", Module}});
        {2, _} ->
            %% Module has manage_schema function
            manage_schema(Module, Current, Target, Context)
    end.

%% @doc Database version equals target version; ignore
manage_schema(_Module, Version, Version, _Context) ->
    ok;

%% @doc Upgrading to undefined schema version..?
manage_schema(_Module, _Current, undefined, _Context) ->
    ok; %% or?: throw({error, {upgrading_to_empty_schema_version, Module}});

%% @doc Installing a schema
manage_schema(Module, undefined, Target, Context) ->
    F = fun(C) ->
                case Module:manage_schema(install, C) of
                    D=#datamodel{} ->
                        ok = z_datamodel:manage(Module, D, Context);
                    ok -> ok
                end,
                ok = set_db_schema_version(Module, Target, C),
                ok = z_db:flush(C)
        end,
    ok = z_db:transaction(F, Context);

%% @doc Attempting a schema downgrade.
manage_schema(Module, Current, Target, _Context) when
      is_integer(Current) andalso is_integer(Target)
      andalso Current > Target ->
    throw({error, {"Module downgrades currently not supported.", Module}});

%% @doc Do a single upgrade step.
manage_schema(Module, Current, Target, Context) when
      is_integer(Current) andalso is_integer(Target) ->
    F = fun(C) ->
                case Module:manage_schema({upgrade, Current+1}, C) of
                    D=#datamodel{} ->
                        ok = z_datamodel:manage(Module, D, Context);
                    ok -> ok
                end,
                ok = set_db_schema_version(Module, Current+1, C),
                ok = z_db:flush(C)
        end,
    ok = z_db:transaction(F, Context),
    manage_schema(Module, Current+1, Target, Context);

%% @doc Invalid version numbering
manage_schema(_, Current, Target, _) ->
    throw({error, {"Invalid schema version numbering", Current, Target}}).


%% @doc Add the observers for a module, called after module has been activated
add_observers(Module, Pid, Context) ->
    [ z_notifier:observe(Message, Handler, Context) || {Message, Handler} <- observes(Module, Pid) ].


%% @doc Remove the observers for a module, called before module is deactivated
remove_observers(Module, Pid, Context) ->
    [ z_notifier:detach(Message, Handler, Context) || {Message, Handler} <- lists:reverse(observes(Module, Pid)) ].


%% @doc Get the list of events the module observes.
%% The event functions should be called: observe_(event)
%% observe_xxx/2 functions observer map/notify and observe_xxx/3 functions observe folds.
%% @spec observes(atom(), pid()) -> [{atom(), Handler}]
observes(Module, Pid) ->
    observes(Module, Pid, erlang:get_module_info(Module, exports), []).
    
    observes(_Module, _Pid, [], Acc) ->
        Acc;
    observes(Module, Pid, [{F,Arity}|Rest], Acc) ->
        case atom_to_list(F) of
            "observe_" ++ Message when Arity == 2; Arity == 3 ->
                observes(Module, Pid, Rest, [{list_to_atom(Message), {Module,F}}|Acc]);
            "pid_observe_" ++ Message when Arity == 3; Arity == 4 ->
                observes(Module, Pid, Rest, [{list_to_atom(Message), {Module,F,[Pid]}}|Acc]);
            _ -> 
                observes(Module, Pid, Rest, Acc)
        end;
    observes(Module, Pid, [_|Rest], Acc) -> 
        observes(Module, Pid, Rest, Acc).

