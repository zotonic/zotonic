%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2023 Marc Worrell
%% @doc Module manager, starts/restarts a site's modules.
%% @end

%% Copyright 2009-2023 Marc Worrell
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
    upgrade_await/1,
    deactivate/2,
    activate/2,
    activate_await/2,
    activate_precheck/2,
    deactivate_precheck/2,
    restart/2,
    module_reloaded/2,
    active/1,
    active/2,
    active_dir/1,
    lib_dir/1,
    module_to_app/1,
    is_provided/2,
    get_provided/1,
    scan_provided/1,
    scan_depending/1,
    get_modules/1,
    get_modules_status/1,
    get_upgrade_status/1,
    whereis/2,
    all/1,
    scan/1,
    scan/0,
    prio/1,
    prio_sort/1,
    dependency_sort/1,
    dependencies/1,
    startable/2,
    module_exists/1,
    mod_info/1,
    mod_version/1,
    mod_title/1,
    mod_description/1,
    mod_author/1,
    mod_schema/1,
    reinstall/2,
    sidejob_finish_start/1
]).

-include_lib("zotonic.hrl").

-type manage_schema() :: install
                       | {upgrade, ToVersion:: integer()}.

-type module_status() :: new
                       | starting
                       | running
                       | stopping
                       | restarting
                       | retrying
                       | failed
                       | stopped
                       | removing.

-record(module_status, {
    module :: atom(),
    application :: atom(),
    pid = undefined :: pid() | undefined,
    status = new :: module_status(),
    start_time = undefined :: undefined | pos_integer(),
    stop_time = undefined :: undefined | pos_integer(),
    crash_time = undefined :: undefined | erlang:timestamp(),
    crash_count = 0 :: integer()
}).

%% Module manager state
-record(state, {
    site :: atom(),
    module_exports = [] :: list({atom(), list({atom(),non_neg_integer()})}),
    module_schema = [] :: list({atom(), integer()|undefined}),
    start_wait  = none :: none | {atom(), pid(), erlang:timestamp()},
    start_queue = [] :: list(atom()),
    start_error = [] :: list({atom(), Reason::term()}),
    start_failed = [] :: list({Timestamp::pos_integer(), atom()}),
    upgrade_waiters = [] :: list({reference(), pid()}),
    module_monitors = #{} :: map(),  % pid() => atom()
    modules = #{} :: map() % atom() => #module_status{}
}).

-export_type([
    manage_schema/0,
    module_status/0
]).

%% The default module priority
-define(MOD_PRIO, 500).

% Backoff periods (in seconds) for restarting failed modules
-define(BACKOFF_SHORT, 1).
-define(BACKOFF_LONG, 60).
-define(BACKOFF_VERY_LONG, 600).

% Seconds after we declare a site as non-crashed and clear the backoff
% (Defaults to 5 minutes)
-define(PERIOD_CLEAR_CRASH, 300).

% Periodically check if we need to restat any failed modules
-define(FAILED_CHECK, 1000).

% Periodically force a garbage collect
-define(GC_INTERVAL, 60000).


%%====================================================================
%% API
%%====================================================================
%% @spec start_link(SiteProps::proplist()) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the module manager
start_link(Site) ->
    gen_server:start_link({local, name(Site)}, ?MODULE, Site, []).


%% @doc Reload the list of all modules, add processes if necessary.
-spec upgrade( z:context() ) -> ok.
upgrade(Context) ->
    flush(Context),
    gen_server:cast(name(Context), upgrade).

%% @doc Wait till all modules are started, used when starting up a new or test site.
-spec upgrade_await(z:context()) -> ok | {error, timeout}.
upgrade_await(Context) ->
    upgrade_await_1(Context, 20).

upgrade_await_1(_Context, 0) ->
    {error, timeout};
upgrade_await_1(Context, RetryCt) ->
    case erlang:whereis(name(Context)) of
        undefined ->
            timer:sleep(500),
            upgrade_await_1(Context, RetryCt-1);
        Pid ->
            gen_server:call(Pid, upgrade, infinity)
    end.

%% @doc Deactivate a module. The module is marked as deactivated and stopped when it was running.
-spec deactivate(atom(), z:context()) -> ok.
deactivate(Module, Context) ->
    flush(Context),
    case z_db:q("
            update module
            set is_active = false,
                modified = now()
            where name = $1",
            [Module],
            Context)
    of
        1 -> upgrade(Context);
        0 -> ok
    end.


%% @doc Before activating a module, check if it depends on non-activated other modules.
%% This checks the complete graph of all modules that are currently activated plus the new
%% modules. All missing dependencies will be listed.
%% This also shows the modules that have missing dependencies and can't run because of those
%% missing dependencies.
-spec activate_precheck( atom() | list( atom() ), z:context() ) ->
          ok
        | {error, #{ atom() => list( atom() ) }}
        | {error, {cyclic, z_toposort:cycles()}}.
activate_precheck(Module, Context) when is_atom(Module) ->
    activate_precheck([ Module ], Context);
activate_precheck(Modules, Context) when is_list(Modules) ->
    Active = active(Context),
    Active1 = Active ++ Modules,
    case dependency_sort(Active1) of
        {ok, Sorted} ->
            activate_precheck_1(Sorted, [], #{});
        {error, {cyclic, _}} = Error ->
            Error
    end.

-spec activate_precheck_1(list( atom() ), list( atom() ), map() ) -> ok | {error, map()}.
activate_precheck_1([], _Provided, Acc) ->
    case maps:size(Acc) of
        0 -> ok;
        _ -> {error, Acc}
    end;
activate_precheck_1([ M | Ms ], Provided, Acc) ->
    {M, MDep, MProv} = dependencies(M),
    Missing = MDep -- Provided,
    Provided1 = Provided ++ MProv,
    Acc1 = case Missing of
        [] -> Acc;
        _ -> Acc#{ M => Missing }
    end,
    activate_precheck_1(Ms, Provided1, Acc1).


%% @doc Before deactivating a module, check if active modules depend on the deactivated module.
%% This checks the complete graph of all modules that are currently activated minus the deactivated
%% module. All missing dependencies will be listed.
%% This also shows the modules that have missing dependencies and can't run because of those
%% missing dependencies.
-spec deactivate_precheck( atom(), z:context() ) ->
          ok
        | {error, #{ atom() => list( atom() ) }}
        | {error, {cyclic, z_toposort:cycles()}}.
deactivate_precheck(Module, Context) when is_atom(Module) ->
    Active = active(Context),
    Active1 = Active -- [ Module ],
    case dependency_sort(Active1) of
        {ok, Sorted} ->
            activate_precheck_1(Sorted, [], #{});
        {error, {cyclic, _}} = Error ->
            Error
    end.

%% @doc Activate a module. The module is marked as active and started as a child of the module supervisor.
%% The module manager can be checked later to see if the module started or not.
-spec activate(atom(), z:context()) -> ok | {error, not_found}.
activate(Module, Context) when is_atom(Module) ->
    activate(Module, false, Context).

-spec activate_await(atom(), z:context()) -> ok | {error, not_active | not_found}.
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
                    {error, not_active};
                {error, not_found} ->
                    {error, not_found}
            end;
        {error, _} = Error ->
            Error
    end.

activate(Module, IsSync, #context{site=Module} = Context) ->
    flush(Context),
    activate_1(Module, IsSync, Context);
activate(Module, IsSync, Context) ->
    flush(Context),
    case proplists:is_defined(Module, scan(Context)) of
        true -> activate_1(Module, IsSync, Context);
        false ->
            ?zError(
                "Could not find module '~p'",
                [Module],
                Context),
            {error, not_found}
    end.

activate_1(Module, IsSync, Context) ->
    F = fun(Ctx) ->
            case z_db:q("
                update module
                set is_active = true,
                    modified = now()
                where name = $1",
                [Module],
                Ctx)
            of
                0 ->
                    z_db:q("
                        insert into module (name, is_active)
                        values ($1, true)
                        ",
                        [Module],
                        Ctx);
                1 -> 1
            end
        end,
    1 = z_db:transaction(F, Context),
    UId = z_acl:user(Context),
    ?zInfo(
        "Module ~p activated by ~p (~s)",
        [ Module, UId, z_convert:to_binary( m_rsc:p_no_acl(UId, email, Context) ) ],
        Context),
    case IsSync of
        true -> upgrade_await(Context);
        false -> upgrade(Context)
    end.


%% @doc Restart a module, activates the module if it was not activated.
-spec restart(Module::atom(), #context{}) -> ok | {error, not_found}.
restart(Module, Context) ->
    case z_db:q1("
            select is_active
            from module
            where name = $1",
            [Module],
            Context)
    of
        true -> gen_server:cast(name(Context), {restart_module, Module});
        _ -> activate(Module, Context)
    end.

%% @doc Check all observers of a module, ensure that they are all active.
%%      Used after a module has been reloaded
-spec module_reloaded(Module::atom(), #context{}) -> ok.
module_reloaded(Module, Context) ->
    case z_db:q1("
        select is_active
        from module
        where name = $1",
        [Module],
        Context)
    of
        true -> gen_server:cast(name(Context), {module_reloaded, Module});
        _ -> ok
    end.

%% @doc Return the list of active modules.
-spec active(#context{}) -> list(Module::atom()).
active(Context) ->
    case z_db:has_connection(Context) of
        true ->
            F = fun() ->
                Modules = z_db:q("
                        select name
                        from module
                        where is_active = true
                        order by name",
                        Context),
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
                case z_db:q1("
                    select is_active
                    from module
                    where name = $1",
                    [Module],
                    Context)
                of
                    true -> true;
                    false -> false;
                    undefined -> false
                end
            end,
            z_depcache:memo(F, {?MODULE, {active, Module}, z_context:site(Context)}, Context);
        false ->
            lists:member(Module, active(Context))
    end.


%% @doc Return the list of all active modules and their directories
-spec active_dir(z:context()) -> [ {Module::atom(), Dir::file:filename_all()} ].
active_dir(Context) ->
    lists:foldr(
        fun(Module, Acc) ->
            case lib_dir(Module) of
                {error, bad_name} -> Acc;
                Dirname when is_list(Dirname) -> [ {Module, Dirname} | Acc ]
            end
        end,
        [],
        active(Context)).

-spec lib_dir(atom()) -> {error, bad_name} | file:filename().
lib_dir(Module) when is_atom(Module) ->
    code:lib_dir(module_to_app(Module)).

-spec module_to_app(ModuleName) -> Module when
    ModuleName :: binary() | atom() | string(),
    Module :: module().
module_to_app(<<"mod_", _/binary>> = M) ->
    binary_to_atom(<<"zotonic_", M/binary>>, utf8);
module_to_app("mod_" ++ _ = M) ->
    list_to_atom("zotonic_" ++ M);
module_to_app(Module) when is_atom(Module) ->
    case atom_to_list(Module) of
        "mod_" ++ _ = M ->
            list_to_atom("zotonic_"++M);
        _ ->
            Module
    end;
module_to_app(Module) ->
    z_convert:to_atom(Module).

-spec module_to_mod(ModuleName) -> Module when
    ModuleName :: binary() | atom() | string(),
    Module :: module().
module_to_mod(ModuleName) ->
    App = module_to_app(ModuleName),
    case atom_to_binary(App, utf8) of
        <<"zotonic_mod_", _/binary>> = B ->
            <<"zotonic_", M/binary>> = B,
            binary_to_atom(M, utf8);
        _ ->
            App
    end.

%% @doc Return the list of all modules running.
get_modules(Context) ->
    gen_server:call(name(Context), get_modules).


%% @doc Check if a service is provided by any module.
-spec is_provided( atom(), z:context() ) -> boolean().
is_provided(Service, Context) ->
    gen_server:call(name(Context), {is_provided, Service}).

%% @doc Return the list of all provided functionalities in running modules.
-spec get_provided( z:context() ) -> list( atom() ).
get_provided(Context) ->
    gen_server:call(name(Context), get_provided).


%% @doc Return a table with per provision which modules provide it.
-spec scan_provided( z:context() ) -> #{ atom() := [ atom() ]}.
scan_provided(Context) ->
    lists:foldl(
        fun({Module, _App, _Dir}, Acc) ->
            {_Mod, _Deps, Provs} = dependencies(Module),
            lists:foldl(
                fun(Prov, PAcc) ->
                    ProvidedBy = maps:get(Prov, PAcc, []),
                    PAcc#{ Prov => [ Module | ProvidedBy ]}
                end,
                Acc,
                lists:usort( [ Module | Provs ] ))
        end,
        #{},
        scan(Context)).

%% @doc Return a table with per dependeny which modules depend on it.
-spec scan_depending( z:context() ) -> #{ atom() := [ atom() ]}.
scan_depending(Context) ->
    lists:foldl(
        fun({Module, _App, _Dir}, Acc) ->
            {_Mod, Deps, _Provs} = dependencies(Module),
            lists:foldl(
                fun(Dep, DAcc) ->
                    DepOn = maps:get(Dep, DAcc, []),
                    DAcc#{ Dep => [ Module | DepOn ]}
                end,
                Acc,
                lists:usort( Deps ))
        end,
        #{},
        scan(Context)).


%% @doc Return the status of all enabled modules.
-spec get_modules_status( Context ) -> ModulesStatus when
    Context :: z:context(),
    ModulesStatus :: [ {atom(), module_status()} ].
get_modules_status(Context) ->
    gen_server:call(name(Context), get_modules_status).


%% @doc Return the status of any ongoing upgrade
get_upgrade_status(Context) ->
    gen_server:call(name(Context), get_upgrade_status).


%% @doc Return the pid of a running module
-spec whereis(atom(), z:context()) -> {ok, pid()} | {error, not_running | not_found}.
whereis(Module, Context) ->
    gen_server:call(name(Context), {whereis, Module}).


%% @doc Return the list of all modules in the database.
-spec all(z:context()) -> [ atom() ].
all(Context) ->
    Modules = z_db:q("
            select name
            from module
            order by name",
            Context),
    [ z_convert:to_atom(M) || {M} <- Modules ].


%% @doc Scan for a list of modules and the current site. A module is always an OTP application,
-spec scan( z:context() ) -> [ {Module :: atom(), Application :: atom(), Dir :: file:filename_all()} ].
scan(Context) ->
    Modules = scan(),
    Site = z_context:site(Context),
    [
        {Site, Site, code:lib_dir(Site)}
        | Modules
    ].

%% @doc Scan for a list of modules. A module is always an OTP application,
%% the name of the application is similar to the name of the module.
%% @todo Cache this
-spec scan() -> [ {Module :: atom(), Application :: atom(), Dir :: file:filename_all()} ].
scan() ->
    lists:flatten(
        lists:map(
            fun scan_path/1,
            [
                % All compiled modules
                filename:join(z_path:build_lib_dir(), "*zotonic_mod_*")
            ])
        ).

scan_path(Path) ->
    lists:foldl(
        fun
            ("." ++ _, Acc) ->
                Acc;
            (Dir, Acc) ->
                case filelib:is_dir(Dir) of
                    true ->
                        {Module, Application} = module_name(Dir),
                        [ {Module, Application, Dir} | Acc ];
                    false ->
                        Acc
                end
        end,
        [],
        filelib:wildcard(Path)).

%% @doc Strip prefix from module names iff there is a src/mod_thing.erl file.
-spec module_name( file:filename_all() ) -> {module(), atom()}.
module_name(Dir) ->
    ModName = filename:basename(Dir),
    SimpleModName = strip_module_namespace(z_convert:to_list(ModName)),
    case filelib:is_regular(filename:join([Dir, "src", SimpleModName++".erl"])) of
        true -> {list_to_atom(SimpleModName), list_to_atom(ModName)};
        false -> {list_to_atom(ModName), list_to_atom(ModName)}
    end.

strip_module_namespace("zotonic_mod_" ++ Name) -> "mod_" ++ Name;
strip_module_namespace("mod_" ++ _ = Mod) -> Mod;
strip_module_namespace([_C|Name]) -> strip_module_namespace(Name).


%% @doc Return the priority of a module. Default priority is 500, lower is higher priority.
%% Never crash on a missing module.
-spec prio(atom()) -> integer().
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
-spec prio_sort(list( atom() | {atom(), term()} )) -> list( atom() | {atom(), term()}).
prio_sort([{_,_}|_]=ModuleProps) ->
    WithPrio = [ {z_module_manager:prio(M), {M, X}} || {M, X} <- ModuleProps ],
    Sorted = lists:sort(WithPrio),
    [ X || {_Prio, X} <- Sorted ];
prio_sort(Modules) ->
    WithPrio = [ {z_module_manager:prio(M), M} || M <- Modules ],
    Sorted = lists:sort(WithPrio),
    [ M || {_Prio, M} <- Sorted ].


%% @doc Sort all modules on their dependencies (with sub sort the module's priority)
-spec dependency_sort( z:context() | list( atom() ) ) ->
        {ok, list( atom() )}
        | {error, {cyclic, z_toposort:cycles()}}.
dependency_sort(#context{} = Context) ->
    dependency_sort( active(Context) );
dependency_sort(Modules) when is_list(Modules) ->
    Ms = [ dependencies(M) || M <- prio_sort(Modules) ],
    z_toposort:sort(Ms).



%% @doc Return a module's dependencies as a tuple usable for z_toposort:sort/1.
-spec dependencies( {atom(), term()} | atom() ) ->
            {atom() | {atom(), term()}, Depends::list(atom()), Provides::list(atom())}.
dependencies({M, X}) when is_atom(M) ->
    {_, Ds, Ps} = dependencies(M),
    {{M,X}, Ds, Ps};
dependencies(M) when is_atom(M) ->
    try
        Info = erlang:get_module_info(M, attributes),
        Depends = proplists:get_value(mod_depends, Info, []),
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
            {Module, Depends, _Provides} = dependencies(Module),
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
        false ->
            {error, not_found}
    end.


get_start_error_reason({error, not_found}) ->
    "Module not found";
get_start_error_reason({error, {missing_dependencies, Missing}}) ->
    "Missing dependencies: " ++ binary_to_list(iolist_to_binary(io_lib:format("~p", [Missing]))).


%% @doc Check if the code of a module exists. The database can hold module references to non-existing modules.
module_exists(M) ->
    case code:ensure_loaded(M) of
        {module,M} -> true;
        {error, _} -> false
    end.

%% @doc Fetch information about a module or site.
-spec mod_info(Module) -> Info when
    Module :: atom() | binary() | string(),
    Info :: #{
        app := atom(),
        prio := integer(),
        version := binary() | undefined,
        schema := integer() | undefined,
        title := binary() | undefined,
        description := binary() | undefined,
        app_dir := file:filename_all() | undefined
    }.
mod_info(Module) ->
    App = module_to_app(Module),
    Mod = module_to_mod(App),
    LibDir = case code:lib_dir(App) of
        {error, _} -> undefined;
        Dir -> unicode:characters_to_binary(Dir)
    end,
    #{
        app => App,
        prio => prio(Mod),
        version => mod_version(App),
        schema => mod_schema(App),
        title => mod_title(App),
        description => mod_description(App),
        app_dir => LibDir
    }.

-spec mod_version(Module) -> Version when
    Module :: atom() | binary() | string(),
    Version :: binary().
mod_version(Module) ->
    App = module_to_app(Module),
    case application:get_key(App, vsn) of
        {ok, ""} ->
            app_git_version(App);
        {ok, "git"} ->
            app_git_version(App);
        {ok, Vsn} ->
            unicode:characters_to_binary(Vsn, utf8);
        undefined ->
            app_git_version(App)
    end.

app_git_version(App) ->
    case maybe_git_version(code:lib_dir(App)) of
        undefined ->
            maybe_git_version(z_path:site_source_dir(App));
        Vsn ->
            Vsn
    end.

maybe_git_version({error, _}) ->
    undefined;
maybe_git_version(LibDir) ->
    GitDir = filename:join(LibDir, ".git"),
    case filelib:is_dir(GitDir) of
        true ->
            git_version(LibDir);
        false ->
            undefined
    end.

git_version(LibDir) ->
    Cmd = "git rev-parse --short HEAD",
    case exec:run(Cmd, [sync, stdout, {cd, LibDir}]) of
        {ok, Res} ->
            {stdout, Hash} = proplists:lookup(stdout, Res),
            iolist_to_binary([
                "git-", z_string:trim(unicode:characters_to_binary(Hash))
            ]);
        {error, Reason} ->
            ?LOG_WARNING(#{
                in => zotonic_core,
                text => <<"Git rev-parse for module version failed">>,
                cmd => unicode:characters_to_binary(Cmd),
                lib_dir => LibDir,
                result => error,
                reason => Reason
            }),
            undefined
    end.

%% @doc Get the title of a module.
-spec mod_title(Module) -> Title when
    Module :: atom() | binary() | string(),
    Title :: binary() | undefined.
mod_title(Module) ->
    Mod = module_to_mod(Module),
    try
        Title = proplists:get_value(mod_title, Mod:module_info(attributes), <<>>),
        bin(Title)
    catch
        _M:_E -> undefined
    end.

%% @doc Get the description of a module.
-spec mod_description(Module) -> Desc when
    Module :: atom() | binary() | string(),
    Desc :: binary() | undefined.
mod_description(Module) ->
    Mod = module_to_mod(Module),
    try
        Desc = proplists:get_value(mod_description, Mod:module_info(attributes), <<>>),
        bin(Desc)
    catch
        _M:_E -> undefined
    end.

%% @doc Get the author of a module.
-spec mod_author(Module) -> Author when
    Module :: atom() | binary() | string(),
    Author :: binary() | undefined.
mod_author(Module) ->
    Mod = module_to_mod(Module),
    try
        Author = proplists:get_value(author, Mod:module_info(attributes), <<>>),
        bin(Author)
    catch
        _M:_E -> undefined
    end.

%% @doc Get the schema version of a module.
-spec mod_schema(Module) -> SchemaVersion when
    Module :: atom() | binary() | string(),
    SchemaVersion :: integer() | undefined.
mod_schema(Module) ->
    Mod = module_to_mod(Module),
    try
        {mod_schema, [SchemaVersion]} = proplists:lookup(mod_schema, Mod:module_info(attributes)),
        z_convert:to_integer(SchemaVersion)
    catch
        _M:_E -> undefined
    end.

db_schema_version(M, Context) ->
    z_db:q1("SELECT schema_version FROM module WHERE name = $1", [M], Context).

set_db_schema_version(M, V, Context) ->
    1 = z_db:q("UPDATE module SET schema_version = $1 WHERE name = $2", [V, M], Context),
    ok.


bin(A) when is_atom(A) ->
    atom_to_binary(A, utf8);
bin(A) when is_list(A) ->
    unicode:characters_to_binary(A);
bin(A) when is_binary(A) ->
    A.

%%====================================================================
%% gen_server callbacks
%%====================================================================


%% @doc Initiates the server.
init(Site) ->
    timer:send_interval(?GC_INTERVAL, force_gc),
    z_context:logger_md(Site),
    {ok, #state{ site = Site }}.

%% @doc Return a list of all modules
handle_call(get_modules, _From, #state{ modules = Modules } = State) ->
    {reply, maps:keys(Modules), State};

%% @doc Return the list of all provided services by the modules.
handle_call(get_provided, _From, State) ->
    Provided = handle_get_provided(State),
    {reply, Provided, State};

handle_call({is_provided, Service}, _From, State) ->
    IsProvided = lists:member(Service, handle_get_provided(State)),
    {reply, IsProvided, State};

%% @doc Return all running modules and their status
handle_call(get_modules_status, _From, #state{ modules = Modules } = State) ->
    Statuses = maps:fold(
        fun(Module, ModStatus, Acc) ->
            [ {Module, ModStatus#module_status.status} | Acc ]
        end,
        [],
        Modules),
    {reply, Statuses, State};

%% @doc Return the pid of a running module
handle_call({whereis, Module}, _From, #state{ modules = Modules } = State) ->
    Ret = case maps:find(Module, Modules) of
        {ok, #module_status{ pid = Pid }} when is_pid(Pid) ->
            {ok, Pid};
        {ok, _} ->
            {error, not_running};
        error ->
            {error, not_found}
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

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @doc Sync enabled modules with loaded modules
handle_cast(upgrade, State) ->
    State1 = handle_upgrade(State),
    {noreply, State1};

%% @doc Sync enabled modules with loaded modules
handle_cast(start_next, State) ->
    State1 = do_cleanup_removed_modules(State),
    State2 = handle_start_next(State1),
    {noreply, State2};

%% @doc Restart a running module.
handle_cast({restart_module, Module}, State) ->
    State1 = handle_restart_module(Module, State),
    {noreply, State1};

%% @doc Handle errors, success is handled by the supervisor_child_started above.
handle_cast({start_child_result, Module, Result}, #state{ site = Site } = State) ->
    ?zDebug(
        "Module ~p start result ~p",
        [Module, Result],
        z_context:new(Site)),
    State1 = handle_start_child_result(Module, Result, State),
    {noreply, State1};

%% @doc Check all observers of a module. Add new ones, remove non-existing ones.
%%      This is called after a code reload of a module.
handle_cast({module_reloaded, Module}, State) ->
    ?LOG_DEBUG(#{
        text => <<"Checking observers of (re-)loaded module">>,
        in => zotonic_core,
        module => Module
    }),
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
            ?LOG_NOTICE(#{
                text => <<"Exports or schema of (re-)loaded module changed, restarting module">>,
                in => zotonic_core,
                module => Module
            }),
            gen_server:cast(self(), {restart_module, Module}),
            {noreply, State}
    end;

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    ?LOG_ERROR(#{
        text => <<"z_module_manager: unknown cast">>,
        in => zotonic_core,
        message_in => Message,
        state => State
    }),
    {stop, {unknown_cast, Message}, State}.


%% @doc Handle down messages from modules.
handle_info({'DOWN', _MRef, process, Pid, Reason}, #state{ start_wait = {Module, Pid, _}, site = Site } = State) ->
    ?zDebug(
        "Module ~p start result ~p",
        [Module, {error, Reason}],
        z_context:new(Site)),
    State1 = handle_start_child_result(Module, {error, Reason}, State),
    {noreply, State1};

%% @doc Handle shutdown or crash of a module
handle_info({'DOWN', _MRef, process, Pid, Reason}, #state{ site = Site } = State) ->
    case maps:find(Pid, State#state.module_monitors) of
        {ok, Module} ->
            ?zInfo(
                "Module ~p stopped, reason ~p",
                [Module, Reason],
                z_context:new(Site)),
            State1 = do_module_down(Module, State, Pid, Reason),
            {noreply, State1};
        error ->
            {noreply, State}
    end;

handle_info(failed_restart, #state{ start_queue = [], modules = Modules } = State) ->
    State1 = do_cleanup_crash_state(State),
    case maps:fold(
        fun
            (_M, #module_status{}, true) ->
                true;
            (_M, #module_status{ status = failed, start_time = ST }, false) ->
                ST >= z_datetime:timestamp();
            (_M, #module_status{}, false) ->
                false
        end,
        false,
        Modules)
    of
        true -> self() ! upgrade;
        false -> ok
    end,
    flush_mbox(failed_restart),
    timer:send_after(?FAILED_CHECK, failed_restart),
    garbage_collect(),
    {noreply, State1};
handle_info(failed_restart, State) ->
    State1 = do_cleanup_crash_state(State),
    flush_mbox(failed_restart),
    timer:send_after(?FAILED_CHECK, failed_restart),
    {noreply, State1};

handle_info(upgrade, #state{ start_queue = [] } = State) ->
    flush_mbox(upgrade),
    State1 = handle_upgrade(State),
    {noreply, State1};
handle_info(upgrade, State) ->
    {noreply, State};
handle_info(force_gc, #state{ site = Site } = State) ->
    garbage_collect(),
    z_module_sup:gc(Site),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

%% @doc Return the name for this site's module manager
name(ContextOrSite) ->
    name(?MODULE, ContextOrSite).

name(Module, ContextOrSite) ->
    z_utils:name_for_site(Module, ContextOrSite).

flush(Context) ->
    z_depcache:flush({?MODULE, active, z_context:site(Context)}, Context).

% ----------------------------------------------------------------------------

do_module_down(Module, #state{ modules = Modules } = State, Pid, Reason) ->
    State1 = State#state{
        module_monitors = maps:remove(Pid, State#state.module_monitors),
        modules = do_module_down_1(Modules, maps:get(Module, Modules), Reason)
    },
    remove_observers(Module, Pid, State1),
    z_notifier:notify(
        #module_deactivate{ module = Module },
        z_acl:sudo(z_context:new(State1#state.site))),
    State2 = stop_children_with_missing_depends(State1),
    self() ! failed_restart,
    State2.


do_module_down_1(Modules, #module_status{ module = Mod, status = removing }, shutdown) ->
    ?LOG_DEBUG(#{
        text => <<"Module stopped">>,
        in => zotonic_core,
        old_status => removing,
        new_status => removed,
        result => ok,
        reason => shutdown,
        module => Mod
    }),
    maps:remove(Mod, Modules);
do_module_down_1(Modules, #module_status{ module = Mod, status = stopping } = Ms, shutdown) ->
    ?LOG_DEBUG(#{
        text => <<"Module stopped">>,
        in => zotonic_core,
        old_status => stopping,
        new_status => stopped,
        result => ok,
        reason => shutdown,
        module => Mod
    }),
    Ms1 = Ms#module_status{
        pid = undefined,
        status = stopped
    },
    Modules#{ Mod => Ms1 };
do_module_down_1(Modules, #module_status{ module = Mod, status = running } = Ms, normal) ->
    ?LOG_INFO(#{
        text => <<"Module stopped">>,
        in => zotonic_core,
        old_status => running,
        new_status => stopped,
        result => ok,
        reason => normal,
        module => Mod
    }),
    Ms1 = Ms#module_status{
        pid = undefined,
        status = stopped
    },
    Modules#{ Mod => Ms1 };
do_module_down_1(Modules, #module_status{ module = Mod, status = restarting } = Ms, shutdown) ->
    ?LOG_INFO(#{
        text => <<"Module shutdown during restart">>,
        in => zotonic_core,
        old_status => restarting,
        new_status => failed,       % set to failed to enforce restart
        result => ok,
        reason => shutdown,
        module => Mod,
        crash_count => 0
    }),
    Ms1 = Ms#module_status{
        pid = undefined,
        status = failed,
        start_time = start_backoff(0),
        crash_count = 0,
        crash_time = os:timestamp()
    },
    Modules#{ Mod => Ms1 };
do_module_down_1(Modules, #module_status{ module = Mod, status = Status } = Ms, Reason) ->
    ?LOG_ERROR(#{
        text => <<"Module failed">>,
        in => zotonic_core,
        old_status => Status,
        new_status => failed,
        result => error,
        reason => Reason,
        module => Mod,
        crash_count => Ms#module_status.crash_count + 1
    }),
    Ms1 = Ms#module_status{
        pid = undefined,
        status = failed,
        start_time = start_backoff(Ms#module_status.crash_count + 1),
        crash_count = Ms#module_status.crash_count + 1,
        crash_time = os:timestamp()
    },
    Modules#{ Mod => Ms1 }.

start_backoff(N) when N < 2 ->
    z_datetime:timestamp();
start_backoff(N) when N < 10 ->
    z_datetime:timestamp() + ?BACKOFF_SHORT;
start_backoff(N) when N < 20 ->
    z_datetime:timestamp() + ?BACKOFF_LONG;
start_backoff(_N) ->
    z_datetime:timestamp() + ?BACKOFF_VERY_LONG.

% ----------------------------------------------------------------------------

%% @doc If a module is running longer than ?PERIOD_CLEAR_CRASH seconds, then
%%      clear the crash count, assuming previous crashes are gone.
do_cleanup_crash_state(#state{ modules = Modules } = State) ->
    ClearTime = z_datetime:timestamp() - ?PERIOD_CLEAR_CRASH,
    Modules1 = maps:map(
        fun
            (_, #module_status{status = running, crash_count = N, start_time = T } = S)
                when N > 0, T < ClearTime ->
                S#module_status{ crash_count = 0 };
            (_, S) ->
                S
        end,
        Modules),
    State#state{ modules = Modules1 }.

do_cleanup_removed_modules(#state{ start_queue = Queue, modules = Ms } = State) ->
    Queue1 = lists:filter(
            fun(Module) -> maps:is_key(Module, Ms) end,
            Queue),
    State#state{ start_queue = Queue1 }.

% ----------------------------------------------------------------------------

handle_restart_module(Module, #state{ site = Site, modules = Modules } = State) ->
    case maps:find(Module, Modules) of
        {ok, #module_status{ status = running } = ModuleStatus} ->
            z_proc:spawn_md(
                fun() ->
                    z_module_sup:stop_module(Module, Site)
                end),
            Ms1 = ModuleStatus#module_status{ status = restarting },
            Modules1 = Modules#{ Module => Ms1 },
            handle_upgrade(State#state{ modules = Modules1 });
        {ok, _} ->
            handle_upgrade(State);
        error ->
            ?LOG_WARNING(#{
                text => <<"Restart of unknown module">>,
                in => zotonic_core,
                module => Module
            }),
            State
    end.

handle_upgrade(#state{ site = Site, modules = Modules } = State) ->
    Scan = scan(),
    ValidModules = valid_modules(Site, Scan),
    z_depcache:flush(z_modules, z_context:new(Site)),

    Old  = sets:from_list(maps:keys(Modules)),
    New  = sets:from_list(ValidModules),
    Kill = sets:subtract(Old, New),
    Create = sets:subtract(New, Old),

    Running = handle_get_running(State),
    Start = sets:to_list(sets:subtract(New, sets:from_list(Running))),
    StartOk = filter_startable_status(Start, Modules),
    {ok, StartList} = dependency_sort(StartOk),

    ?LOG_DEBUG(#{
        text => <<"Stopping/starting modules">>,
        in => zotonic_core,
        stopping => [sets:to_list(Kill)],
        starting => [StartList]
    }),

    Modules1 = sets:fold(
        fun (Module, MsAcc) ->
            #{ Module := ModuleStatus } = MsAcc,
            ModuleStatus1 = case ModuleStatus#module_status.pid of
                MPid when is_pid(MPid) ->
                    z_module_sup:stop_module(Module, Site),
                    ModuleStatus#module_status{
                        status = removing
                    };
                undefined ->
                    ModuleStatus#module_status{
                        status = stopped
                    }
            end,
            MsAcc#{ Module => ModuleStatus1 }
        end,
        State#state.modules,
        Kill),

    Modules2 = sets:fold(
        fun
            (Module, MsAcc) when Module =:= Site ->
                ModuleStatus = #module_status{
                    module = Site,
                    application = Site,
                    status = stopped,
                    stop_time = z_datetime:timestamp()
                },
                MsAcc#{ Module => ModuleStatus };
            (Module, MsAcc) ->
                {Module, ModApp, _ModDir} = lists:keyfind(Module, 1, Scan),
                ModuleStatus = #module_status{
                    module = Module,
                    application = ModApp,
                    status = stopped,
                    stop_time = z_datetime:timestamp()
                },
                MsAcc#{ Module => ModuleStatus }
        end,
        Modules1,
        Create),

    State1 = State#state{ modules = Modules2 },

    % 1. Put all to be started modules into a start list (add to State)
    % 2. Let the module manager start them one by one (if startable)
    % 3. Log any start errors, suppress modules that have errors.
    % 4. Log non startable modules (remaining after all startable modules have started)
    case {StartList, sets:size(Kill)} of
        {[], 0} ->
            signal_upgrade_waiters(State1#state{ start_queue = [] });
        _ ->
            gen_server:cast(self(), start_next),
            State1#state{ start_queue = StartList }
    end.

signal_upgrade_waiters(#state{upgrade_waiters = Waiters} = State) ->
    lists:foreach(
        fun(From) ->
            gen_server:reply(From, ok)
        end,
        Waiters),
    State#state{ upgrade_waiters = [] }.

%% @doc Run after a site has been started up. Notify to the site that all modules
%% are ready and load the translations.
-spec sidejob_finish_start(Site) -> ok when
    Site :: atom().
sidejob_finish_start(Site) ->
    Context = z_context:new(Site),
    ?zDebug("Finished starting modules", [], Context),
    z_notifier:notify_sync(module_ready, Context),
    z_trans_server:load_translations(Context).


handle_start_next(#state{site=Site, start_queue=[]} = State) ->
    % Signal modules are loaded, and load all translations.
    {ok, _} = z_sidejob:start({?MODULE, sidejob_finish_start, [Site]}),
    signal_upgrade_waiters(State);
handle_start_next(#state{site=Site, start_queue=Starting, modules=Modules} = State) ->
    % Filter all children on the capabilities of the loaded modules.
    Provided = handle_get_provided(State),
    case lists:filter(
            fun(M) -> is_startable(M, Provided, Modules) end,
            Starting)
    of
        [] ->
            lists:foreach(
                fun(M) ->
                    case startable(M, Provided) of
                        ok ->
                            % #{ M := MS } = Modules,
                            % ?DEBUG({MS#module_status.start_time - z_datetime:timestamp()}),
                            % Failed module in backoff state - ignore
                            ?zDebug("Could not start module ~p, reason 'failure backoff'",
                                    [M],
                                    z_context:new(Site));
                        Reason ->
                            % TODO: remove the broadcast and publish to topic
                            StartErrorReason = get_start_error_reason(Reason),
                            % Msg = iolist_to_binary(io_lib:format("Could not start ~p: ~s", [M, StartErrorReason])),
                            % z_session_manager:broadcast(
                            %     #broadcast{type="error", message=Msg, title="Module manager", stay=false},
                            %     z_acl:sudo(z_context:new(Site))),
                            ?zError("Could not start module ~p, reason ~s",
                                    [M, StartErrorReason],
                                    z_context:new(Site))
                    end
                end,
                Starting),

            % Add non-started modules to the list with errors.
            CleanedUpErrors = lists:foldl(
                fun(M,Acc) ->
                    proplists:delete(M,Acc)
                end,
                State#state.start_error,
                Starting),

            handle_start_next(
                State#state{
                    start_error = [
                        {M, case is_module(M) of false -> not_found; true -> dependencies_error end}
                        || M <- Starting
                    ] ++ CleanedUpErrors,
                    start_queue = []
                });
        [ Module | _ ] ->
            State1 = refresh_module_exports(Module, refresh_module_schema(Module, State)),
            #{ Module := ModuleStatus } = State1#state.modules,
            {ok, StartHelperPid} = start_child(
                    self(), Module, ModuleStatus#module_status.application,
                    module_spec(Module, Site), Site),
            State1#state{
                start_error=proplists:delete(Module, State1#state.start_error),
                start_wait={Module, StartHelperPid, os:timestamp()},
                start_queue=lists:delete(Module, Starting)
            }
    end.

%% @doc Check if all module dependencies are running.
is_startable(Module, Dependencies, Modules) ->
    #{ Module := ModuleStatus } = Modules,
    case is_startable_status(ModuleStatus) of
        true -> startable(Module, Dependencies) =:= ok;
        false -> false
    end.

is_startable_status(#module_status{ status = starting }) -> false;
is_startable_status(#module_status{ status = retrying }) -> false;
is_startable_status(#module_status{ status = restarting }) -> false;
is_startable_status(#module_status{ status = stopping }) -> false;
is_startable_status(#module_status{ status = removing }) -> false;
is_startable_status(#module_status{ status = failed, start_time = ST, crash_count = C}) when C > 0 ->
    Now = z_datetime:timestamp(),
    ST =< Now;
is_startable_status(_ModuleStatus) ->
    true.

filter_startable_status(Ms, Modules) ->
    lists:filter(
        fun(M) ->
            case maps:find(M, Modules) of
                {ok, ModuleStatus} -> is_startable_status(ModuleStatus);
                error -> true % assume new module
            end
        end,
        Ms).

%% @doc Check if we can load the module
is_module(Module) ->
    try
        {ok, _} = z_utils:ensure_existing_module(Module),
        true
    catch
        M:E ->
            ?LOG_ERROR(#{
                text => <<"Can not fetch module info for module">>,
                in => zotonic_core,
                module => Module,
                error => M,
                reason => E
            }),
            false
    end.

%% @doc Try to add and start the child, do not crash on missing modules. Run as a separate process.
%% @todo Add some preflight tests
-spec start_child(pid(), atom(), atom(), supervisor:child_spec(), atom()) -> {ok, pid()}.
start_child(ManagerPid, Module, App, ChildSpec, Site) ->
    StartPid = spawn(
        fun() ->
            Context = z_acl:sudo(z_context:new(Site)),
            ?LOG_DEBUG(#{
                text => <<"Starting module">>,
                in => zotonic_core,
                module => Module,
                app => App
            }),
            Result = case manage_schema(Module, Context) of
                ok ->
                    z_module_sup:start_module(App, ChildSpec, Site);
                {error, Reason} ->
                    ?LOG_ERROR(#{
                        text => <<"Error starting module due to schema initialization error">>,
                        in => zotonic_core,
                        module => Module,
                        result => error,
                        reason => Reason
                    }),
                    z:flush(Context),
                    {error, {schema_init, Reason}}
            end,
            gen_server:cast(ManagerPid, {start_child_result, Module, Result})
        end),
    erlang:monitor(process, StartPid),
    {ok, StartPid}.


handle_start_child_result(Module, {error, {already_started, Pid}}, State) ->
    handle_start_child_result(Module, {ok, Pid}, State);
handle_start_child_result(Module, Result, #state{ site = Site, module_monitors = Monitors } = State) ->
    % Were we waiting for this child? If so, start the next.
    State1 = case State#state.start_wait of
        {Module, _Pid, _NowStarted} ->
            gen_server:cast(self(), start_next),
            State#state{ start_wait = none };
        _Other ->
            State
    end,
    case Result of
        {ok, Pid} ->
            % Remove any registered errors for the started module
            erlang:monitor(process, Pid),
            #{ Module := Ms } = State1#state.modules,
            Ms1 = Ms#module_status{
                pid = Pid,
                status = running,
                start_time = z_datetime:timestamp()
            },
            State2 = State1#state{
                start_error = lists:keydelete(Module, 1, State1#state.start_error),
                module_monitors = Monitors#{ Pid => Module },
                modules = (State1#state.modules)#{ Module => Ms1 }
            },
            add_observers(Module, Pid, State2),
            Context = z_acl:sudo(z_context:new(Site)),
            z_notifier:notify(#module_activate{module=Module, pid=Pid}, Context),
            State2;
        {error, Reason} = Error ->
            ?LOG_ERROR(#{
                text => <<"Could not start module">>,
                in => zotonic_core,
                module => Module,
                result => error,
                reason => Reason
            }),
            State2 = do_module_down(Module, State1, undefined, Reason),
            State2#state{
                start_error = [
                    {Module, Error} | lists:keydelete(Module, 1, State1#state.start_error)
                ]
            }
    end.


%% @doc Return the list of all provided services.
handle_get_provided(State) ->
    get_provided_for_modules(handle_get_running(State)).

get_provided_for_modules(Modules) ->
    lists:flatten(
        lists:map(
            fun(M) ->
                {_, _, Provides} = dependencies(M),
                Provides
            end,
            Modules)).


stop_children_with_missing_depends(#state{ site = Site, modules = Modules } = State) ->
    Ms = maps:keys(Modules),
    Provided = get_provided_for_modules(Ms),
    case lists:filter(
        fun(M) -> not is_startable(M, Provided, State#state.modules) end,
        Ms)
    of
        [] ->
            State;
        Unstartable ->
            ?LOG_DEBUG(#{
                text => <<"Stopping child modules">>,
                in => zotonic_core,
                modules => Unstartable
            }),
            Modules1 = lists:foldl(
                fun(Module, ModAcc) ->
                    case maps:find(Module, ModAcc) of
                        {ok, #module_status{ status = failed }} ->
                            ModAcc;
                        {ok, #module_status{ status = S } = ModuleStatus}
                            when S =:= starting; S =:= stopping; S =:= retrying ->
                            ModuleStatus1 = ModuleStatus#module_status{
                                status = stopping
                            },
                            _ = z_module_sup:stop_module(Module, Site),
                            ModAcc#{ Module => ModuleStatus1 };
                        {ok, _ModuleStatus} ->
                            ModAcc;
                        error ->
                            ModAcc
                    end
                end,
                Modules,
                Unstartable),
            State#state{ modules = Modules1 }
    end.


%% @doc Return the list of module names currently up and running.
handle_get_running(State) ->
    maps:fold(
        fun
            (Module, #module_status{ status = running }, Acc) ->
                [ Module | Acc ];
            (_Module, _Status, Acc) ->
                Acc
        end,
        [],
        State#state.modules).

%% @doc Get a list of all valid modules
valid_modules(Site, Scan) ->
    Ms0 = lists:filter(fun module_exists/1, active(z_context:new(Site))),
    lists:filter(
        fun (Mod) ->
            Mod =:= Site orelse lists:keymember(Mod, 1, Scan)
        end,
        Ms0).

%% @doc Return the supervisor child spec for a module
module_spec(ModuleName, Site) ->
    {ok, Cfg} = z_sites_manager:get_site_config(Site),
    Context = z_context:new(Site),
    Args = [ {context, Context}, {module, ModuleName} | Cfg ],
    ChildModule = gen_server_module(ModuleName),
    case has_behaviour(ChildModule, supervisor) of
        true ->
            #{
                id => ModuleName,
                start => {ChildModule, start_link, [Args]},
                restart => temporary,
                type => supervisor
            };
        false ->
            #{
                id => ModuleName,
                start => {ChildModule, start_link, [Args]},
                restart => temporary,
                type => worker,
                modules => [ChildModule]
            }
    end.


%% When a module does not implement a gen_server then we use a dummy gen_server.
gen_server_module(M) ->
    case has_behaviour(M, gen_server) orelse has_behaviour(M, supervisor) of
        true -> M;
        false -> z_module_dummy
    end.

has_behaviour(M, Behaviour) ->
    case code:ensure_loaded(M) of
        {module, M} ->
            case proplists:get_value(behaviour, erlang:get_module_info(M, attributes)) of
                L when is_list(L) ->
                    lists:member(Behaviour, L);
                undefined ->
                    false
            end;
        {error, Reason} ->
            ?LOG_ERROR(#{
                text => <<"Could not load module">>,
                in => zotonic_core,
                module => M,
                result => error,
                reason => Reason
            }),
            false
    end.


%% @doc Manage the upgrade/install of this module.
manage_schema(Module, Context) ->
    Target = mod_schema(Module),
    Current = db_schema_version(Module, Context),
    HasManageSchema = erlang:function_exported(Module, manage_schema, 2),
    case {HasManageSchema, Target =/= undefined} of
        {false, false} ->
            ok; %% No manage_schema function, and no target schema
        {false, true} ->
            ?LOG_ERROR(#{
                text => <<"Schema version defined in module but no manage_schema/2 function">>,
                in => zotonic_core,
                module => Module,
                result => error,
                reason => no_manage_schema
            }),
            {error, {manage_schema, Module}};
        {true, _} ->
            %% Module has manage_schema function
            manage_schema_if_db(z_db:has_connection(Context), Module, Current, Target, Context)
    end.

%% @doc Fetch the list of exported functions of a module.
refresh_module_exports(Module, #state{module_exports=Exports} = State) ->
    Exports1 = lists:keydelete(Module, 1, Exports),
    State#state{
        module_exports = [
            {Module, lists:sort(erlang:get_module_info(Module, exports))}
            | Exports1
        ]
    }.

%% @doc Fetch the list of exported functions of a module.
refresh_module_schema(Module, #state{module_schema=Schemas} = State) ->
    Schemas1 = lists:keydelete(Module, 1, Schemas),
    State#state{
        module_schema=[{Module, mod_schema(Module)} | Schemas1]
    }.

manage_schema_if_db(true, Module, Current, Target, #context{} = Context) ->
    call_manage_schema(Module, Current, Target, Context);
manage_schema_if_db(false, Module, _Current, _Target, #context{}) ->
    ?LOG_INFO(#{
        text => <<"Skipping schema for module as the site has no database ('nodb')">>,
        in => zotonic_core,
        module => Module,
        result => skip,
        reason => nodb
    }),
    ok.

%% @doc Optionally upgrade the schema.
call_manage_schema(_Module, Version, Version, _Context) ->
    ok;
call_manage_schema(_Module, _Current, undefined, _Context) ->
    ok;
call_manage_schema(Module, Current, Target, _Context)
    when is_integer(Current), is_integer(Target), Target < Current ->
    % Downgrade
    ?LOG_ERROR(#{
        text => <<"Module downgrades not supported - continuing without schema modifications">>,
        in => zotonic_core,
        module => Module,
        result => error,
        reason => module_downgrade_unsupported,
        version_current => Current,
        version_target => Target
    }),
    ok;
call_manage_schema(Module, undefined, Target, Context) ->
    % New install
    SchemaRet = z_db:transaction(
                    fun(C) ->
                        Module:manage_schema(install, C)
                    end,
                    Context),
    datamodel_manage(Module, SchemaRet, Context),
    z_db:flush(Context),
    maybe_manage_data(Module, install, Context),
    ok = set_db_schema_version(Module, Target, Context),
    ok;
call_manage_schema(Module, Current, Target, Context)
    when is_integer(Current), is_integer(Target), Target > Current ->
    % Upgrade
    SchemaRet = z_db:transaction(
                    fun(C) ->
                        Module:manage_schema({upgrade, Current+1}, C)
                    end,
                    Context),
    datamodel_manage(Module, SchemaRet, Context),
    z_db:flush(Context),
    maybe_manage_data(Module, {upgrade, Current+1}, Context),
    ok = set_db_schema_version(Module, Current+1, Context),
    call_manage_schema(Module, Current+1, Target, Context);
call_manage_schema(Module, Current, Target, _) ->
    % Should be an integer (or undefined)\
    ?LOG_ERROR(#{
        text => <<"Invalid schema version numbering">>,
        in => zotonic_core,
        module => Module,
        result => error,
        reason => invalid_schema_number,
        version_current => Current,
        version_target => Target
    }),
    {error, {schema_numbering, Current, Target}}.

%% @doc After the manage_schema we can optionally install or modify data.
maybe_manage_data(Module, Version, Context) ->
    case erlang:function_exported(Module, manage_data, 2) of
        true -> Module:manage_data(Version, Context);
        false -> ok
    end.


%% @doc Add the observers for a module, called after module has been activated
add_observers(Module, Pid, #state{ site = Site } = State) ->
    {Module, Exports} = lists:keyfind(Module, 1, State#state.module_exports),
    lists:foreach(fun({Message, Handler}) ->
                      z_notifier:observe(Message, Handler, Pid, Site)
                  end,
                  observes(Module, Exports,Pid)).

%% @doc Remove the observers for a module, called before module is deactivated
remove_observers(_Module, undefined, #state{}) ->
    ok;
remove_observers(_Module, Pid, #state{ site = Site }) ->
    z_notifier:detach_all(Pid, Site).


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
        "observe_" ++ Message when Arity =:= 2; Arity =:= 3 ->
            observes_1(Module, Pid, Rest, [{list_to_atom(Message), {Module,F}}|Acc]);
        "pid_observe_" ++ Message when Arity =:= 3; Arity =:= 4 ->
            observes_1(Module, Pid, Rest, [{list_to_atom(Message), {Module,F,[Pid]}}|Acc]);
        _ ->
            observes_1(Module, Pid, Rest, Acc)
    end.



%% @doc Reinstall the given module's schema, calling
%% Module:manage_schema(install, Context); if that function exists.
reinstall(Module, Context) ->
    case proplists:get_value(manage_schema, erlang:get_module_info(Module, exports)) of
        undefined ->
            %% nothing to do, no manage_schema
            nop;
        2 ->
            %% has manage_schema/2
            SchemaRet = z_db:transaction(
                            fun(C) ->
                                Module:manage_schema(install, C)
                            end,
                            Context),
            datamodel_manage(Module, SchemaRet, Context),
            z_db:flush(Context),
            maybe_manage_data(Module, install, Context)
    end.

datamodel_manage(_Module, ok, _Context) ->
    ok;
datamodel_manage(Module, #datamodel{} = D, Context) ->
    ok = z_datamodel:manage(Module, D, Context);
datamodel_manage(Module, Ds, Context) when is_list(Ds) ->
    lists:foreach(fun(D) -> datamodel_manage(Module, D, Context) end, Ds).

flush_mbox(Msg) ->
    receive
        Msg -> flush_mbox(Msg)
    after 0 ->
        ok
    end.
