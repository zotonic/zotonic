%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-06-06
%%
%% @doc Implements the module extension mechanisms for scomps, templates, actions etc.  Scans all active modules
%% for scomps (etc) and maintains lookup lists for when the system tries to find a scomp (etc).

%% Copyright 2009 Marc Worrell
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

-module(z_module_indexer).
-author("Marc Worrell <marc@worrell.nl").
-behaviour(gen_server).


%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
    reindex/1,
    translations/1,
    find/3,
    find_all/3,
    all/2
]).

-record(state, {context, scomps=[], actions=[], validators=[], models=[], templates=[], lib=[], services=[]}).

-include("zotonic.hrl").

%%====================================================================
%% API
%%====================================================================
%% @spec start_link(Props) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(SiteProps) ->
    {host, Host} = proplists:lookup(host, SiteProps),
    Name = z_utils:name_for_host(?MODULE, Host),
    gen_server:start_link({local, Name}, ?MODULE, SiteProps, []).

    
%% @doc Reindex the list of all scomps, etc for the site in the context.
reindex(Context) ->
    gen_server:cast(Context#context.module_indexer, {module_ready, Context}).


%% @doc Find all .po files in all modules and the active site.
%% This is an active scan, not designed to be fast.
%% @spec translations(#context{}) -> [ {module, {ModuleDirectory, [{Language,File}]}} ]
translations(Context) ->
    translations1(Context).

%% @doc Find a scomp, validator etc.
%% @spec find(What, Name, Context) -> {ok, term()} | {error, Reason}
find(What, Name, Context) ->
    gen_server:call(Context#context.module_indexer, {find, What, Name}).


%% @doc Find a scomp, validator etc.
%% @spec find_all(What, Name, Context) -> list()
find_all(What, Name, Context) ->
    gen_server:call(Context#context.module_indexer, {find_all, What, Name}).

%% @doc Return a list of all templates, scomps etc per module
all(What, Context) ->
    scan_all(What, Context).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(SiteProps) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(SiteProps) ->
    process_flag(trap_exit, true),
    Context = z_context:new(proplists:get_value(host, SiteProps)),
    z_notifier:observe(module_ready, self(), Context),
    {ok, #state{context=Context}}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Find a template definition
handle_call({find, scomp, Name}, _From, State) ->
    {reply, lookup(Name, State#state.scomps), State};
handle_call({find, action, Name}, _From, State) ->
    {reply, lookup(Name, State#state.actions), State};
handle_call({find, validator, Name}, _From, State) ->
    {reply, lookup(Name, State#state.validators), State};
handle_call({find, model, Name}, _From, State) ->
    {reply, lookup(Name, State#state.models), State};
handle_call({find, template, Name}, _From, State) ->
    {reply, lookup(Name, State#state.templates), State};
handle_call({find, lib, Name}, _From, State) ->
    {reply, lookup(Name, State#state.lib), State};
handle_call({find, service, Name}, _From, State) ->
    {reply, lookup(Name, State#state.services), State};

handle_call({find_all, scomp, Name}, _From, State) ->
    {reply, lookup_all(Name, State#state.scomps), State};
handle_call({find_all, action, Name}, _From, State) ->
    {reply, lookup_all(Name, State#state.actions), State};
handle_call({find_all, validator, Name}, _From, State) ->
    {reply, lookup_all(Name, State#state.validators), State};
handle_call({find_all, model, Name}, _From, State) ->
    {reply, lookup_all(Name, State#state.models), State};
handle_call({find_all, template, Name}, _From, State) ->
    {reply, lookup_all(Name, State#state.templates), State};
handle_call({find_all, lib, Name}, _From, State) ->
    {reply, lookup_all(Name, State#state.lib), State};
handle_call({find_all, service, Name}, _From, State) ->
    {reply, lookup_all(Name, State#state.services), State};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Scan for all scomps etc. for the context given.
handle_cast({module_ready, _NotifyContext}, State) ->
    flush(),
    Scanned = scan(State#state.context),
    State1 = State#state{
        scomps     = proplists:get_value(scomp, Scanned),
        actions    = proplists:get_value(action, Scanned),
        validators = proplists:get_value(validator, Scanned),
        models     = proplists:get_value(model, Scanned),
        templates  = proplists:get_value(template, Scanned),
        lib        = proplists:get_value(lib, Scanned),
        services   = proplists:get_value(service, Scanned)
    },
    case State =/= State1 of
        true ->
            % Reset the template server when the templates are changed
            z_template:reset(State1#state.context),
            {noreply, State1};
        false ->
            {noreply, State}
    end;


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
    z_notifier:detach(module_ready, self(), State#state.context),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

translations1(Context) ->
    [{M,lang_to_atom(X)} || {M,X} <- z_module_manager:prio_sort(scan_subdir("translations", "", ".po", Context))].

    lang_to_atom({ModDir,LangFiles}) -> 
        {ModDir, [{list_to_atom(extract_lang(Lang)), File} || {Lang,File} <- LangFiles]}.

    extract_lang(Basename) ->
        [Lang|_] = string:tokens(Basename, "."),
        Lang.


%% @doc Find a scomp etc in a lookup list
lookup(Name, List) ->
    case proplists:get_value(Name, List) of
        undefined ->
            {error, enoent};
        Result ->
            {ok, Result}
    end.

%% @doc Find all scomps etc in a lookup list
lookup_all(true, List) ->
    List;
lookup_all(Name, List) ->
    proplists:get_all_values(Name, List).


%% @doc Scan the module directories for scomps, actions etc.
scan(Context) ->
    [ {What, scan1(What, Context)} || What <- [ scomp, action, validator, model, template, lib, service ] ].


%% @doc Scan module directories for specific kinds of parts. Returns a lookup list [ {lookup-name, fullpath} ]
scan1(template, Context) ->
    scan_subdir_files("templates", Context);
scan1(lib, Context) ->
    scan_subdir_files("lib", Context);
scan1(What, Context) ->
    {Subdir, Prefix, Extension} = subdir(What),
    Scan = scan_subdir(Subdir, Prefix, Extension, Context), 
    Sorted = z_module_manager:prio_sort(Scan),
    FlattenFun = fun({_Module, {_ModuleDir, Files}}, Acc) ->
        Files1 = [ file2index(What, F) || F <- Files ],
        Files1 ++ Acc
    end,
    lists:foldr(FlattenFun, [], Sorted).

subdir(translation)-> { "translations","",           ".po" };
subdir(template)   -> { "templates",   "",           ".tpl" };
subdir(lib)        -> { "lib",         "",           "" };
subdir(scomp)      -> { "scomps",      "scomp_",     ".erl" };
subdir(action)     -> { "actions",     "action_",    ".erl" };
subdir(validator)  -> { "validators",  "validator_", ".erl" };
subdir(model)      -> { "models",      "m_",         ".erl" };
subdir(service)    -> { "services",    "service_",   ".erl" };
subdir(erlang)     -> { "",            "",           ".erl" }.

file2index(_, {NoPrefixExt, File}) ->
    ModuleName = list_to_atom(filename:basename(File, ".erl")),
    {list_to_atom(NoPrefixExt), ModuleName}.

%% @doc Find all files, for the all/2 function.
scan_all(What, Context) ->
    {Subdir, Prefix, Extension} = subdir(What),
    Scan = scan_subdir(Subdir, Prefix, Extension, Context), 

    z_module_manager:prio_sort(Scan).


%% @doc Scan the whole subdir hierarchy for files, used for templates and lib folders.
scan_subdir_files(Subdir, Context) ->
    Modules = z_module_manager:active_dir(Context),
    Scan1 = fun({Module, Dir}, Acc) ->
        case z_utils:list_dir_recursive(filename:join(Dir, Subdir)) of
            [] -> 
                Acc;
            Files -> 
                AbsFiles = [ {F, filename:join([Dir, Subdir, F])} || F <- Files ],
                [{z_module_manager:prio(Module), Module, AbsFiles} | Acc]
        end
    end,
    Files = lists:foldl(Scan1, [], Modules),
    Sorted = lists:sort(Files),
    lists:flatten([ Fs || {_Module, _Prio, Fs} <- Sorted ]).
    

%% @doc Scan all module directories for templates/scomps/etc.  Example: scan("scomps", "scomp_", ".erl", Context)
%% @spec scan_subdir(Subdir, Prefix, Extension, context()) -> [ {ModuleAtom, {ModuleDir, [{Name, File}]}} ]
scan_subdir(Subdir, Prefix, Extension, Context) ->
    Modules = z_module_manager:active_dir(Context),
    ExtensionRe = case Extension of [] -> []; "."++_ -> "\\" ++ Extension ++ "$" end,
    Scan1 = fun({Module, Dir}, Acc) ->
        {Dir1, Pattern, PrefixLen} = case Prefix of
            [] -> 
		{filename:join([Dir, Subdir]), ".*" ++ ExtensionRe, 0};
            _ ->
                Prefix1 = Prefix ++ module2prefix(Module) ++ "_",
		{filename:join([Dir, Subdir]), Prefix1 ++ ".*" ++ ExtensionRe, length(Prefix1)}
        end,
	Files = filelib:fold_files(Dir1, Pattern, true, fun(F1,Acc1) -> [F1 | Acc1] end, []),
        case Files of
            [] -> Acc;
            _  -> 
                Files1 = [ {scan_remove_prefix_ext(F, PrefixLen, Extension), F} || F <- Files ],
                Files2 = lists:filter(
                            fun
                                ( {[$.|_], _} ) -> false;
                                ( _ ) -> true
                            end,
                            Files1), 
                [{Module, {Dir, Files2}} | Acc]
        end
    end,
    lists:foldl(Scan1, [], Modules).
    
    module2prefix(Module) ->
        case atom_to_list(Module) of
            "mod_" ++ Rest -> Rest;
            Name -> Name
        end.

    scan_remove_prefix_ext(Filename, PrefixLen, Ext) ->
        Basename = filename:basename(Filename, Ext),
        lists:nthtail(PrefixLen, Basename).



%% @doc Flush all 'module_ready' messages in the message queue.  This is needed when waking up after sleep.
flush() ->
    receive
        {'$gen_cast', {module_ready, _Context}} -> flush()
    after 
        0 -> ok
    end.
