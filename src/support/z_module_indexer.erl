%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2012 Marc Worrell
%% Date: 2009-06-06
%%
%% @doc Implements the module extension mechanisms for scomps, templates, actions etc.  Scans all active modules
%% for scomps (etc) and maintains lookup lists for when the system tries to find a scomp (etc).

%% Copyright 2009-2012 Marc Worrell
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
    index_ref/1,
    translations/1,
    find/3,
    find_all/3,
    all/2
]).

-record(state, {
    context,
    scomps=[],
    actions=[],
    validators=[],
    models=[],
    templates=[],
    lib=[],
    services=[],
    scanner_pid=undefined
}).
-record(mfile, {name, filepath, module, erlang_module, prio}).

-include("zotonic.hrl").

-define(TIMEOUT, infinity).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link(Props) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(SiteProps) ->
    {site, Site} = proplists:lookup(site, SiteProps),
    Name = z_utils:name_for_site(?MODULE, Site),
    gen_server:start_link({local, Name}, ?MODULE, SiteProps, []).


%% @doc Reindex the list of all scomps, etc for the site in the context.
reindex(Context) ->
    gen_server:cast(Context#context.module_indexer, {module_ready, Context}).

index_ref(#context{} = Context) ->
    z_depcache:get(module_index_ref, Context).


%% @doc Find all .po files in all modules and the active site.
%% This is an active scan, not designed to be fast.
%% @spec translations(#context{}) -> [ {module, {ModuleDirectory, [{Language,File}]}} ]
-spec translations(#context{}) -> [ {Module :: atom(), [{Language :: atom(), filelib:filename()}]}].
translations(Context) ->
    translations1(Context).

%% @doc Find a scomp, validator etc.
%% @spec find(What, Name, Context) -> {ok, #module_index{}} | {error, Reason}
find(What, Name, Context) when What =:= lib; What =:= template ->
    case ets:lookup(?MODULE_INDEX,
                    #module_index_key{
                        site=z_context:site(Context),
                        type=What,
                        name=z_convert:to_binary(Name)
                    })
    of
        [] -> {error, enoent};
        [#module_index{} = M|_] -> {ok, M}
    end;
find(What, Name, Context) ->
    case ets:lookup(?MODULE_INDEX,
                    #module_index_key{
                        site=z_context:site(Context),
                        type=What,
                        name=Name
                    })
    of
        [] -> {error, enoent};
        [#module_index{} = M|_] -> {ok, M}
    end.


%% @doc Find a scomp, validator etc.
%% @spec find_all(What, Name, Context) -> list()
find_all(template, Name, Context) ->
    gen_server:call(Context#context.module_indexer, {find_all, template, z_convert:to_binary(Name)}, ?TIMEOUT);
find_all(What, Name, Context) ->
    gen_server:call(Context#context.module_indexer, {find_all, What, Name}, ?TIMEOUT).

%% @doc Return a list of all templates, scomps etc per module
all(What, Context) ->
    ActiveDirs = z_module_manager:active_dir(Context),
    [
        #module_index{
            key=#module_index_key{name=F#mfile.name},
            module=F#mfile.module,
            filepath=F#mfile.filepath,
            erlang_module=F#mfile.erlang_module
        }
        || F <- scan_all(What, ActiveDirs)
    ].


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
    {site, Site} = proplists:lookup(site, SiteProps),
    lager:md([
        {site, Site},
        {module, ?MODULE}
      ]),
    Context = z_context:new(Site),
    z_notifier:observe(module_ready, self(), Context),
    {ok, #state{context=Context}}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
handle_call({find_all, scomp, Name}, _From, State) ->
    {reply, lookup_all(Name, State#state.scomps), State};
handle_call({find_all, action, Name}, _From, State) ->
    {reply, lookup_all(Name, State#state.actions), State};
handle_call({find_all, validator, Name}, _From, State) ->
    {reply, lookup_all(Name, State#state.validators), State};
handle_call({find_all, model, Name}, _From, State) ->
    {reply, lookup_all(Name, State#state.models), State};
handle_call({find_all, service, Name}, _From, State) ->
    {reply, lookup_all(Name, State#state.services), State};

handle_call({find_all, lib, File}, _From, State) ->
    {reply, lookup_all(File, State#state.lib), State};
handle_call({find_all, template, File}, _From, State) ->
    {reply, lookup_all(File, State#state.templates), State};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Scan for all scomps etc. for the context given.

handle_cast({module_ready, _NotifyContext}, #state{scanner_pid=undefined}=State) ->
    flush(),

    % Start the scan in the background. Scanning can take a considerable amount
    % of time, especially on slower hardware.
    Self = self(),
    Pid = spawn_link(fun() ->
            Scanned = scan(State#state.context),
            gen_server:cast(Self, {scanned_items, Scanned})
        end),

    {noreply, State#state{scanner_pid=Pid}};

handle_cast({module_ready, _NotifyContext}, #state{scanner_pid=Pid}=State) when is_pid(Pid) ->
    %% The scanner is still busy, just let it continue.
    flush(),
    {noreply, State};

%% @doc Receive the scanned items.
handle_cast({scanned_items, Scanned}, State) ->
    State1 = State#state{scanner_pid=undefined},
    NewState = State1#state{
        scomps      = proplists:get_value(scomp, Scanned),
        actions     = proplists:get_value(action, Scanned),
        validators  = proplists:get_value(validator, Scanned),
        models      = proplists:get_value(model, Scanned),
        templates   = proplists:get_value(template, Scanned),
        lib         = proplists:get_value(lib, Scanned),
        services    = proplists:get_value(service, Scanned)
    },
    case NewState =/= State1 of
        true ->
            % Reindex the ets table for this host
            reindex_ets_lookup(NewState),

            % Reset the template server (and others) when there the index is changed.
            z_depcache:set(module_index_ref, erlang:make_ref(), NewState#state.context),
            z_notifier:notify(module_reindexed, NewState#state.context),
            z_depcache:flush(module_index, NewState#state.context),
            {noreply, NewState};
        false ->
            {noreply, State1}
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
    Dirs = [
        {priv, z_utils:lib_dir(priv)}          %% core modules translations
        | z_module_manager:active_dir(Context) %% other module translations
    ],
    POs = [{M,F} || #mfile{filepath=F, module=M} <- scan_subdir(translation, "translations", "", ".po", Dirs) ],

    ByModule = lists:foldl(fun({M,F}, Acc) ->
                                dict:append(M, F, Acc)
                           end,
                           dict:new(),
                           POs),
    [{M,tag_with_lang(POFiles)} || {M,POFiles} <- z_module_manager:prio_sort(dict:to_list(ByModule))].

tag_with_lang(POFiles) ->
    [{pofile_to_lang(POFile), POFile} || POFile <- POFiles].

pofile_to_lang(POFile) ->
    binary_to_atom(hd(binary:split(filename:basename(POFile), <<".">>)), 'utf8').

%% @doc Find all scomps etc in a lookup list
lookup_all(true, List) ->
    [
        #module_index{
            key=#module_index_key{name=F#mfile.name},
            module=F#mfile.module,
            filepath=F#mfile.filepath,
            erlang_module=F#mfile.erlang_module
        }
        || F <- List
    ];
lookup_all(Name, List) ->
    lookup_all1(Name, List, []).

lookup_all1(_Name, [], Acc) ->
    lists:reverse(Acc);
lookup_all1(Name, [#mfile{name=Name} = F|T], Acc) ->
    M = #module_index{
        key=#module_index_key{name=Name},
        module=F#mfile.module,
        filepath=F#mfile.filepath,
        erlang_module=F#mfile.erlang_module
    },
    lookup_all1(Name, T, [M|Acc]);
lookup_all1(Name, [_|T], Acc) ->
    lookup_all1(Name, T, Acc).


%% @doc Find the first template with matching name
lookup_first(_Name, []) ->
    {error, enoent};
lookup_first(Name, [#mfile{name=Name} = M|_]) ->
    {ok, M};
lookup_first(Name, [_|T]) ->
    lookup_first(Name, T).


%% @doc Scan the module directories for scomps, actions etc.
scan(Context) ->
    ActiveDirs = z_module_manager:active_dir(Context),
    [
        {What, scan_subdir(What, ActiveDirs)}
        || What <- [ template, lib, scomp, action, validator, model, service ]
    ].


%% @doc Scan module directories for specific kinds of parts. Returns a lookup list [ {lookup-name, fullpath} ]
scan_subdir(What, ActiveDirs) ->
    {Subdir, Prefix, Extension} = subdir(What),
    scan_subdir(What, Subdir, Prefix, Extension, ActiveDirs).

subdir(template)   -> { "templates",   "",           "" };
subdir(lib)        -> { "lib",         "",           "" };
subdir(translation)-> { "translations","",           ".po" };
subdir(scomp)      -> { "scomps",      "scomp_",     ".erl" };
subdir(action)     -> { "actions",     "action_",    ".erl" };
subdir(validator)  -> { "validators",  "validator_", ".erl" };
subdir(model)      -> { "models",      "m_",         ".erl" };
subdir(service)    -> { "services",    "service_",   ".erl" };
subdir(erlang)     -> { "support",     "",           ".erl" }.


%% @doc Find all files, for the all/2 function.
scan_all(What, ActiveDirs) ->
    {Subdir, Prefix, Extension} = subdir(What),
    scan_subdir(What, Subdir, Prefix, Extension, ActiveDirs).


%% @doc Scan all module directories for templates/scomps/etc.  Example: scan(scomp, "scomps", "scomp_", ".erl", Context)
%% @spec scan_subdir(What, Subdir, Prefix, Extension, context()) -> [ {ModuleAtom, {ModuleDir, [{Name, File}]}} ]
scan_subdir(What, Subdir, Prefix, Extension, ActiveDirs) ->
    ExtensionRe = case Extension of
                        "" -> "";
                        "."++_ -> "\\" ++ Extension ++ "$"
                  end,
    Scan1 = fun({Module, Dir}, Acc) ->
                scan_moddir(What, Module, Dir, Subdir, Prefix, Extension, ExtensionRe, Acc)
            end,
    lists:sort(fun mfile_compare/2, lists:flatten(lists:foldl(Scan1, [], ActiveDirs))).

scan_moddir(What, Module, Dir, Subdir, _Prefix, _Extension, _ExtensionRe, Acc)
    when What =:= template; What =:= lib ->
    case z_utils:list_dir_recursive(filename:join(Dir, Subdir)) of
        [] ->
            Acc;
        Files ->
            Prio = z_module_manager:prio(Module),
            [[
                #mfile{
                    filepath=iolist_to_binary(filename:join([Dir, Subdir, F])),
                    name=convert_name(What, F),
                    module=Module,
                    erlang_module=undefined,
                    prio=Prio
                }
                || F <- Files
            ] | Acc ]
    end;
scan_moddir(What, Module, Dir, Subdir, Prefix, Extension, ExtensionRe, Acc) ->
    {Dir1, Pattern, PrefixLen} =
            case Prefix of
                    [] ->
                        {filename:join([Dir, Subdir]), ".*" ++ ExtensionRe, 0};
                    _ ->
                        Prefix1 = Prefix ++ module2prefix(Module) ++ "_",
                        {filename:join([Dir, Subdir]), Prefix1 ++ ".*" ++ ExtensionRe, length(Prefix1)}
            end,
    Files = filelib:fold_files(Dir1, Pattern, true, fun(F1,Acc1) -> [F1 | Acc1] end, []),
    case Files of
        [] ->
            Acc;
        _  ->
            [[
                #mfile{
                    filepath=iolist_to_binary(F),
                    name=convert_name(What, scan_remove_prefix_ext(F, PrefixLen, Extension)),
                    module=Module,
                    erlang_module=opt_erlang_module(F, Extension),
                    prio=z_module_manager:prio(Module)
                }
                || F <- Files
            ] | Acc ]
    end.



convert_name(template, Name) -> z_convert:to_binary(Name);
convert_name(lib, Name) -> z_convert:to_binary(Name);
convert_name(_, Name) -> z_convert:to_atom(Name).

module2prefix(Module) ->
    case atom_to_list(Module) of
        "mod_" ++ Rest -> Rest;
        Name -> Name
    end.

scan_remove_prefix_ext(Filename, PrefixLen, Ext) ->
    Basename = filename:basename(Filename, Ext),
    lists:nthtail(PrefixLen, Basename).

opt_erlang_module(Filepath, ".erl") ->
    list_to_atom(filename:basename(Filepath, ".erl"));
opt_erlang_module(_Filepath, _Ext) ->
    undefined.


%% @doc Order function for #mfile records on module priority
mfile_compare(#mfile{prio=A}, #mfile{prio=A}) -> true;
mfile_compare(#mfile{prio=A}, #mfile{prio=B}) when A < B -> true;
mfile_compare(#mfile{prio=A}, #mfile{prio=B}) when A > B -> false.


%% @doc Flush all 'module_ready' messages in the message queue.  This is needed when waking up after sleep.
flush() ->
    receive
        {'$gen_cast', {module_ready, _Context}} -> flush()
    after
        0 -> ok
    end.


%% @doc Re-index the ets table holding all module indices
reindex_ets_lookup(State) ->
    Site = z_context:site(State#state.context),
    Now = os:timestamp(),
    templates_to_ets(State#state.templates, Now, Site),
    to_ets(State#state.lib, lib, Now, Site),
    to_ets(State#state.scomps, scomp, Now, Site),
    to_ets(State#state.actions, action, Now, Site),
    to_ets(State#state.validators, validator, Now, Site),
    to_ets(State#state.services, service, Now, Site),
    cleanup_ets(Now, Site).


%% @doc Re-index all non-templates
to_ets(List, Type, Tag, Site) ->
    to_ets(List, Type, Tag, Site, []).

to_ets([], _Type, _Tag, _Site, _Acc) ->
    ok;
to_ets([#mfile{name=Name, module=Mod, erlang_module=ErlMod, filepath=FP}|T], service, Tag, Site, Acc) ->
    K = #module_index{
        key=#module_index_key{
            site=Site,
            type=service,
            name=service_key(z_convert:to_binary(Mod), Name)
        },
        module=Mod,
        erlang_module=ErlMod,
        filepath=FP,
        tag=Tag
    },
    ets:insert(?MODULE_INDEX, K),
    to_ets(T, service, Tag, Site, [Name|Acc]);
to_ets([#mfile{name=Name, module=Mod, erlang_module=ErlMod, filepath=FP}|T], Type, Tag, Site, Acc) ->
    case lists:member(Name, Acc) of
        true ->
            to_ets(T, Type, Tag, Site, Acc);
        false ->
            K = #module_index{
                key=#module_index_key{
                    site=Site,
                    type=Type,
                    name=Name
                },
                module=Mod,
                erlang_module=ErlMod,
                filepath=FP,
                tag=Tag
            },
            ets:insert(?MODULE_INDEX, K),
            to_ets(T, Type, Tag, Site, [Name|Acc])
    end.

service_key(<<"mod_", Mod/binary>>, Name) ->
    {Mod, z_convert:to_binary(Name)};
service_key(Site, Name) ->
    {Site, z_convert:to_binary(Name)}.

% Place all templates in the ets table, indexed per device type
templates_to_ets(List, Tag, Site) ->
    Templates = lists:usort([ Name || #mfile{name=Name} <- List ]),
    templates_to_ets_1(Templates, List, Tag, Site).

templates_to_ets_1([], _List, _Tag, _Site) ->
    ok;
templates_to_ets_1([Name|T], List, Tag, Site) ->
    case lookup_first(Name, List) of
        {error, enoent} ->
            skip;
        {ok, #mfile{filepath=FP, module=Mod}} ->
            K = #module_index{
                key=#module_index_key{
                    site=Site,
                    type=template,
                    name=Name
                },
                module=Mod,
                erlang_module=undefined,
                filepath=FP,
                tag=Tag
            },
            ets:insert(?MODULE_INDEX, K)
    end,
    templates_to_ets_1(T, List, Tag, Site).


%% @doc Remove all ets entries for this host with an old tag
cleanup_ets(Tag, Site) ->
    cleanup_ets_1(ets:first(?MODULE_INDEX), Tag, Site, []).

cleanup_ets_1('$end_of_table', _Tag, _Site, Acc) ->
    [ ets:delete(?MODULE_INDEX, K) || K <- Acc ];
cleanup_ets_1(#module_index_key{site=Site} = K, Tag, Site, Acc) ->
    case ets:lookup(?MODULE_INDEX, K) of
        [#module_index{tag=Tag}] ->
            cleanup_ets_1(ets:next(?MODULE_INDEX, K), Tag, Site, Acc);
        _ ->
            cleanup_ets_1(ets:next(?MODULE_INDEX, K), Tag, Site, [K|Acc])
    end;
cleanup_ets_1(K, Tag, Site, Acc) ->
    cleanup_ets_1(ets:next(?MODULE_INDEX, K), Tag, Site, Acc).
