%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2018 Marc Worrell
%%
%% @doc Implements the module extension mechanisms for scomps, templates, actions etc.  Scans all active modules
%% for scomps (etc) and maintains lookup lists for when the system tries to find a scomp (etc).

%% Copyright 2009-2018 Marc Worrell
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
    new_ets/0,
    reindex/1,
    index_ref/1,
    translations/1,
    dispatch/1,
    find/3,
    find_all/3,
    all/2,
    all_files/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_fileindexer/include/zotonic_fileindexer.hrl").

-type key_type() :: template  | lib | filter | scomp | action | validator | model | dispatch | service.

-record(state, {
    context :: z:context(),
    scomps = [],
    actions = [],
    validators = [],
    models = [],
    templates = [],
    lib = [],
    services = [],
    scanner_pid = undefined :: pid() | undefined
}).
-record(mfile, {name, filepath, module, erlang_module, prio}).

-export_type([ key_type/0 ]).

-define(TIMEOUT, infinity).

%% Name of the global module index table
-define(MODULE_INDEX, 'zotonic$module_index').

%%====================================================================
%% API
%%====================================================================

-spec new_ets() -> ets:tid() | atom().
new_ets() ->
    ets:new(?MODULE_INDEX, [set, public, named_table, {keypos, #module_index.key}]).

%% @spec start_link(Props) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(SiteProps) ->
    {site, Site} = proplists:lookup(site, SiteProps),
    Name = z_utils:name_for_site(?MODULE, Site),
    gen_server:start_link({local, Name}, ?MODULE, SiteProps, []).


%% @doc Reindex the list of all scomps, etc for the site in the context.
-spec reindex(z:context()) -> ok.
reindex(#context{ module_indexer = ModuleIndexer } = Context) ->
    gen_server:cast(ModuleIndexer, {module_ready, Context}).

index_ref(#context{} = Context) ->
    z_depcache:get(module_index_ref, Context).


%% @doc Find all .po files in all modules and the active site.
%% This is an active scan, not designed to be fast.
-spec translations(z:context()) -> [ {Module :: atom(), [{Language :: atom(), file:filename()}]}].
translations(Context) ->
    translations1(Context).

%% @doc Find all dispatch files in all modules and the active site.
-spec dispatch(z:context()) -> [ file:filename() ].
dispatch(Context) ->
    dispatch1(Context).

%% @doc Find a scomp, validator etc.
-spec find( key_type(), binary()|atom(), z:context() ) -> {ok, #module_index{}} | {error, term()}.
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
-spec find_all( key_type(), binary()|atom(), z:context() ) -> list( #module_index{} ).
find_all(template, Name, Context) ->
    gen_server:call(Context#context.module_indexer, {find_all, template, z_convert:to_binary(Name)}, ?TIMEOUT);
find_all(What, Name, Context) ->
    gen_server:call(Context#context.module_indexer, {find_all, What, Name}, ?TIMEOUT).

%% @doc Return a list of all templates, scomps etc per module
all(What, #context{} = Context) ->
    ActiveApps = z_module_manager:active(Context),
    [
        #module_index{
            key = #module_index_key{ name = F#mfile.name },
            module = F#mfile.module,
            filepath = F#mfile.filepath,
            erlang_module = F#mfile.erlang_module
        }
        || F <- scan_apps(What, ActiveApps)
    ].

all_files(erlang, Module) ->
    Filename = <<(z_convert:to_binary(Module))/binary, ".erl">>,
    [
        #module_index{
            key = #module_index_key{name = Filename},
            module = Module,
            erlang_module = Module
        }
        | all_files1(erlang, Module)
    ];
all_files(Type, Module) ->
    all_files1(Type, Module).

all_files1(Type, Module) ->
    [
        #module_index{
            key = #module_index_key{name = F#mfile.name},
            module = F#mfile.module,
            filepath = F#mfile.filepath,
            erlang_module = F#mfile.erlang_module
        }
        || F <- scan_apps(Type, [ Module ])
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
    ActiveApps = [ zotonic_core | z_module_manager:active(Context) ],
    POs = lists:map(
        fun( #mfile{ filepath = F, module = M } ) ->
            {M, F}
        end,
        scan_apps(translation, ActiveApps)),
    ByModule = lists:foldl(
        fun({M,F}, Acc) ->
            dict:append(M, F, Acc)
        end,
        dict:new(),
        POs),
    lists:map(
        fun({M, POFiles}) ->
            {M, tag_with_lang(POFiles)}
        end,
        z_module_manager:prio_sort(dict:to_list(ByModule))).

dispatch1(Context) ->
    ActiveApps = [ zotonic_core | z_module_manager:active(Context) ],
    POs = lists:map(
        fun( #mfile{ filepath = F, module = M } ) ->
            {M, F}
        end,
        scan_apps(dispatch, ActiveApps)),
    ByModule = lists:foldl(
        fun({M,F}, Acc) ->
            dict:append(M, F, Acc)
        end,
        dict:new(),
        POs),
    lists:map(
        fun({M, DispatchFiles}) ->
            {M, DispatchFiles}
        end,
        z_module_manager:prio_sort(dict:to_list(ByModule))).

tag_with_lang(POFiles) ->
    [ {pofile_to_lang(POFile), POFile} || POFile <- POFiles ].

pofile_to_lang(POFile) ->
    erlang:binary_to_atom(hd(binary:split(filename:basename(POFile), <<".">>)), 'utf8').

%% @doc Find all scomps etc in a lookup list
lookup_all(true, List) ->
    [
        #module_index{
            key = #module_index_key{ name = F#mfile.name },
            module = F#mfile.module,
            filepath = F#mfile.filepath,
            erlang_module = F#mfile.erlang_module
        }
        || F <- List
    ];
lookup_all(Name, List) ->
    lookup_all1(Name, List, []).

lookup_all1(_Name, [], Acc) ->
    lists:reverse(Acc);
lookup_all1(Name, [ #mfile{ name=Name } = F | T ], Acc) ->
    M = #module_index{
        key = #module_index_key{name=Name},
        module = F#mfile.module,
        filepath = F#mfile.filepath,
        erlang_module = F#mfile.erlang_module
    },
    lookup_all1(Name, T, [ M | Acc ]);
lookup_all1(Name, [ _ | T ], Acc) ->
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
    ActiveApps = [ zotonic_core | z_module_manager:active(Context) ],
    lists:map(
        fun(What) ->
            {What, scan_apps(What, ActiveApps)}
        end,
        [ template, lib, scomp, action, validator, model, service ]).

scan_apps(What, Apps) ->
    {SubDir, FileRE} = subdir_pattern(What),
    scan_apps_subdir(What, SubDir, FileRE, Apps).

subdir_pattern(template)   -> { "priv/templates",    "" };
subdir_pattern(lib)        -> { "priv/lib",          "" };
subdir_pattern(dispatch)   -> { "priv/dispatch",     "" };
subdir_pattern(translation)-> { "priv/translations", "\\.po" };
subdir_pattern(scomp)      -> { "src/scomps",        "^scomp_(.*)\\.erl$" };
subdir_pattern(action)     -> { "src/actions",       "^action_(.*)\\.erl$" };
subdir_pattern(validator)  -> { "src/validators",    "^validator_(.*)\\.erl$" };
subdir_pattern(service)    -> { "src/services",      "^service_(.*)\\.erl$" };
subdir_pattern(model)      -> { "src/models",        "^m_.*\\.erl$" };
subdir_pattern(erlang)     -> { "src/support",       "\\.erl" }.


%% @doc Scan all apps for templates/scomps/etc.
-spec scan_apps_subdir(atom(), file:filename_all(), string(), list( atom() )) -> list( #mfile{} ).
scan_apps_subdir(What, Subdir, Pattern, Apps) ->
    Found = lists:foldl(
        fun(App, Acc) ->
            [ scan_app(What, Subdir, Pattern, App) | Acc ]
        end,
        [],
        Apps),
    lists:sort(fun mfile_compare/2, lists:flatten( Found )).

scan_app(What, Subdir, Pattern, App) ->
    MApp = z_module_manager:module_to_app(App),
    case zotonic_fileindexer:scan(MApp, Subdir, Pattern) of
        {ok, Files} ->
            AppPrefix = app2prefix(App),
            AppPrio = z_module_manager:prio(App),
            lists:map(
                fun(#fileindex{} = F) ->
                    #mfile{
                        filepath = F#fileindex.path,
                        name = convert_name(
                                    What, AppPrefix, Pattern,
                                    F#fileindex.basename, F#fileindex.rootname, F#fileindex.relpath),
                        module = App,
                        erlang_module = opt_erlang_module(What, F#fileindex.rootname),
                        prio = AppPrio
                    }
                end,
                Files);
        {error, _} ->
            []
    end.

convert_name(template, _AppPrefix, _Pattern, _Basename, _Rootname, RelPath) ->
    RelPath;
convert_name(lib, _AppPrefix, _Pattern, _Basename, _Rootname, RelPath) ->
    RelPath;
convert_name(translation, _AppPrefix, _Pattern, _Basename, _Rootname, RelPath) ->
    RelPath;
convert_name(_What, AppPrefix, Pattern, Basename, Rootname, _RelPath) ->
    case re:run(Basename, Pattern, [{capture, all_but_first, binary}]) of
        {match, [Name]} ->
            Name1 = drop_prefix(AppPrefix, Name),
            erlang:binary_to_atom(Name1, utf8);
        _ ->
            erlang:binary_to_atom(Rootname, utf8)
    end.

drop_prefix(Prefix, Name) ->
    case binary:split(Name, Prefix) of
        [<<>>, <<$_, Rest/binary>>] -> Rest;
        _ -> Name
    end.

opt_erlang_module(template, _) -> undefined;
opt_erlang_module(lib, _) -> undefined;
opt_erlang_module(translation, _) -> undefined;
opt_erlang_module(_, Rootname) -> binary_to_atom(Rootname, utf8).

app2prefix(App) ->
    case atom_to_binary(App, utf8) of
        <<"mod_", Rest/binary>> -> Rest;
        Name -> Name
    end.

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
    Tag = erlang:unique_integer(),
    templates_to_ets(State#state.templates, Tag, Site),
    to_ets(State#state.lib, lib, Tag, Site),
    to_ets(State#state.scomps, scomp, Tag, Site),
    to_ets(State#state.actions, action, Tag, Site),
    to_ets(State#state.validators, validator, Tag, Site),
    to_ets(State#state.services, service, Tag, Site),
    cleanup_ets(Tag, Site).


%% @doc Re-index all non-templates
to_ets(List, Type, Tag, Site) ->
    to_ets(List, Type, Tag, Site, []).

to_ets([], _Type, _Tag, _Site, _Acc) ->
    ok;
to_ets([#mfile{name=Name, module=Mod, erlang_module=ErlMod, filepath=FP}|T], service, Tag, Site, Acc) ->
    K = #module_index{
        key = #module_index_key{
            site = Site,
            type = service,
            name = service_key(z_convert:to_binary(Mod), Name)
        },
        module = Mod,
        erlang_module = ErlMod,
        filepath = FP,
        tag = Tag
    },
    ets:insert(?MODULE_INDEX, K),
    to_ets(T, service, Tag, Site, [Name|Acc]);
to_ets([#mfile{name=Name, module=Mod, erlang_module=ErlMod, filepath=FP}|T], Type, Tag, Site, Acc) ->
    case lists:member(Name, Acc) of
        true ->
            to_ets(T, Type, Tag, Site, Acc);
        false ->
            K = #module_index{
                key = #module_index_key{
                    site = Site,
                    type = Type,
                    name = Name
                },
                module = Mod,
                erlang_module = ErlMod,
                filepath = FP,
                tag = Tag
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
