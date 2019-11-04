%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2012 Marc Worrell
%% @doc Template handling, compiles and renders django compatible templates using an extended version of erlydtl
%% @todo Make the template handling dependent on the host of the context (hosts have different modules enabled).

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

-module(z_template).
-behaviour(gen_server).
-author("Marc Worrell <marc@worrell.nl>").

-compile([{parse_transform, lager_transform}]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-include_lib("zotonic.hrl").

%% Max time comppiling a template is allowed to take.  Could be long after a cache flush or when
%% the server is on tight memory and is swapping.
-define(TIMEOUT, 30000).

%% External exports
-export([
    compile/2,
    render/2,
    render/3,
    render_to_iolist/3,
    find_template/2,
    find_template/3,
    find_template_cat/3,
    reset/1,
    module_reindexed/2,
    is_template_module/1,
    filename_to_modulename/2,
    filename_to_modulename/3
]).

-record(state, {reset_counter, host, do_modified_check, compile_queue, compile_job, compile_job_pid}).

%% Prefix for modules generated from templates.
-define(TEMPLATE_PREFIX, "template_").


start_link(SiteProps) ->
    {host, Host} = proplists:lookup(host, SiteProps),
    Name = z_utils:name_for_host(?MODULE, Host),
    gen_server:start_link({local, Name}, ?MODULE, SiteProps, []).


%% @doc Force a reset of all templates, used after a module has been activated or deactivated.
reset(Host) when is_atom(Host) ->
    Name = z_utils:name_for_host(?MODULE, Host),
    gen_server:cast(Name, module_reindexed);
reset(Context) ->
    gen_server:cast(Context#context.template_server, module_reindexed).

%% @doc Observer, triggered when there are new module files indexed
module_reindexed(module_reindexed, Context) ->
    reset(Context).


render(#render{} = Render, Context) ->
    render(Render#render.template, Render#render.vars, Context).


%% @spec render(File, Variables, Context) -> list()
%% @doc Render a template.  First requests the template module from the template server, then renders the template.
%% The resulting list contains the rendered template and scomp contexts.  Use render_to_iolist/3 to get a iolist().
render({cat, File}, Variables, Context) ->
    Id = proplists:get_value(id, Variables),
    maybe_render(find_template_cat(File, Id, Context), File, Variables, Context);
render({cat, File, [Cat|_] = IsA}, Variables, Context) when is_atom(Cat) ->
    maybe_render(find_template_cat(File, IsA, Context), File, Variables, Context);
render(#module_index{} = M, Variables, Context) ->
    render1(M#module_index.filepath, M, Variables, Context);
render(File, Variables, Context) ->
    maybe_render(find_template(File, Context), File, Variables, Context).

maybe_render({ok, ModuleIndex}, File, Variables, Context) ->
    render1(File, ModuleIndex, Variables, Context);
maybe_render({error, Reason}, File, _Variables, _Context) ->
    lager:info("Could not find template: ~s (~p)", [File, Reason]),
    throw({error, {template_not_found, File, Reason}}).

%% Render the found template
render1(File, #module_index{filepath=FoundFile, erlang_module=undefined}, Variables, Context) ->
    Module = filename_to_modulename(FoundFile, Context),
    render1(File, FoundFile, Module, Variables, Context);
render1(File, #module_index{filepath=FoundFile, erlang_module=Module}, Variables, Context) ->
    render1(File, FoundFile, Module, Variables, Context).

render1(File, FoundFile, Module, Variables, Context) ->
    Result = case gen_server:call(Context#context.template_server, {check_modified, Module}, ?TIMEOUT) of
                modified -> compile(File, FoundFile, Module, Context);
                ok -> {ok, Module}
             end,
    case Result of
        {ok, Module} ->
            %% @todo Move the in_process caching to the template compiler?
            OldCaching = z_depcache:in_process(true),
            case Module:render(Variables, Context) of
                {ok, Output}   ->
                    z_depcache:in_process(OldCaching),
                    runtime_wrap_debug_comments(FoundFile, Output, Context);
                {error, Reason} ->
                    z_depcache:in_process(OldCaching),
                    lager:error("Error rendering template: ~p (~p)~n", [FoundFile, Reason]),
                    throw({error, {template_rendering_error, FoundFile, Reason}})
             end;
        {error, {{ErrFile,Line,Col}, _YeccModule, Error}} ->
            Error1 = try lists:flatten(Error) catch _:_ -> Error end,
            lager:error("Error compiling template: ~s:~p (~p) ~s~n", [ErrFile, Line, Col, Error1]),
            throw({error, {template_compile_error, ErrFile, {Line,Col}, Error1}});
        {error, Reason} ->
            Reason1 = try lists:flatten(Reason) catch _:_ -> Reason end,
            lager:error("Error compiling template: ~p (~p)~n", [FoundFile, Reason1]),
            throw({error, {template_compile_error, FoundFile, Reason1}})
    end.


%% @doc Render a template to an iolist().  This removes all scomp state etc from the rendered html and appends the
%% information in the scomp states to the context for later rendering.
%% @spec render_to_iolist(File, Vars, Context) -> {iolist(), Context}
render_to_iolist(File, Vars, Context) ->
    Html = render(File, Vars, Context),
    z_render:render_to_iolist(Html, Context).


%% @spec compile(File, Context) -> {ok, atom()} | {error, Reason}
%% @doc Compile a template, return the module name.
compile(File, Context) ->
    case find_template(File, Context) of
        {ok, FoundFile} ->
            compile(File, FoundFile, Context);
        {error, Reason} ->
            {error, Reason}
    end.

compile(File, #module_index{filepath=FoundFile, erlang_module=Module}, Context) ->
    compile(File, FoundFile, Module, Context);
compile(File, FoundFile, Context) ->
    Module = filename_to_modulename(FoundFile, Context),
    compile(File, FoundFile, Module, Context).

compile(File, FoundFile, Module, Context) ->
    z_notifier:notify(#debug{what=template, arg={compile, File, FoundFile, Module}}, Context),
    % swap basenames for the File and FoundFile
    File1 = set_filename(File, FoundFile),
    gen_server:call(Context#context.template_server,
                    {compile, File1, FoundFile, Module, z_context:prune_for_async(Context)},
                    ?TIMEOUT).

set_filename(File, FoundFile) ->
    set_filename_1(filename:dirname(File), FoundFile).

set_filename_1(<<".">>, FoundFile) ->
    filename:basename(FoundFile);
set_filename_1(".", FoundFile) ->
    filename:basename(FoundFile);
set_filename_1(FilePath, FoundFile) ->
    filename:join([FilePath, filename:basename(FoundFile)]).

%% @spec find_template(File, Context) -> {ok, filename()} | {ok, #module_index{}} | {error, code}
%% @doc Finds the template designated by the file, check modules.
%% When the file is tagged with 'abs' path, then do nothing and assume the file exists.
find_template({abs, File}, _Context) ->
    {ok, #module_index{filepath=File}};
find_template(#module_index{} = ModuleIndex, _Context) ->
    {ok, ModuleIndex};
find_template(File, Context) ->
    z_module_indexer:find(template, File, Context).


%% @spec find_template(File, All, Context) -> [ #module_index{} ]
%% @doc Finds the first or all templates designated by the file, check modules.
find_template(File, false, Context) ->
    case z_module_indexer:find(template, File, Context) of
        {ok, #module_index{}=M} -> [M];
        {error, _Reason} -> []
    end;
find_template(File, true, Context) ->
    z_module_indexer:find_all(template, File, Context).


%% @spec find_template_cat(File, Id, Context) -> {ok, filename()} | {error, code}
%% @doc Finds the template designated by the file, for the category of the rsc with id, check modules.
%% When the file is an absolute path, then do nothing and assume the file exists.
find_template_cat(File, None, Context) when None =:= <<>>; None =:= undefined; None =:= [] ->
    find_template(File, Context);
find_template_cat(File, [Item|_]=Stack, Context) when is_atom(Item) ->
    find_template_cat_stack(File, Stack, Context);
find_template_cat(File, Id, Context) ->
    Stack = case {m_rsc:is_a(Id, Context), m_rsc:p(Id, name, Context)} of
                {L, undefined} -> L;
                {L, Name} -> L ++ [Name]
            end,
    find_template_cat_stack(File, Stack, Context).

find_template_cat_stack(File, Stack, Context) ->
    Root = z_convert:to_list(filename:rootname(File)),
    Ext = z_convert:to_list(filename:extension(File)),
    case lists:foldr(fun(Cat, {error, enoent}) ->
                            find_template(Root ++ [$.|z_convert:to_list(Cat)] ++ Ext, Context);
                        (_Cat, Found) ->
                            Found
                     end,
                     {error, enoent},
                     Stack)
    of
        {error, enoent} -> find_template(File, Context);
        {ok, ModuleIndex} -> {ok, ModuleIndex}
    end.


%% @doc Check if the module is a template module.
%% @spec is_template_module(atom()) -> bool()
is_template_module(Module) ->
    case z_convert:to_list(Module) of
        ?TEMPLATE_PREFIX ++ _ -> true;
        _ -> false
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initialize the template server, handles template compiles and rendering.
init(SiteProps) ->
    process_flag(trap_exit, true),
    Host = proplists:get_value(host, SiteProps),
    z_notifier:observe(module_reindexed, {?MODULE, module_reindexed}, z_context:new(Host)),
    Flag = z_config:get(template_modified_check, true),
    {ok, #state{
            reset_counter=z_utils:now_msec(),
            host=Host,
            compile_queue=[],
            do_modified_check=Flag
    }}.


%% @spec handle_call({check_modified, Module}, From, State) -> ok | modified
%% @doc Compile the template if it has been modified, return the template module for rendering.
handle_call({check_modified, Module}, _From, State) when Module =/= undefined ->
    Result = case template_is_modified(Module, State) of
                true  -> modified;
                false -> ok
             end,
    {reply, Result, State};


%% @doc Compile the template, loads the compiled template in memory.  Make sure that we only compile
%% one template at a time to prevent overloading the server on restarts.
handle_call({compile, _File, _FoundFile, Module, _Context} = Compile, From, State) ->
    case template_is_modified(Module, State) of
        true  ->
            {noreply, queue_compile(Compile, From, State)};
        false ->
            {reply, {ok, Module}, State}
    end.

%% @doc Reset all compiled templates, done by the module_indexer after the module list changed.
handle_cast(module_reindexed, State) ->
    {noreply, State#state{reset_counter=State#state.reset_counter+1}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle result of the compilation server
handle_info({compile_result, Pid, Result}, #state{compile_job_pid=Pid} = State) ->
    {value, {_Key,_Req,Wait}, CQ} = lists:keytake(State#state.compile_job, 1, State#state.compile_queue),
    lists:foreach(fun(From) ->
                     gen_server:reply(From, Result)
                  end,
                  Wait),
    {noreply, maybe_start_compile(State#state{compile_queue=CQ, compile_job=undefined, compile_job_pid=undefined})};

handle_info({'EXIT', Pid, Reason}, #state{compile_job_pid=Pid} = State) ->
    {value, {_Key,_Req,Wait}, CQ} = lists:keytake(State#state.compile_job, 1, State#state.compile_queue),
    lists:foreach(fun(From) ->
                     gen_server:reply(From, {error, {'EXIT', Reason}})
                  end,
                  Wait),
    {noreply, maybe_start_compile(State#state{compile_queue=CQ, compile_job=undefined, compile_job_pid=undefined})};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    z_notifier:detach(module_reindexed, {?MODULE, module_reindexed}, z_context:new(State#state.host)),
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% support functions
%%====================================================================


queue_compile({compile, File, FoundFile, Module, Context} = Req, From, State) ->
    Key = {
        File,
        FoundFile,
        Module,
        z_context:language(Context),
        Context#context.ua_class
    },
    CQ1 = case lists:keytake(Key, 1, State#state.compile_queue) of
            false ->
                [ {Key, Req, [From]} | State#state.compile_queue ];
            {value, {Key, Req0, Wait}, CQ} ->
                [ {Key, Req0, [From|Wait]} | CQ ]
          end,
    maybe_start_compile(State#state{compile_queue=CQ1}).

maybe_start_compile(#state{compile_queue=[]} = State) ->
    State;
maybe_start_compile(#state{compile_queue=CQ, compile_job_pid=undefined} = State) ->
    {Key,Job,_Wait} = hd(CQ),
    Self = self(),
    Pid = erlang:spawn_link(
                    fun() ->
                        compile_job(Self, Job, State#state.reset_counter)
                    end),
    State#state{compile_job=Key, compile_job_pid=Pid};
maybe_start_compile(State) ->
    State.

compile_job(Server, {compile, File, FoundFile, Module, Context}, ResetCounter) ->
    FinderFun  = fun(FinderFile, All) ->
                    [
                        case F of
                            #module_index{filepath=FP} -> FP;
                            {abs, FP} -> FP
                        end
                        || F <- ?MODULE:find_template(FinderFile, All, Context)
                    ]
                 end,
    Result = erlydtl:compile(FoundFile,
                             File,
                             Module,
                             [{finder, FinderFun}, {template_reset_counter, ResetCounter},
                              {debug_includes, get_debug_includes(Context)},
                              {debug_blocks, get_debug_blocks(Context)}
                             ],
                             Context),
    Server ! {compile_result, self(), Result}.



%% @doc Translate a filename to a module name
-spec filename_to_modulename(file:filename(), #context{}) -> string().
filename_to_modulename(File, #context{} = Context) ->
    filename_to_modulename(File, z_user_agent:get_class(Context), z_context:site(Context)).

-spec filename_to_modulename(file:filename(), ua_classifier:device_type(), atom()) -> string().
filename_to_modulename(File, UAClass, Host) ->
    list_to_atom(filename_to_modulename_1(File, Host, lists:reverse([$_|z_convert:to_list(UAClass)]))).

    filename_to_modulename_1([], Host, Acc) ->
        ?TEMPLATE_PREFIX ++ atom_to_list(Host) ++ [$_ | lists:reverse(Acc)];
    filename_to_modulename_1([C|T], Host, Acc) ->
        filename_to_modulename_1(T, Host, [savechar(C)|Acc]).

    savechar(C) when C >= $0 andalso C =< $9 ->
        C;
    savechar(C) when C >= $a andalso C =< $z ->
        C;
    savechar(C) when C >= $A andalso C =< $Z ->
        C+32;
    savechar(_C) ->
        $_.


%% Check if the template or one of the by the template included files is modified since compilation
%% or if the template has been compiled before a reset of all compiled templates.
template_is_modified(Module, State) ->
    case catch Module:template_reset_counter() < State#state.reset_counter of
        true ->
            true;
        false ->
            case State#state.do_modified_check of
                false -> false;
                true -> is_modified( Module:dependencies() )
            end;
        _Error ->
            true
    end.

%% Check if one of the template files is newer than the given datetime
is_modified([]) ->
    false;
is_modified([{File, DateTime}|Rest]) ->
    case filelib:last_modified(File) of
        0 ->
            true;
        FileMod when FileMod =/= DateTime ->
            true;
        _ ->
            is_modified(Rest)
    end.


get_debug_includes(Context) ->
    z_convert:to_bool(m_config:get_value(mod_development, debug_includes, Context)).

get_debug_blocks(Context) ->
    z_convert:to_bool(m_config:get_value(mod_development, debug_blocks, Context)).


runtime_wrap_debug_comments(FilePath, Output, Context) ->
    case get_debug_includes(Context) of
        false ->
            Output;
        true ->
            Start = "\n<!-- START " ++ relpath(FilePath) ++ " (runtime) -->\n",
            End = "\n<!-- END " ++ relpath(FilePath) ++ " -->\n",
            [Start, Output, End]
    end.

relpath(FilePath) when is_binary(FilePath) ->
    relpath( binary_to_list(FilePath) );
relpath(FilePath) when is_list(FilePath) ->
    Base = case os:getenv("ZOTONIC") of
        false ->
            {ok, Cwd} = file:get_cwd(),
            Cwd;
        ZDir ->
            ZDir
    end,
    drop_prefix(Base, FilePath).

drop_prefix([ A | As ], [ A | Bs ]) ->
    drop_prefix(As, Bs);
drop_prefix(_, Bs) ->
    Bs.
