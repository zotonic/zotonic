%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Template handling, compiles and renders django compatible templates using an extended version of erlydtl
%% @todo Make the template handling dependent on the host of the context (hosts have different modules enabled).

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

-module(z_template).
-behaviour(gen_server).

-author("Marc Worrell <marc@worrell.nl>").

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
	is_template_module/1
]).

-record(state, {reset_counter, host}).

%% Prefix for modules generated from templates.
-define(TEMPLATE_PREFIX, "template_").


start_link(SiteProps) ->
    {host, Host} = proplists:lookup(host, SiteProps),
    Name = z_utils:name_for_host(?MODULE, Host),
    gen_server:start_link({local, Name}, ?MODULE, SiteProps, []).


%% @doc Force a reset of all templates, used after a module has been activated or deactivated.
reset(Host) when is_atom(Host) ->
    Name = z_utils:name_for_host(?MODULE, Host),
    gen_server:cast(Name, reset);
reset(Context) ->
    gen_server:cast(Context#context.template_server, reset).


render(#render{} = Render, Context) ->
    render(Render#render.template, Render#render.vars, Context).


%% @spec render(File, Variables, Context) -> list()
%% @doc Render a template.  First requests the template module from the template server, then renders the template.
%% The resulting list contains the rendered template and scomp contexts.  Use render_to_iolist/3 to get a iolist().
render({cat, File}, Variables, Context) ->
    case find_template_cat(File, proplists:get_value(id, Variables), Context) of
        {ok, FoundFile} -> 
            render1(File, FoundFile, Variables, Context);
        {error, Reason} ->
            ?LOG("Could not find template: ~s (~p)", [File, Reason]),
            throw({error, {template_not_found, File, Reason}})
    end;
render(File, Variables, Context) ->
    File1 = z_convert:to_list(File),
    case find_template(File1, Context) of
        {ok, FoundFile} ->
            render1(File1, FoundFile, Variables, Context);
        {error, Reason} ->
            ?LOG("Could not find template: ~s (~p)", [File1, Reason]),
            throw({error, {template_not_found, File1, Reason}})
    end.

    %% Render the found template
    render1(File, FoundFile, Variables, Context) ->
        Result = case gen_server:call(Context#context.template_server, {check_modified, FoundFile}, ?TIMEOUT) of
            modified -> compile(File, FoundFile, Context);
            Other -> Other
        end,

        case Result of
            {ok, Module} ->
                %% @todo Move the in_process caching to the template compiler?
                OldCaching = z_depcache:in_process(true),
                case Module:render(Variables, Context) of
                    {ok, Output}   -> 
                        z_depcache:in_process(OldCaching),
                        Output;
                    {error, Reason} ->
                        z_depcache:in_process(OldCaching),
                        ?ERROR("Error rendering template: ~p (~p)~n", [File, Reason]),
                        throw({error, {template_rendering_error, File, Reason}})
                 end;
            {error, {{ErrFile,Line,Col}, _YeccModule, Error}} ->
                Error1 = try lists:flatten(Error) catch _:_ -> Error end,
                ?ERROR("Error compiling template: ~s:~p (~p) ~s~n", [ErrFile, Line, Col, Error1]),
                throw({error, {template_compile_error, ErrFile, {Line,Col}, Error1}});
            {error, Reason} ->
                Reason1 = try lists:flatten(Reason) catch _:_ -> Reason end,
                ?ERROR("Error compiling template: ~s (~p)~n", [File, Reason1]),
                throw({error, {template_compile_error, File, Reason1}})
        end.
    
            
%% @doc Render a template to an iolist().  This removes all scomp state etc from the rendered html and appends the
%% information in the scomp states to the context for later rendering.
%% @spec render_to_iolist(File, Vars, Context) -> {iolist(), Context}
render_to_iolist(File, Vars, Context) ->
    Html = render(File, Vars, Context),
    z_render:render_to_iolist(Html, Context).


%% @spec compile(File, Context) -> {ok, atom()} | {error, Reason}
%% @doc Compile a template, retun the module name.
compile(File, Context) ->
    case find_template(File, Context) of
        {ok, FoundFile} ->
            compile(File, FoundFile, Context);
        {error, Reason} ->
            {error, Reason}
    end.

compile(File, FoundFile, Context) ->
    z_notifier:notify(#debug{what=template, arg={compile, File, FoundFile}}, Context),
    gen_server:call(Context#context.template_server, {compile, File, FoundFile, Context}, ?TIMEOUT).


%% @spec find_template(File, Context) -> {ok, filename()} | {error, code} 
%% @doc Finds the template designated by the file, check modules.
%% When the file is an absolute path, then do nothing and assume the file exists.
find_template([$/|_] = File, _Context) ->
    {ok, File};
find_template([_,$:,$/|_] = File, _Context) ->
    {ok, File};
find_template(File, Context) ->
    z_module_indexer:find(template, File, Context).


%% @spec find_template(File, All, Context) -> FilenameList
%% @doc Finds the first or all templates designated by the file, check modules.
find_template(File, false, Context) ->
    case z_module_indexer:find(template, File, Context) of
        {ok, TemplateFile} -> [TemplateFile];
        {error, _Reason} -> []
    end;
find_template(File, true, Context) ->
    z_module_indexer:find_all(template, File, Context).


%% @spec find_template_cat(File, Id, Context) -> {ok, filename()} | {error, code} 
%% @doc Finds the template designated by the file, for the category of the rsc with id, check modules.
%% When the file is an absolute path, then do nothing and assume the file exists.
find_template_cat([$/|_] = File, _Id, _Context) ->
    {ok, File};
find_template_cat(File, None, Context) when None =:= <<>>; None =:= undefined; None =:= [] ->
    find_template(File, Context);
find_template_cat(File, Id, Context) ->
	Stack = case {m_rsc:is_a(Id, Context), m_rsc:p(Id, name, Context)} of
                {[meta,category|_], _} -> m_category:is_a(Id, Context); %% When the Id is a category, use the category chain itself.
                {L, undefined} -> L;
                {L, Name} -> L ++ [z_convert:to_atom(Name)]
            end,
	Root = filename:rootname(File),
	Ext = filename:extension(File),
	case lists:foldr(fun(Cat, {error, enoent}) -> find_template(Root ++ [$.|atom_to_list(Cat)] ++ Ext, Context);
					    (_Cat, Found) -> Found	
					 end, {error, enoent}, Stack) of
		{error, enoent} -> find_template(File, Context);
		{ok, Template} -> {ok, Template}
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
    {ok, #state{reset_counter=z_utils:now_msec(), host=proplists:get_value(host, SiteProps)}}.


%% @spec handle_call({check_modified, File}, From, State) -> {ok, Module} | {error, Reason}
%% @doc Compile the template if it has been modified, return the template module for rendering.
handle_call({check_modified, File}, _From, State) ->
    ModuleName = filename_to_modulename(File, State#state.host),
    Module = list_to_atom(ModuleName),
    Result = case template_is_modified(Module, State#state.reset_counter, State#state.host) of
                true  -> modified;
                false -> {ok, Module}
             end,
    {reply, Result, State};


%% @doc Compile the template, loads the compiled template in memory.  Make sure that we only compile 
%% one template at a time to prevent overloading the server on restarts.
handle_call({compile, File, FoundFile, Context}, _From, State) ->
    FinderFun  = fun(FinderFile, All) ->
        ?MODULE:find_template(FinderFile, All, Context)
    end,
    ModuleName = filename_to_modulename(FoundFile, State#state.host),
    Module     = list_to_atom(ModuleName),
    ErlyResult = case erlydtl:compile(  FoundFile,
                                        File,
                                        Module, 
                                        [{finder, FinderFun}, {template_reset_counter, State#state.reset_counter}],
                                        Context) of
                    {ok, Module1} -> {ok, Module1};
                    Error -> Error
                 end,
    {reply, ErlyResult, State}.

%% @doc Reset all compiled templates, done by the module_indexer after the module list changed.
handle_cast(reset, State) -> 
    {noreply, State#state{reset_counter=State#state.reset_counter+1}};
handle_cast(_Msg, State) -> 
    {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


%% @doc Translate a filename to a module name
filename_to_modulename(File, Host) ->
    filename_to_modulename(File, Host, []).

filename_to_modulename([], Host, Acc) ->
    ?TEMPLATE_PREFIX ++ atom_to_list(Host) ++ [$_ | lists:reverse(Acc)];
filename_to_modulename([C|T], Host, Acc) ->
    filename_to_modulename(T, Host, [savechar(C)|Acc]).

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
template_is_modified(Module, ResetCounter, Host) ->
    case catch Module:template_reset_counter() < ResetCounter of
        true ->
            true;
        false ->
            case Module:trans_table() /= z_trans_server:table(Host) of
                true ->
                    true;
                false ->
                    Deps = Module:dependencies(),
                    is_modified(Deps)
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
