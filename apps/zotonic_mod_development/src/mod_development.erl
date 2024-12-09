%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2023 Marc Worrell
%% @doc Support functions for site development and introspection of the
%% live system for template and database query tracing.
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

-module(mod_development).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

-mod_title("Development").
-mod_description("Development support, periodically builds and loads changed files.").
-mod_prio(1000).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
    event/2,
    task_xref_check/2,
    task_graph/2,
    reload/1,
    make/1,
    template_trace_start/1,
    template_trace_start/2,
    template_trace_stop/1,
    template_trace_fetch/1,
    observe_filewatcher/2,
    pid_observe_debug/3,
    pid_observe_development_reload/3,
    pid_observe_development_make/3,
    observe_admin_menu/3,
    observe_request_context/3,
    chrome/1,
    chrome/2,
    chrome/3,
    chromium/1,
    chromium/2,
    chromium/3,
    exec_browser/4,
    exec_browser/5
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").

-record(state, {
        site :: atom(),
        template_trace_sid :: undefined | binary() | all,
        template_trace :: map(),
        template_trace_timer :: undefined | timer:tref()
    }).

% After 10 minutes with no fetches we automatically stop with tracing templates.
-define(TEMPLATE_TRACE_AUTOSTOP, 600_000).


%%====================================================================
%% API
%%====================================================================

%% @doc Handle wired postback events.
event(#postback{ message = {template_xref_check, Args} }, Context) ->
    {element_id, EltId} = proplists:lookup(element_id, Args),
    case z_acl:is_allowed(use, mod_development, Context) of
        true ->
            z_sidejob:start(?MODULE, task_xref_check, [ EltId ], Context),
            z_render:wire({mask, [
                    {target, EltId},
                    {message, ?__("Checking all templates...", Context)}
                ]}, Context);
        false ->
            z_render:update(
                EltId,
                ?__("No permission to use mod_development.", Context),
                Context)
    end;
event(#postback{ message = {template_graph, Args} }, Context) ->
    {element_id, EltId} = proplists:lookup(element_id, Args),
    case z_acl:is_allowed(use, mod_development, Context) of
        true ->
            z_sidejob:start(?MODULE, task_graph, [ EltId ], Context),
            z_render:wire({mask, [
                    {target, EltId},
                    {message, ?__("Checking all templates...", Context)}
                ]}, Context);
        false ->
            z_render:update(
                EltId,
                ?__("No permission to use mod_development.", Context),
                Context)
    end;
event(#postback{ message = {template_trace_start, Args} }, Context) ->
    case z_acl:is_allowed(use, mod_development, Context) of
        true ->
            case z_convert:to_bool(proplists:get_value(all, Args)) of
                true ->
                    template_trace_start(all, Context),
                    z_render:growl(?__("Started tracing template inclusions for your session.", Context), Context);
                false ->
                    template_trace_start(Context),
                    z_render:growl(?__("Started tracing all template inclusions.", Context), Context)
            end;
        false ->
            z_render:growl_error(?__("No permission to use mod_development.", Context), Context)
    end;
event(#postback{ message = {template_trace_stop, _} }, Context) ->
    case z_acl:is_allowed(use, mod_development, Context) of
        true ->
            template_trace_stop(Context),
            z_render:growl(?__("Stopped tracing template inclusions for all sessions.", Context), Context);
        false ->
            z_render:growl_error(?__("No permission to use mod_development.", Context), Context)
    end;
event(#postback{ message = {template_trace_fetch, Args} }, Context) ->
    {textarea, EltId} = proplists:lookup(textarea, Args),
    case z_acl:is_allowed(use, mod_development, Context) of
        true ->
            Sid = session_id(Context),
            {ok, #{ session_id := TraceSid, graph := G }} = template_trace_fetch(Context),
            Status = case TraceSid of
                undefined -> <<"trace-stopped">>;
                all -> <<"trace-all">>;
                Sid -> <<"trace-session">>;
                _ -> <<"trace-other">>
            end,
            Context1 = z_render:wire({set_class, [{target, <<"body">>}, {class, Status}]}, Context),
            case Status of
                <<"trace-stopped">> ->
                    Context1;
                _ ->
                    {ok, Dot} = z_development_template_graph:dot_from_graph(G),
                    z_render:wire([
                        {set_value, [ {target, EltId}, {value, Dot}, {trigger_event, <<"change">>}]}
                    ], Context1)
            end;
        false ->
            z_render:update(
                EltId,
                ?__("No permission to use mod_development.", Context),
                Context)
    end;
event(#postback{ message = log_client_enable }, Context) ->
    IsEnabled = z_convert:to_bool(z_context:get_q(<<"is_enabled">>, Context)),
    Result = if
        IsEnabled ->
            mod_logging:log_client_start(Context);
        not IsEnabled ->
            mod_logging:log_client_stop(Context)
    end,
    case Result of
        ok when IsEnabled ->
            z_render:growl(?__("Started logging to the console of your browser tab.", Context), Context);
        ok when not IsEnabled ->
            z_render:growl(?__("Stopped logging to the console of all browser tabs.", Context), Context);
        {error, eacces} ->
            z_render:growl(?__("No permission to access the console logs.", Context), Context);
        {error, _} ->
            z_render:growl(?__("Error changing the console logs.", Context), Context)
    end.


task_xref_check(EltId, Context) ->
    {ok, XRef} = z_development_template_xref:check(Context),
    Vars = [
        {xref, XRef}
    ],
    Context1 = z_render:update(
        EltId,
        #render{
            template = "_development_template_xref.tpl",
            vars = Vars
        },
        Context),
    Context2 = z_render:wire({unmask, [ {target, EltId} ]}, Context1),
    Script = z_render:get_script(Context2),
    z_transport:reply(Script, Context).

task_graph(EltId, Context) ->
    {ok, Dot} = z_development_template_graph:dot(Context),
    Vars = [
        {dot, Dot}
    ],
    Context1 = z_render:update(
        EltId,
        #render{
            template = "_development_template_graph.tpl",
            vars = Vars
        },
        Context),
    Context2 = z_render:wire({unmask, [ {target, EltId} ]}, Context1),
    Script = z_render:get_script(Context2),
    z_transport:reply(Script, Context).


%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    gen_server:start_link({local, name(Context)}, ?MODULE, Args, []).


reload(Context) ->
    z_notifier:notify(development_reload, Context).

make(Context) ->
    z_notifier:notify(development_make, Context).

template_trace_start(Context) ->
    gen_server:cast(name(Context), {template_trace_start, session_id(Context)}).

template_trace_start(Sid, Context) ->
    gen_server:cast(name(Context), {template_trace_start, Sid}).

template_trace_stop(Context) ->
    gen_server:cast(name(Context), {template_trace_start, undefined}).

template_trace_fetch(Context) ->
    gen_server:call(name(Context), template_trace_fetch).

%% @doc Catch filewatcher file change events, reloads css or the open pages.
observe_filewatcher(#filewatcher{ file = File, extension = Extension }, Context) ->
    case z_convert:to_bool(m_config:get_value(mod_development, livereload, Context)) of
        true -> maybe_livereload(Extension, File, Context);
        false -> ok
    end.

pid_observe_development_reload(Pid, development_reload, _Context) ->
    gen_server:cast(Pid, development_reload).

pid_observe_development_make(Pid, development_make, _Context) ->
     gen_server:cast(Pid, development_make).

%% @doc Copy the database trace flag to a new (fresh) request process. This ensures
%% that the flag set in the session is also set for the current process.
observe_request_context(#request_context{ phase = refresh }, Context, _Context) ->
    z_development_dbtrace:copy_from_session(Context),
    Context;
observe_request_context(#request_context{ phase = _ }, Context, _Context) ->
    Context.

%% @doc Trace all template includes, used to generate a runtime view of all template
%% inclusions and dependencies.
pid_observe_debug(Pid, #debug{ what = template, arg = {render, _Filename, _SrcPos} = Arg }, Context) ->
    gen_server:cast(Pid, {template_render, Arg, session_id(Context)}),
    ok;
pid_observe_debug(_Pid, #debug{}, _Context) ->
    ok.

%% @doc Runs Chrome opening it in the site URL.
%% Ignore certificate errors and defines the site as secure, helpful to run Web Workers.
%% For extra args @see https://peter.sh/experiments/chromium-command-line-switches/
%% Common args:
%%   --incognito            Launches Chrome directly in Incognito private browsing mode
%%   --purge-memory-button  Add purge memory button to Chrome
%%   --multi-profiles       Enable multiple profiles in Chrome
%% e.g.
%% ``` mod_development:chrome(foo, ["--incognito", "--start-maximized"]). '''
chrome(SiteOrContext) ->
    z_exec_browser:chrome(SiteOrContext).

chrome(SiteOrContext, ExtraArgs) ->
    z_exec_browser:chrome(SiteOrContext, ExtraArgs).

chrome(SiteOrContext, ExtraArgs, Options) ->
    z_exec_browser:chrome(SiteOrContext, ExtraArgs, Options).

%% @doc Runs Chromium opening it in the site URL.
%% Ignore certificate errors and defines the site as secure, helpful to run Web Workers.
%% For extra args @see https://peter.sh/experiments/chromium-command-line-switches/
%% Common args:
%%   --incognito            Launches Chromium directly in Incognito private browsing mode
%%   --purge-memory-button  Add purge memory button to Chromium
%%   --multi-profiles       Enable multiple profiles in Chromium
%% e.g.
%% ``` mod_development:chromium(foo, ["--incognito", "--start-maximized"]). '''
chromium(SiteOrContext) ->
    z_exec_browser:chromium(SiteOrContext).

chromium(SiteOrContext, ExtraArgs) ->
    z_exec_browser:chromium(SiteOrContext, ExtraArgs).

chromium(SiteOrContext, ExtraArgs, Options) ->
    z_exec_browser:chromium(SiteOrContext, ExtraArgs, Options).

%% @doc Opens the site URL as secure in a browser
%% Currently supported:
%%   * Linux  [Chrome, Chromium]
%%   * macOS  [Chrome, Chromium]
%% @todo: support more OS and maybe other browsers
-spec exec_browser(Browser, SiteOrContext, ExtraArgs, Options) -> RetType
    when
        Browser       :: atom(),
        SiteOrContext :: atom() | z:context(),
        ExtraArgs     :: [string()],
        Options       :: map(),
        RetType       :: {ok, term()} | {error, term()}.
exec_browser(Browser, SiteOrContext, ExtraArgs, Options) ->
    z_exec_browser:exec_browser(Browser, SiteOrContext, ExtraArgs, Options).

exec_browser(Browser, OS, SiteUrl, ExtraArgs, Options) ->
    z_exec_browser:exec_browser(Browser, OS, SiteUrl, ExtraArgs, Options).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    Site = z_context:site(Context),
    logger:set_process_metadata(#{
        site => Site,
        module => ?MODULE
    }),
    {ok, #state{
        site = Site,
        template_trace_sid = undefined,
        template_trace = #{
            nodes => #{},
            edges => #{}
        }
    }}.


handle_call(template_trace_fetch, _From, #state{
        template_trace = Trace,
        template_trace_sid = Sid,
        template_trace_timer = PrevTimer
    } = State) ->
    #{ edges := Edges, nodes := Nodes  } = Trace,
    EdgesList = maps:values(Edges),
    NodesList = maps:values(Nodes),
    Result = #{
        session_id => Sid,
        graph => #{
            nodes => NodesList,
            edges => EdgesList
        }
    },
    case PrevTimer of
        undefined -> ok;
        _ -> timer:cancel(PrevTimer)
    end,
    {ok, Timer} = timer:send_after(?TEMPLATE_TRACE_AUTOSTOP, template_trace_stop),
    {reply, {ok, Result}, State#state{ template_trace_timer = Timer }};
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


handle_cast(development_reload, State) ->
    zotonic_filewatcher_beam_reloader:reload(),
    {noreply, State};
handle_cast(development_make, State) ->
    zotonic_filewatcher_beam_reloader:make(),
    {noreply, State};
handle_cast({template_trace_start, undefined}, State) ->
    {noreply, do_template_trace_stop(State)};
handle_cast({template_trace_start, Sid}, #state{ template_trace_timer = PrevTimer } = State) ->
    ?LOG_INFO(#{
        in => mod_development,
        text => <<"Template trace started.">>,
        sid => Sid
    }),
    {ok, Timer} = timer:send_after(?TEMPLATE_TRACE_AUTOSTOP, template_trace_stop),
    case PrevTimer of
        undefined -> ok;
        _ -> timer:cancel(PrevTimer)
    end,
    {noreply, State#state{
        template_trace_sid = Sid,
        template_trace = #{
            nodes => #{},
            edges => #{}
        },
        template_trace_timer = Timer
    }};
handle_cast({template_render, {render, Template, SrcPos}, Sid},
           #state{ template_trace_sid = TraceSid, template_trace = Trace } = State)
    when is_binary(Sid) andalso Sid =:= TraceSid; TraceSid =:= all ->
    Trace1 = do_template_trace(Template, SrcPos, Trace),
    {noreply, State#state{ template_trace = Trace1 }};
handle_cast({template_render, _Src, _Sid}, #state{} = State) ->
    {noreply, State};
handle_cast(Message, State) ->
    % Trap unknown casts
    {stop, {unknown_cast, Message}, State}.


handle_info(template_trace_stop, State) ->
    {noreply, do_template_trace_stop(State)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

do_template_trace_stop(#state{ template_trace_timer = undefined } = State) ->
    ?LOG_INFO(#{
        in => mod_development,
        text => <<"Template trace stopped.">>
    }),
    State#state{
        template_trace_sid = undefined,
        template_trace = #{
            nodes => #{},
            edges => #{}
        }
    };
do_template_trace_stop(#state{ template_trace_timer = Timer } = State) ->
    timer:cancel(Timer),
    do_template_trace_stop(State#state{ template_trace_timer = undefined }).

do_template_trace([Template], SrcPos, Trace) ->
    do_template_trace(Template, SrcPos, Trace);
do_template_trace(Template, undefined, Trace) ->
    % Root template from controller or render call
    {_FromId, Trace1, _FromNode} = add_template_node(Template, Trace, root),
    Trace1;
do_template_trace(Template, {From, 0, _Col}, Trace) ->
    % Extends or overrules
    {FromId, Trace1, FromNode} = add_template_node(From, Trace, normal),
    {ToId, Trace2, ToNode} = add_template_node(Template, Trace1, normal),
    #{ template := FromTpl} = FromNode,
    #{ template := ToTpl} = ToNode,
    Type = if
        FromTpl =:= ToTpl -> overrules;
        true -> extends
    end,
    Edge = #{
        from => ToId,
        to => FromId,
        module => maps:get(module, FromNode),
        type => Type
    },
    add_edge(Edge, Trace2);
do_template_trace(Template, {From, _Line, _Col}, Trace) ->
    % Include
    {FromId, Trace1, FromNode} = add_template_node(From, Trace, normal),
    {ToId, Trace2, _} = add_template_node(Template, Trace1, normal),
    Edge = #{
        from => FromId,
        to => ToId,
        module => maps:get(module, FromNode),
        type => include
    },
    add_edge(Edge, Trace2).

add_edge(Edge, #{ edges := Edges } = Trace) ->
    #{
        from := FromId,
        to := ToId,
        type := Type
    } = Edge,
    Key = {FromId, ToId, Type},
    case maps:is_key(Key, Edges) of
        false ->
            Edges1 = Edges#{
                Key => Edge
            },
            Trace#{ edges => Edges1 };
        true ->
            Trace
    end.


add_template_node(Template, #{ nodes := Ns } = Trace, Type) ->
    case maps:get(Template, Ns, undefined) of
        undefined ->
            N = maps:size(Ns),
            NodeId = <<"n", (integer_to_binary(N))/binary>>,
            Node = z_development_template_graph:filename_to_node(NodeId, Template),
            Node1 = Node#{
                type => Type
            },
            Ns1 = Ns#{ Template => Node1 },
            {NodeId, Trace#{ nodes => Ns1 }, Node1};
        #{ id := NodeId } = Node ->
            {NodeId, Trace, Node}
    end.

maybe_livereload(Ext, File, Context)
    when Ext =:= <<".css">>;
         Ext =:= <<".gif">>;
         Ext =:= <<".jpg">>;
         Ext =:= <<".png">>;
         Ext =:= <<".svg">> ->
    maybe_livereload_lib(File, Context);
maybe_livereload(<<".js">>, File, Context) ->
    case binary:split(File, <<"/lib/">>) of
        [ _, _Path ] -> livereload_page(Context);
        [ _ ] -> ok
    end;
maybe_livereload(<<".tpl">>, File, Context) ->
    case binary:split(File, <<"/templates/">>) of
        [ _, _Path ] -> livereload_page(Context);
        [ _ ] ->
            ok
    end;
maybe_livereload(_Ext, _File, _Context) ->
    ok.

maybe_livereload_lib(File, Context) ->
    case binary:split(File, <<"/lib/">>) of
        [ _, Path ] ->
            Url = z_dispatcher:url_for(lib, [ {star, Path} ], z_context:set_language(undefined, Context)),
            z_mqtt:publish(
                <<"public/development/livereload">>,
                [
                    {is_page_reload, false},
                    {path, Url}
                ],
                z_acl:sudo(Context));
        [ _ ] ->
            ok
    end.

livereload_page(Context) ->
    z_mqtt:publish(
        <<"public/development/livereload">>,
        [
            {is_page_reload, true}
        ],
        z_acl:sudo(Context)).

observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
     #menu_item{id=admin_development,
                parent=admin_system,
                label=?__("Development", Context),
                url={admin_development},
                visiblecheck={acl, use, mod_development}}

     |Acc].

name(Context) ->
    z_utils:name_for_site(?MODULE, Context).

session_id(Context) ->
    case z_context:session_id(Context) of
        {ok, Sid} -> Sid;
        {error, _} -> undefined
    end.

