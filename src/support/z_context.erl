%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009  Marc Worrell
%% @doc Request context for zophenic request evaluation.

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

-module(z_context).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    new/1,
    new/2,
    
    new_tests/0,

    site/1,
    hostname/1,
    hostname_port/1,

    is_request/1,

    prune_for_async/1,
    prune_for_template/1,
    prune_for_database/1,
    prune_for_scomp/2,
    output/2,

    abs_url/2,

    pickle/1,
    depickle/1,

    combine_results/2,

    continue_session/1,
    has_session/1,
    has_session_page/1,
    
    ensure_all/1,
    ensure_session/1,
    ensure_qs/1,

    get_reqdata/1,
    set_reqdata/2,
    get_resource_module/1,
    set_resource_module/2,

    get_q/2,
    get_q/3,
    get_q_all/1,
    get_q_all/2,
    get_q_all_noz/1,
    get_q_validated/2,

    add_script_session/1,
    add_script_page/1,
    add_script_session/2,
    add_script_page/2,

    spawn_link_session/4,
    spawn_link_page/4,

    get_value/2,

    set_session/3,
    get_session/2,
    get_session/3,
    incr_session/3,

    set_page/3,
    get_page/2,
    incr_page/3,

    persistent_id/1,
    set_persistent/3,
    get_persistent/2,

    set/3,
    set/2,
    get/2,
    get/3,
    incr/3,
    get_all/1,

    language/1,
    set_language/2,

    merge_scripts/2,
    copy_scripts/2,
    clean_scripts/1,

    set_resp_header/3,
    get_resp_header/2,
    get_req_header/2,

    get_req_path/1,

    set_cookie/3,
    set_cookie/4,
    get_cookie/2,

    cookie_domain/1,
    document_domain/1,
    streamhost/1
]).

-include_lib("zotonic.hrl").


%% @doc Return a new empty context, no request is initialized.
%% @spec new(HostDescr) -> Context2
%%      HostDescr = Context | atom() | ReqData
new(#context{} = C) ->
    #context{
        host=C#context.host,
        language=C#context.language,
        depcache=C#context.depcache,
        notifier=C#context.notifier,
        session_manager=C#context.session_manager,
        dispatcher=C#context.dispatcher,
        template_server=C#context.template_server,
        scomp_server=C#context.scomp_server,
        dropbox_server=C#context.dropbox_server,
        pivot_server=C#context.pivot_server,
        module_indexer=C#context.module_indexer,
        translation_table=C#context.translation_table
    };
new(undefined) ->
    case z_sites_dispatcher:get_fallback_site() of
        undefined -> throw({error, no_site_enabled});
        Site -> new(Site)
    end;
new(Host) when is_atom(Host) ->
    Context = set_server_names(#context{host=Host}),
    Context#context{language=z_trans:default_language(Context)};
new(ReqData) ->
    %% This is the requesting thread, enable simple memo functionality.
    z_memo:enable(),
    z_depcache:in_process(true),
    Context = set_server_names(#context{wm_reqdata=ReqData, host=site(ReqData)}),
    set_dispatch_from_path(Context#context{language=z_trans:default_language(Context)}).


%% @doc Create a new context record for a host with a certain language.
new(Host, Lang) when is_atom(Host), is_atom(Lang) ->
    Context = set_server_names(#context{host=Host}),
    Context#context{language=Lang};
%% @doc Create a new context record for the current request and resource module
new(ReqData, Module) ->
    %% This is the requesting thread, enable simple memo functionality.
    z_memo:enable(),
    z_depcache:in_process(true),
    Context = set_server_names(#context{wm_reqdata=ReqData, resource_module=Module, host=site(ReqData)}),
    set_dispatch_from_path(Context#context{language=z_trans:default_language(Context)}).


% @doc Create a new context used when testing parts of zotonic
new_tests() ->
    z_trans_server:set_context_table(#context{host=test, language=en, notifier='z_notifier$test'}).


%% @doc Set the dispatch rule for this request to the context var 'zotonic_dispatch'
set_dispatch_from_path(Context) ->
    case dict:find(zotonic_dispatch, wrq:path_info(Context#context.wm_reqdata)) of
        {ok, Dispatch} -> set(zotonic_dispatch, Dispatch, Context);
        error -> Context
    end.

%% @doc Set all server names for the given host.
%% @spec set_server_names(Context1) -> Context2
set_server_names(#context{host=Host} = Context) ->
    HostAsList = [$$ | atom_to_list(Host)],
    Context#context{
        depcache=list_to_atom("z_depcache"++HostAsList),
        notifier=list_to_atom("z_notifier"++HostAsList),
        session_manager=list_to_atom("z_session_manager"++HostAsList),
        dispatcher=list_to_atom("z_dispatcher"++HostAsList),
        template_server=list_to_atom("z_template"++HostAsList),
        scomp_server=list_to_atom("z_scomp"++HostAsList),
        dropbox_server=list_to_atom("z_dropbox"++HostAsList),
        pivot_server=list_to_atom("z_pivot_rsc"++HostAsList),
        module_indexer=list_to_atom("z_module_indexer"++HostAsList),
        translation_table=z_trans_server:table(Host)
    }.



%% @doc Maps the host in the request to a site in the sites folder.
%% @spec site(wm_reqdata) -> atom()
site(#context{host=Host}) ->
    Host;
%% @spec site(wm_reqdata) -> atom()
site(ReqData = #wm_reqdata{}) ->
    PathInfo = wrq:path_info(ReqData),
    case dict:find(zotonic_host, PathInfo) of
        {ok, Host} -> Host;
        error -> z_sites_dispatcher:get_fallback_site()
    end.


%% @doc Return the preferred hostname from the site configuration
%% @spec hostname(Context) -> string()
hostname(Context) ->
    case z_dispatcher:hostname(Context) of
        Empty when Empty == undefined; Empty == []; Empty == <<>> ->
            "localhost";
        Hostname ->
            Hostname
    end.

%% @doc Return the preferred hostname, including port, from the site configuration
%% @spec hostname_port(Context) -> string()
hostname_port(Context) ->
    case z_dispatcher:hostname_port(Context) of
        Empty when Empty == undefined; Empty == [] ->
            "localhost";
        Hostname ->
            Hostname
    end.


%% @doc Check if the current context is a request context
is_request(#context{wm_reqdata=undefined}) -> false;
is_request(_Context) -> true.


%% @doc Make the context safe to use in a async message. This removes buffers and the db transaction.
prune_for_async(#context{} = Context) ->
    #context{
        wm_reqdata=Context#context.wm_reqdata,
        host=Context#context.host,
        user_id=Context#context.user_id,
        session_pid=Context#context.session_pid,
        page_pid=Context#context.page_pid,
        acl=Context#context.acl,
        props=Context#context.props,
        depcache=Context#context.depcache,
        notifier=Context#context.notifier,
        session_manager=Context#context.session_manager,
        dispatcher=Context#context.dispatcher,
        template_server=Context#context.template_server,
        scomp_server=Context#context.scomp_server,
        dropbox_server=Context#context.dropbox_server,
        pivot_server=Context#context.pivot_server,
        module_indexer=Context#context.module_indexer,
        translation_table=Context#context.translation_table,
        language=Context#context.language
    }.


%% @doc Cleanup a context for the output stream
prune_for_template(#context{}=Context) ->
    #context{
        wm_reqdata=undefined,
        props=undefined,
        updates=Context#context.updates,
        actions=Context#context.actions,
        content_scripts=Context#context.content_scripts,
        scripts=Context#context.scripts,
        wire=Context#context.wire,
        validators=Context#context.validators,
        render=Context#context.render
    };
prune_for_template(Output) -> Output.


%% @doc Cleanup a context so that it can be used exclusively for database connections
prune_for_database(Context) ->
    #context{
        host=Context#context.host,
        dbc=Context#context.dbc,
        depcache=Context#context.depcache,
        notifier=Context#context.notifier,
        session_manager=Context#context.session_manager,
        dispatcher=Context#context.dispatcher,
        template_server=Context#context.template_server,
        scomp_server=Context#context.scomp_server,
        dropbox_server=Context#context.dropbox_server,
        pivot_server=Context#context.pivot_server,
        module_indexer=Context#context.module_indexer
    }.


%% @doc Cleanup a context for cacheable scomp handling.  Resets most of the accumulators to prevent duplicating
%% between different (cached) renderings.
prune_for_scomp(VisibleFor, Context) ->
    z_acl:set_visible_for(VisibleFor, Context#context{
        dbc=undefined,
        wm_reqdata=undefined,
        updates=[],
        actions=[],
        content_scripts=[],
        scripts=[],
        wire=[],
        validators=[],
        render=[]
    }).


%% @doc Make the url an absolute url by prepending the hostname.
%% @spec abs_url(string(), Context) -> string()
abs_url(Url, Context) when is_binary(Url) ->
    abs_url(binary_to_list(Url), Context);
abs_url(Url, Context) ->
    case has_url_protocol(Url) of
        true ->
            Url;
        false ->
            ["http://", hostname_port(Context), Url]
    end.

    has_url_protocol([]) ->
        false;
    has_url_protocol([H|T]) when is_integer($a) andalso H >= $a andalso H =< $z ->
        has_url_protocol(T);
    has_url_protocol([$:|_]) ->
        true;
    has_url_protocol(_) ->
        false.



%% @doc Pickle a context for storing in the database
%% @todo pickle/depickle the visitor id (when any)
%% @spec pickle(Context) -> tuple()
pickle(Context) ->
    {pickled_context, Context#context.host, Context#context.user_id, Context#context.language, undefined}.

%% @doc Depickle a context for restoring from a database
%% @todo pickle/depickle the visitor id (when any)
depickle({pickled_context, Host, UserId, Language, _VisitorId}) ->
    Context = set_server_names(#context{host=Host, language=Language}),
    case UserId of
        undefined -> Context;
        _ -> z_acl:logon(UserId, Context)
    end.

%% @spec output(list(), Context) -> {io_list(), Context}
%% @doc Replace the contexts in the output with their rendered content and collect all scripts
output(<<>>, Context) ->
    {[], Context};
output(B, Context) when is_binary(B) ->
    {B, Context};
output(List, Context) ->
    output1(List, Context, []).

%% @doc Recursively walk through the output, replacing all context placeholders with their rendered output
output1(B, Context, Acc) when is_binary(B) ->
    {[lists:reverse(Acc),B], Context};
output1([], Context, Acc) ->
    {lists:reverse(Acc), Context};
output1([#context{}=C|Rest], Context, Acc) ->
    {Rendered, Context1} = output1(C#context.render, Context, []),
    output1(Rest, merge_scripts(C, Context1), [Rendered|Acc]);
output1([{script, Args}|Rest], Context, Acc) ->
    output1(Rest, Context, [render_script(Args, Context)|Acc]);
output1([List|Rest], Context, Acc) when is_list(List) ->
    {Rendered, Context1} = output1(List, Context, []),
    output1(Rest, Context1, [Rendered|Acc]);
output1([undefined|Rest], Context, Acc) ->
    output1(Rest, Context, Acc);
output1([C|Rest], Context, Acc) when is_atom(C) ->
    output1(Rest, Context, [list_to_binary(atom_to_list(C))|Acc]);
output1([{trans, _} = Trans|Rest], Context, Acc) ->
    output1(Rest, Context, [z_trans:lookup_fallback(Trans, Context)|Acc]);
output1([{{_,_,_},{_,_,_}} = D|Rest], Context, Acc) ->
    output1([filter_date:date(D, "Y-m-d H:i:s", Context)|Rest], Context, Acc);
output1([T|Rest], Context, Acc) when is_tuple(T) ->
    output1([iolist_to_binary(io_lib:format("~p", [T]))|Rest], Context, Acc);
output1([C|Rest], Context, Acc) ->
    output1(Rest, Context, [C|Acc]).
    
    render_script(Args, Context) ->
        NoStartup = z_convert:to_bool(proplists:get_value(nostartup, Args, false)),
        Extra = [ S || S <- z_notifier:map(#scomp_script_render{is_nostartup=NoStartup, args=Args}, Context), S /= undefined ],
        Script = case NoStartup of
            false ->
                [ z_script:get_page_startup_script(Context),
                  Extra,
                  z_script:get_script(Context) ];
            true ->
                [z_script:get_script(Context), Extra]
        end,
        case proplists:get_value(format, Args, "html") of
            "html" ->
                [ <<"\n\n<script type='text/javascript'>\n$(function() {\n">>, Script, <<"\n});\n</script>\n">> ];
            "escapejs" ->
                z_utils:js_escape(Script)
        end.
        

%% @spec combine_results(Context1, Context2) -> Context
%% @doc Merge the scripts and the rendered content of two contexts into Context1
combine_results(C1, C2) ->
    Merged = merge_scripts(C2, C1),
    Merged#context{
        render=combine(C1#context.render, C2#context.render)
    }.

%% @spec merge_scripts(Context, ContextAcc) -> Context
%% @doc Merge the scripts from context C into the context accumulator, used when collecting all scripts in an output stream
merge_scripts(C, Acc) ->
    Acc#context{
        updates=combine(Acc#context.updates, C#context.updates),
        actions=combine(Acc#context.actions, C#context.actions),
        content_scripts=combine(Acc#context.content_scripts, C#context.content_scripts),
        scripts=combine(Acc#context.scripts, C#context.scripts),
        wire=combine(Acc#context.wire, C#context.wire),
        validators=combine(Acc#context.validators, C#context.validators)
    }.
    
combine([],X) -> X;
combine(X,[]) -> X;
combine(X,Y) -> [X++Y].

%% @doc Remove all scripts from the context
%% @spec clean_scripts(Context) -> Context
clean_scripts(C) ->
    z_script:clean(C).


%% @doc Overwrite the scripts in Context with the scripts in From
%% @spec copy_scripts(From, Context) -> Context
copy_scripts(From, Context) ->
    Context#context{
        updates=From#context.updates,
        actions=From#context.actions,
        content_scripts=From#context.content_scripts,
        scripts=From#context.scripts,
        wire=From#context.wire,
        validators=From#context.validators
    }.


%% @doc Continue an existing session, if the session id is in the request.
continue_session(Context) ->
    case Context#context.session_pid of
        undefined ->
            case z_session_manager:continue_session(Context) of
                {ok, Context1} ->
                    Context2 = z_auth:logon_from_session(Context1),
                    z_notifier:foldl(session_context, Context2, Context2);
                {error, _} ->
                    Context
            end;
        _ ->
            Context
    end.
    

%% @doc Check if the current context has a session page attached
has_session_page(#context{page_pid=PagePid}) when is_pid(PagePid) ->
    true;
has_session_page(_) ->
    false.

%% @doc Check if the current context has a session attached
has_session(#context{session_pid=SessionPid}) when is_pid(SessionPid) ->
    true;
has_session(_) ->
    false.


%% @doc Ensure session and page session and fetch and parse the query string
ensure_all(Context) ->
    case get(no_session, Context, false) of
        false ->
            ensure_page_session(ensure_session(ensure_qs(Context)));
        true ->
            continue_page_session(continue_session(ensure_qs(Context)))
    end.



%% @doc Ensure that we have a session, start a new session process when needed
ensure_session(Context) ->
    case Context#context.session_pid of
        undefined ->
            Context1 = z_session_manager:ensure_session(Context),
            Context2 = z_auth:logon_from_session(Context1),
            Context3 = z_notifier:foldl(session_context, Context2, Context2),
            add_nocache_headers(Context3);
        _ ->
            Context
    end.

%% @doc Ensure that we have a page session, used for comet and postback requests.
ensure_page_session(Context) ->
    case Context#context.page_pid of
        undefined ->
            z_session:ensure_page_session(Context);
        _ ->
            Context
    end.

continue_page_session(Context) ->
    case Context#context.session_pid of
        undefined ->
            Context;
        _ ->
            z_session:ensure_page_session(Context)
    end.


%% @doc Ensure that we have parsed the query string, fetch body if necessary
ensure_qs(Context) ->
    case proplists:lookup('q', Context#context.props) of
        {'q', _Qs} ->
            Context;
        none ->
            ReqData  = Context#context.wm_reqdata,
            Query    = wrq:req_qs(ReqData),
            PathDict = wrq:path_info(ReqData),
            PathArgs = lists:map(
                            fun ({T,V}) when is_atom(V) -> {atom_to_list(T),atom_to_list(V)};
                                ({T,V})                 -> {atom_to_list(T),mochiweb_util:unquote(V)}
                            end,
                            dict:to_list(PathDict)),
            QPropsUrl = z_utils:prop_replace('q', PathArgs++Query, Context#context.props),
            {Body, ContextParsed} = parse_form_urlencoded(Context#context{props=QPropsUrl}),
            QPropsAll = z_utils:prop_replace('q', PathArgs++Body++Query, ContextParsed#context.props),
            ContextParsed#context{props=QPropsAll}
    end.


%% @spec get_reqdata(Context) -> #wm_reqdata{}
%% @doc Return the webmachine request data of the context
get_reqdata(Context) ->
    Context#context.wm_reqdata.

%% @spec set_reqdata(ReqData, Context) -> #wm_reqdata{}
%% @doc Set the webmachine request data of the context
set_reqdata(ReqData = #wm_reqdata{}, Context) ->
    Context#context{wm_reqdata=ReqData}.


%% @spec get_resource_module(Context) -> term()
%% @doc Get the resource module handling the request.
get_resource_module(Context) ->
    Context#context.resource_module.

%% @spec set_resource_module(Module::atom(), Context) -> NewContext
set_resource_module(Module, Context) ->
    Context#context{resource_module=Module}.


%% @spec get_q(Key::string(), Context) -> Value::string() | undefined
%% @doc Get a request parameter, either from the query string or the post body.  Post body has precedence over the query string.
get_q([Key|_] = Keys, Context) when is_list(Key); is_atom(Key) ->
    lists:foldl(fun(K, Acc) ->
                    case get_q(K, Context) of
                        undefined -> Acc;
                        Value -> [{z_convert:to_atom(K), Value}|Acc]
                    end
                end,
                [],
                Keys);
get_q(Key, Context) ->
    case proplists:lookup('q', Context#context.props) of
        {'q', Qs} -> proplists:get_value(z_convert:to_list(Key), Qs);
        none -> undefined
    end.


%% @spec get_q(Key::string(), Context, Default) -> Value::string()
%% @doc Get a request parameter, either from the query string or the post body.  Post body has precedence over the query string.
get_q(Key, Context, Default) ->
    case proplists:lookup('q', Context#context.props) of
        {'q', Qs} -> proplists:get_value(z_convert:to_list(Key), Qs, Default);
        none -> Default
    end.


%% @spec get_q_all(Context) -> [{Key::string(), [Values]}]
%% @doc Get all parameters.
get_q_all(Context) ->
    case proplists:lookup('q', Context#context.props) of
        {'q', Qs} -> Qs;
        none -> []
    end.


%% @spec get_q_all(Key::string(), Context) -> [Values]
%% @doc Get the all the parameters with the same name, returns the empty list when non found.
get_q_all(Key, Context) ->
    case proplists:lookup('q', Context#context.props) of
        none -> [];
        {'q', Qs} -> proplists:get_all_values(z_convert:to_list(Key), Qs)
    end.


%% @spec get_q_all_noz(Context) -> [{Key::string(), [Values]}]
%% @doc Get all query/post args, filter the zotonic internal args.
get_q_all_noz(Context) ->
    lists:filter(fun({X,_}) -> not is_zotonic_arg(X) end, z_context:get_q_all(Context)).

    is_zotonic_arg("zotonic_host") -> true;
    is_zotonic_arg("zotonic_dispatch") -> true;
    is_zotonic_arg("postback") -> true;
    is_zotonic_arg("triggervalue") -> true;
    is_zotonic_arg("z_trigger_id") -> true;
    is_zotonic_arg("z_target_id") -> true;
    is_zotonic_arg("z_delegate") -> true;
    is_zotonic_arg("z_sid") -> true;
    is_zotonic_arg("z_pageid") -> true;
    is_zotonic_arg("z_v") -> true;
    is_zotonic_arg("z_msg") -> true;
    is_zotonic_arg("z_comet") -> true;
    is_zotonic_arg(_) -> false.


%% @spec get_q_validated(Key, Context) -> Value
%% @doc Fetch a query parameter and perform the validation connected to the parameter. An exception {not_validated, Key}
%%      is thrown when there was no validator, when the validator is invalid or when the validation failed.
get_q_validated([Key|_] = Keys, Context) when is_list(Key); is_atom(Key) ->
    lists:foldl(fun (K, Acc) ->
                    case get_q_validated(K, Context) of
                        undefined -> Acc;
                        Value -> [{z_convert:to_atom(K), Value}|Acc]
                    end
                end,
                [],
                Keys);
get_q_validated(Key, Context) ->
    case proplists:lookup('q_validated', Context#context.props) of
        {'q_validated', Qs} ->
            case proplists:lookup(z_convert:to_list(Key), Qs) of
                {_Key, Value} -> Value;
                none -> throw({not_validated, Key})
            end
    end.


%% ------------------------------------------------------------------------------------
%% Communicate with pages, session and user processes
%% ------------------------------------------------------------------------------------

%% @doc Add the script from the context to all pages of the session.
add_script_session(Context) ->
    Script = z_script:get_script(Context),
    add_script_session(Script, Context).


%% @doc Add the script from the context to the page in the user agent.
add_script_page(Context) ->
    Script = z_script:get_script(Context),
    add_script_page(Script, Context).


%% @doc Add a script to the all pages of the session. Used for comet feeds.
add_script_session(Script, Context) ->
    z_session:add_script(Script, Context#context.session_pid).


%% @doc Add a script to the page in the user agent.  Used for comet feeds.
add_script_page(Script, Context) ->
    z_session_page:add_script(Script, Context#context.page_pid).


%% @doc Spawn a new process, link it to the session process.
spawn_link_session(Module, Func, Args, Context) ->
    z_session:spawn_link(Module, Func, Args, Context).

%% @doc Spawn a new process, link it to the page process.  Used for comet feeds.
spawn_link_page(Module, Func, Args, Context) ->
    z_session_page:spawn_link(Module, Func, Args, Context).


%% ------------------------------------------------------------------------------------
%% Set/get/modify state properties
%% ------------------------------------------------------------------------------------


%% @spec get_value(Key::string(), Context) -> Value | undefined
%% @doc Find a key in the context, page, session or persistent state.
%% @todo Add page and user lookup
get_value(Key, Context) ->
    case get(Key, Context) of
        undefined ->
            case get_page(Key, Context) of
                undefined ->
                    case get_session(Key, Context) of
                        undefined -> get_persistent(Key, Context);
                        Value -> Value
                    end;
                Value ->
                    Value
            end;
        Value ->
            Value
    end.


%% @doc Ensure that we have an id for the visitor
persistent_id(Context) ->
    z_session:persistent_id(Context).

%% @spec set_persistent(Key, Value, Context) -> Context
%% @doc Set the value of the visitor variable Key to Value
set_persistent(Key, Value, Context) ->
    z_session:set_persistent(Key, Value, Context),
    Context.

%% @spec get_persistent(Key, Context) -> Value
%% @doc Fetch the value of the visitor variable Key
get_persistent(_Key, #context{session_pid=undefined}) ->
    undefined;
get_persistent(Key, Context) ->
    z_session:get_persistent(Key, Context).


%% @spec set_session(Key, Value, Context) -> Context
%% @doc Set the value of the session variable Key to Value
set_session(Key, Value, Context) ->
    z_session:set(Key, Value, Context#context.session_pid),
    Context.

%% @spec get_session(Key, Context) -> Value
%% @doc Fetch the value of the session variable Key
get_session(_Key, #context{session_pid=undefined}) ->
    undefined;
get_session(Key, Context) ->
    z_session:get(Key, Context#context.session_pid).

%% @spec get_session(Key, Context, DefaultValue) -> Value
%% @doc Fetch the value of the session variable Key, falling back to default.
get_session(_Key, #context{session_pid=undefined}, DefaultValue) ->
    DefaultValue;
get_session(Key, Context, DefaultValue) ->
    z_session:get(Key, Context#context.session_pid, DefaultValue).

%% @spec incr_session(Key, Increment, Context) -> {NewValue, NewContext}
%% @doc Increment the session variable Key
incr_session(Key, Value, Context) ->
    {z_session:incr(Key, Value, Context#context.session_pid), Context}.

%% @spec set_page(Key, Value, Context) -> Context
%% @doc Set the value of the page variable Key to Value
set_page(Key, Value, Context) ->
    z_session_page:set(Key, Value, Context#context.page_pid),
    Context.

%% @spec get_page(Key, Context) -> Value
%% @doc Fetch the value of the page variable Key
get_page(_Key, #context{page_pid=undefined}) ->
    undefined;
get_page(Key, Context) ->
    z_session_page:get(Key, Context#context.page_pid).


%% @spec incr_page(Key, Increment, Context) -> {NewValue, NewContext}
%% @doc Increment the page variable Key
incr_page(Key, Value, Context) ->
    {z_session_page:incr(Key, Value, Context#context.session_pid), Context}.


%% @spec set(Key, Value, Context) -> Context
%% @doc Set the value of the context variable Key to Value
set(Key, Value, Context) ->
    Props = z_utils:prop_replace(Key, Value, Context#context.props),
    Context#context{props = Props}.


%% @spec set(PropList, Context) -> Context
%% @doc Set the value of the context variables to all {Key, Value} properties.
set(PropList, Context) when is_list(PropList) ->
    NewProps = lists:foldl(
        fun ({Key,Value}, Props) ->
            z_utils:prop_replace(Key, Value, Props)
        end, Context#context.props, PropList),
    Context#context{props = NewProps}.


%% @spec get(Key, Context) -> Value | undefined
%% @doc Fetch the value of the context variable Key, return undefined when Key is not found.
get(Key, Context) ->
    case proplists:lookup(Key, Context#context.props) of
        {Key, Value} -> Value;
        none -> undefined
    end.

%% @spec get(Key, Context, Default) -> Value | Default
%% @doc Fetch the value of the context variable Key, return Default when Key is not found.
get(Key, Context, Default) ->
    case proplists:lookup(Key, Context#context.props) of
        {Key, Value} -> Value;
        none -> Default
    end.


%% @spec get_all(Context) -> PropList
%% @doc Return a proplist with all context variables.
get_all(Context) ->
    Context#context.props.


%% @spec incr(Key, Increment, Context) -> {NewValue,NewContext}
%% @doc Increment the context variable Key
incr(Key, Value, Context) ->
    R = case z_convert:to_integer(get(Key, Context)) of
	    undefined -> Value;
	    N -> N + Value
	end,
    {R, set(Key, R, Context)}.


%% @doc Return the selected language of the Context
language(Context) ->
    Context#context.language.

%% @doc Set the language of the context.
%% @spec set_language(atom(), context()) -> context()
set_language(Lang, Context) when is_atom(Lang) ->
    Context#context{language=Lang};
set_language(Lang, Context) ->
    Lang1 = z_convert:to_list(Lang),
    case z_trans:is_language(Lang1) of
        true -> set_language(list_to_atom(Lang1), Context);
        false -> Context
    end.

%% @doc Set a response header for the request in the context.
%% @spec set_resp_header(Header, Value, Context) -> NewContext
set_resp_header(Header, Value, Context = #context{wm_reqdata=ReqData}) ->
    RD1 = wrq:set_resp_header(Header, Value, ReqData),
    Context#context{wm_reqdata=RD1}.

%% @doc Get a response header
%% @spec get_resp_header(Header, Context) -> Value
get_resp_header(Header, #context{wm_reqdata=ReqData}) ->
    wrq:get_resp_header(Header, ReqData).


%% @doc Get a request header
%% @spec get_req_header(Header, Context) -> Value
get_req_header(Header, Context) ->
    ReqData = get_reqdata(Context),
    wrq:get_req_header(Header, ReqData).


%% @doc Return the request path
%% @spec get_req_path(Context) -> list()
get_req_path(Context) ->
    ReqData = get_reqdata(Context),
    wrq:raw_path(ReqData).


%% @doc Fetch the cookie domain, defaults to 'undefined' which will equal the domain
%% to the domain of the current request.
%% @spec cookie_domain(Context) -> list() | undefined
cookie_domain(Context) ->
    case m_site:get(cookie_domain, Context) of
        Empty when Empty == undefined; Empty == []; Empty == <<>> ->
            %% When there is a stream domain, the check if the stream domain is a subdomain
            %% of the hostname, if so then set a wildcard
            case m_site:get(streamhost, Context) of
                None when None == undefined; None == []; None == <<>> ->
                    undefined;
                StreamDomain ->
                    [StreamDomain1|_] = string:tokens(z_convert:to_list(StreamDomain), ":"),
                    Hostname = hostname(Context),
                    case postfix(Hostname, StreamDomain1) of
                        [] -> Hostname;
                        [$.|_] = Prefix -> Prefix;
                        Prefix -> [$.|Prefix]
                    end
            end;
        Domain ->
            z_convert:to_list(Domain)
    end.


    %% Return the longest matching postfix of two lists.
    postfix(A, B) ->
        postfix(lists:reverse(z_convert:to_list(A)), lists:reverse(z_convert:to_list(B)), []).
    
    postfix([X|A], [X|B], Acc) ->
        postfix(A, B, [X|Acc]);
    postfix(_A, _B, Acc) ->
        Acc.

%% @doc The document domain used for cross domain iframe javascripts
document_domain(Context) ->
    case cookie_domain(Context) of
        [$.|Domain] -> Domain;
        Domain -> Domain
    end.

%% @doc Fetch the domain and port for stream (comet/websocket) connections
%% @spec streamhost(Context) -> list()
streamhost(Context) ->
    case m_site:get(streamhost, Context) of
        Empty when Empty == undefined; Empty == []; Empty == <<>> ->
            hostname_port(Context);
        Domain ->
            Domain
    end.


%% ------------------------------------------------------------------------------------
%% Local helper functions
%% ------------------------------------------------------------------------------------

%% @spec parse_form_urlencoded(context()) -> {list(), NewContext}
%% @doc Return the keys in the body of the request, only if the request is application/x-www-form-urlencoded
parse_form_urlencoded(Context) ->
    ReqData = get_reqdata(Context),
    case wrq:get_req_header_lc("content-type", ReqData) of
        "application/x-www-form-urlencoded" ++ _ ->
            case wrq:req_body(ReqData) of
                {undefined, ReqData1} ->
                     {[], set_reqdata(ReqData1, Context)};
                {Binary, ReqData1} ->
                     {mochiweb_util:parse_qs(Binary), set_reqdata(ReqData1, Context)}
            end;
        "multipart/form-data" ++ _ ->
            FileCheckFun = fun(_Filename, _ContentType, _Size) ->
                                ok
                           end,
            {Form, ContextRcv} = z_parse_multipart:recv_parse(FileCheckFun, Context),
            FileArgs = [ {Name, #upload{filename=Filename, tmpfile=TmpFile}} || {Name, Filename, TmpFile} <- Form#multipart_form.files ],
            {Form#multipart_form.args ++ FileArgs, ContextRcv};
        _Other ->
            {[], Context}
    end.


%% @doc Some user agents have too aggressive client side caching.
%% These headers prevent the caching of content on the user agent iff
%% the content generated has a session. You can prevent addition of
%% these headers by not calling z_context:ensure_session/1, or
%% z_context:ensure_all/1.
%% @spec add_nocache_headers(#context{}) -> #context{}
add_nocache_headers(Context = #context{wm_reqdata=ReqData}) ->
    RD1 = wrq:set_resp_header("Cache-Control", "no-store, no-cache, must-revalidate, post-check=0, pre-check=0", ReqData),
    RD2 = wrq:set_resp_header("Expires", httpd_util:rfc1123_date({{2008,12,10}, {15,30,0}}), RD1),
    % This let IE6 accept our cookies, basically we tell IE6 that our cookies do not contain any private data.
    RD3 = wrq:set_resp_header("P3P", "CP=\"NOI ADM DEV PSAi COM NAV OUR OTRo STP IND DEM\"", RD2),
    Context#context{wm_reqdata=RD3}.


%% @doc Set a cookie value with default options.
set_cookie(Key, Value, Context) ->
    set_cookie(Key, Value, [], Context).

%% @doc Set a cookie value with cookie options.
set_cookie(Key, Value, Options, Context) ->
    % Add domain to cookie if not set
    Options1 = case proplists:lookup(domain, Options) of
                   {domain, _} -> Options;
                   none -> [{domain, z_context:cookie_domain(Context)}|Options]
               end,
    RD = Context#context.wm_reqdata,
    Hdr = mochiweb_cookies:cookie(Key, Value, Options1),
    RD1 = wrq:merge_resp_headers([Hdr], RD),
    z_context:set_reqdata(RD1, Context).

%% @doc Read a cookie value from the current request.
get_cookie(Key, #context{wm_reqdata=RD}) ->
    wrq:get_cookie_value(Key, RD).
