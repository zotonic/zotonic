%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2018  Marc Worrell
%% @doc Request context for Zotonic request evaluation.

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

-module(z_context).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    new/1,
    new/2,

    new_tests/0,

    site/1,
    hostname/1,
    hostname_port/1,
    hostname_ssl_port/1,
    site_protocol/1,
    is_ssl_site/1,

    db_pool/1,
    db_driver/1,

    is_request/1,
    is_session/1,

    prune_for_spawn/1,
    prune_for_async/1,
    prune_for_database/1,
    prune_for_scomp/1,

    abs_url/2,

    pickle/1,
    depickle/1,
    depickle_site/1,

    % combine_results/2,

    get_reqdata/1,
    set_reqdata/2,
    get_controller_module/1,
    set_controller_module/2,
    get_render_state/1,
    set_render_state/2,

    output/2,

    ensure_qs/1,

    set_q/3,
    get_q/2,
    get_q/3,
    get_q_all/1,
    get_q_all/2,
    get_q_all_noz/1,
    get_q_validated/2,

    is_zotonic_arg/1,

    % add_script_session/1,
    % add_script_page/1,
    % add_script_session/2,
    % add_script_page/2,

    % spawn_link_session/4,
    % spawn_link_page/4,

    lager_md/1,
    lager_md/2,

    get_value/2,

    set_session/3,
    get_session/2,
    get_session/3,
    incr_session/3,

    set_page/3,
    get_page/2,
    incr_page/3,

    client_id/1,

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
    fallback_language/1,
    set_language/2,

    tz/1,
    tz_config/1,
    set_tz/2,

    set_resp_header/3,
    get_resp_header/2,
    get_req_header/2,

    get_req_path/1,

    set_nocache_headers/1,
    set_noindex_header/1,
    set_noindex_header/2,

    set_cookie/3,
    set_cookie/4,
    get_cookie/2,
    get_cookies/2,

    cookie_domain/1
]).

-include_lib("zotonic.hrl").

%% @doc Return a new empty context, no request is initialized.
-spec new( z:context() | atom() | cowboy_req:req() ) -> z:context().
new(#context{} = C) ->
    #context{
        site = C#context.site,
        language = C#context.language,
        tz = C#context.tz,
        depcache = C#context.depcache,
        % session_manager=C#context.session_manager,
        dispatcher = C#context.dispatcher,
        template_server = C#context.template_server,
        scomp_server = C#context.scomp_server,
        dropbox_server = C#context.dropbox_server,
        pivot_server = C#context.pivot_server,
        module_indexer = C#context.module_indexer,
        db = C#context.db,
        translation_table = C#context.translation_table
    };
new(undefined) ->
    % TODO: check if the fallback site is running
    case z_sites_dispatcher:get_fallback_site() of
        {ok, Site} -> new(Site);
        undefined -> throw({error, no_site_enabled})
    end;
new(Site) when is_atom(Site) ->
    set_default_language_tz(
        set_server_names(#context{ site = Site }));
new(Req) when is_map(Req) ->
    %% This is the requesting thread, enable simple memo functionality.
    z_memo:enable(),
    z_depcache:in_process(true),
    Context = set_server_names(#context{ req = Req, site = site(Req)}),
    set_default_language_tz(Context).

%% @doc Create a new context record for a site with a certain language
-spec new( atom() | cowboy_req:req(), atom() ) -> z:context().
new(Site, Lang) when is_atom(Site), is_atom(Lang) ->
    Context = set_server_names(#context{ site = Site }),
    Context#context{
        language = [ Lang ],
        tz = tz_config(Context)
    };
%% @doc Create a new context record for the current request and resource module
new(Req, Module) when is_map(Req), is_atom(Module) ->
    Context = new(Req),
    Context#context{ controller_module = Module }.


-spec set_default_language_tz(z:context()) -> z:context().
set_default_language_tz(Context) ->
    try
        F = fun() ->
            {z_language:default_language(Context), tz_config(Context)}
        end,
        {DefaultLang, TzConfig} = z_depcache:memo(F, default_language_tz, ?DAY, [config], Context),
        Context#context{
            language = [DefaultLang],
            tz = TzConfig
        }
    catch
        error:badarg ->
            % The depache is gone, happens during race conditions on site shutdown.
            % Silently return a default.
            Context#context{
                language = [ en ],
                tz = z_config:get(timezone)
            }
    end.

% @doc Create a new context used when testing parts of zotonic
new_tests() ->
    Context = z_trans_server:set_context_table(
            #context{
                site = test,
                language = [en],
                tz = <<"UTC">>
            }),
    case ets:info(Context#context.translation_table) of
        undefined ->
            ets:new(Context#context.translation_table,
                    [named_table, set, protected,  {read_concurrency, true}]);
        _TabInfo ->
            ok
    end,
    Context.


%% @doc Set all server names for the given site.
-spec set_server_names( z:context() ) -> z:context().
set_server_names(#context{ site = Site } = Context) ->
    SiteAsList = [ $$ | atom_to_list(Site) ],
    Context1 = Context#context{
        depcache = list_to_atom("z_depcache"++SiteAsList),
        dispatcher = list_to_atom("z_dispatcher"++SiteAsList),
        template_server = list_to_atom("z_template"++SiteAsList),
        scomp_server = list_to_atom("z_scomp"++SiteAsList),
        dropbox_server = list_to_atom("z_dropbox"++SiteAsList),
        pivot_server = list_to_atom("z_pivot_rsc"++SiteAsList),
        module_indexer = list_to_atom("z_module_indexer"++SiteAsList),
        translation_table = z_trans_server:table(Site)
    },
    Context1#context{
        % session_manager=list_to_atom("z_session_manager"++SiteAsList),
        db = { z_db_pool:db_pool_name(Site), z_db_pool:db_driver(Context1) }
    }.


%% @doc Maps the site in the request to a site in the sites folder.
-spec site(z:context() | cowboy_req:req()) -> atom().
site(#context{site=Site}) ->
    Site;
site(Req) when is_map(Req) ->
    case maps:get(cowmachine_site, Req, undefined) of
        undefined ->
            {ok, Site} = z_sites_dispatcher:get_fallback_site(),
            Site;
        Site when is_atom(Site) ->
            Site
    end.


%% @doc Return the preferred hostname from the site configuration
-spec hostname( z:context() ) -> binary().
hostname(Context) ->
    case z_dispatcher:hostname(Context) of
        undefined -> <<"localhost">>;
        <<>> -> <<"localhost">>;
        Hostname -> Hostname
    end.

%% @doc Return the hostname (and port) for http from the site configuration
-spec hostname_port( z:context() ) -> binary() | undefined.
hostname_port(Context) ->
    case z_dispatcher:hostname_port(Context) of
        Empty when Empty =:= undefined; Empty =:= <<>> ->
            case z_config:get(port) of
                none -> undefined;
                80 -> <<"localhost">>;
                Port -> <<"localhost:", (integer_to_binary(Port))/binary>>
            end;
        Hostname ->
            Hostname
    end.

%% @doc Return the hostname (and port) for https from the site configuration
-spec hostname_ssl_port( z:context() ) -> binary() | undefined.
hostname_ssl_port(Context) ->
    case z_dispatcher:hostname_ssl_port(Context) of
        Empty when Empty =:= undefined; Empty =:= <<>> ->
            case z_config:get(ssl_port) of
                none -> undefined;
                443 -> <<"localhost">>;
                Port -> <<"localhost:", (integer_to_binary(Port))/binary>>
            end;
        Hostname ->
            Hostname
    end.

%% @doc Check if the current context is a request context
-spec is_request( z:context() ) -> boolean().
is_request(#context{ req = Req }) -> Req =/= undefined.

%% @doc Check if the current context has an active MQTT session.
%%      This is never true for the first request.
-spec is_session( z:context() ) -> boolean().
is_session(#context{ client_topic = ClientTopic }) ->
    is_binary(ClientTopic).


%% @doc Minimal prune, for ensuring that the context can safely used in two processes
-spec prune_for_spawn( z:context() ) -> z:context().
prune_for_spawn(#context{} = Context) ->
    Context#context{
        dbc = undefined
    }.

%% @doc Make the context safe to use in a async message. This removes render_state and the db transaction.
-spec prune_for_async( z:context() ) -> z:context().
prune_for_async(#context{} = Context) ->
    Context#context{
        dbc = undefined,
        render_state = undefined
    }.

%% @doc Cleanup a context so that it can be used exclusively for database connections
-spec prune_for_database( z:context() ) -> z:context().
prune_for_database(Context) ->
    #context{
        site = Context#context.site,
        db = Context#context.db,
        dbc = Context#context.dbc,
        depcache = Context#context.depcache,
        % session_manager=Context#context.session_manager,
        dispatcher = Context#context.dispatcher,
        template_server = Context#context.template_server,
        scomp_server = Context#context.scomp_server,
        dropbox_server = Context#context.dropbox_server,
        pivot_server = Context#context.pivot_server,
        module_indexer = Context#context.module_indexer
    }.


%% @doc Cleanup a context for cacheable scomp handling. Resets most the render_state to prevent duplicating
%% between different (cached) renderings.
-spec prune_for_scomp( z:context() ) -> z:context().
prune_for_scomp(Context) ->
    Context#context{
        dbc = undefined,
        req = prune_reqdata(Context#context.req),
        render_state = undefined
    }.

prune_reqdata(undefined) ->
    undefined;
prune_reqdata(Req) ->
    %% @todo: prune this better, also used by the websocket connection.
    Req#{
        bindings => [],
        cowmachine_cookies => [],
        cowmachine_resp_body => <<>>,
        headers => #{},
        path => <<>>,
        qs => <<>>,
        pid => undefined,
        streamid => undefined
    }.

%% @doc Make the url an absolute url by prepending the hostname.
-spec abs_url(iolist(), z:context()) -> binary().
abs_url(Url, Context) when is_list(Url) ->
    abs_url(iolist_to_binary(Url), Context);
abs_url(<<"//", _/binary>> = Url, Context) ->
    case m_req:get(scheme, Context) of
        undefined ->
            case is_ssl_site(Context) of
                true -> <<"https:", Url/binary>>;
                false -> <<"http:", Url/binary>>
            end;
        https -> <<"https:", Url/binary>>;
        http -> <<"http:", Url/binary>>
    end;
abs_url(<<$/, _/binary>> = Url, Context) ->
    case z_notifier:first(#url_abs{url=Url}, Context) of
        undefined ->
            Hostname = hostname(Context),
            case request_scheme_port(Context) of
                {https, 443} -> <<"https://", Hostname/binary, Url/binary>>;
                {http, 80} -> <<"http://", Hostname/binary, Url/binary>>;
                {https, Port} -> <<"https://", Hostname/binary, $:, (integer_to_binary(Port))/binary, Url/binary>>;
                {http, Port} -> <<"http://", Hostname/binary, $:, (integer_to_binary(Port))/binary, Url/binary>>
            end;
        AbsUrl ->
            AbsUrl
    end;
abs_url(Url, Context) ->
    case has_url_protocol(Url) of
        true -> Url;
        false -> abs_url(<<$/, Url/binary>>, Context)
    end.

request_scheme_port(Context) ->
    case m_req:get(scheme, Context) of
        https ->
            {https, z_config:get(ssl_port)};
        _ ->
            case is_ssl_site(Context) of
                true -> {https, z_config:get(ssl_port)};
                false -> {http, z_config:get(port)}
            end
    end.

has_url_protocol(<<"http:">>) -> true;
has_url_protocol(<<"https:">>) -> true;
has_url_protocol(<<"ws:">>) -> true;
has_url_protocol(<<"wss:">>) -> true;
has_url_protocol(<<"ftp:">>) -> true;
has_url_protocol(<<"email:">>) -> true;
has_url_protocol(<<"file:">>) -> true;
has_url_protocol(<<H, T/binary>>) when H >= $a andalso H =< $z ->
    has_url_protocol_1(T);
has_url_protocol(_) ->
    false.

has_url_protocol_1(<<H, T/binary>>) when H >= $a andalso H =< $z ->
    has_url_protocol_1(T);
has_url_protocol_1(<<$:, _/binary>>) -> true;
has_url_protocol_1(_) -> false.


%% @doc Fetch the pid of the database worker pool for this site
-spec db_pool(z:context()) -> atom().
db_pool(#context{ db = {Pool, _Driver} }) ->
    Pool.

%% @doc Fetch the database driver module for this site
-spec db_driver(z:context()) -> atom().
db_driver(#context{ db = {_Pool, Driver} }) ->
    Driver.

%% @doc Fetch the protocol for absolute urls referring to the site (defaults to http).
%%      Useful when the site is behind a https proxy.
-spec site_protocol(z:context()) -> binary().
site_protocol(Context) ->
    case is_ssl_site(Context) of
        true -> <<"https">>;
        false -> <<"http">>
    end.

%% @doc Check if the preferred protocol of the site is https
-spec is_ssl_site(z:context()) -> boolean().
is_ssl_site(Context) ->
    case z_config:get(ssl_port) of
        none -> false;
        _PortSsl ->
            case z_config:get(port) of
                none -> true;
                _Port ->
                    case z_config:get(ssl_only) of
                        true -> true;
                        false ->
                            z_convert:to_bool(m_config:get_value(site, ssl_only, Context))
                    end
            end
    end.

%% @doc Pickle a context for storing in the database
-spec pickle( z:context() ) -> tuple().
pickle(Context) ->
    {pickled_context,
        Context#context.site,
        Context#context.user_id,
        Context#context.language,
        Context#context.tz,
        undefined}.

%% @doc Depickle a context for restoring from a database
-spec depickle( tuple() ) -> z:context().
depickle({pickled_context, Site, UserId, Language, _VisitorId}) ->
    depickle({pickled_context, Site, UserId, Language, 0, _VisitorId});
depickle({pickled_context, Site, UserId, Language, Tz, _VisitorId}) ->
    Context = set_server_names(#context{ site = Site, language = Language, tz = Tz }),
    case UserId of
        undefined -> Context;
        _ -> z_acl:logon(UserId, Context)
    end.

%% @doc Depickle a context, return the site name.
-spec depickle_site( tuple() ) -> z:context().
depickle_site({pickled_context, Site, _UserId, _Language, _VisitorId}) ->
    Site;
depickle_site({pickled_context, Site, _UserId, _Language, _Tz, _VisitorId}) ->
    Site.


% %% @doc After ensuring a session, try to log on from the user-id stored in the session
% maybe_logon_from_session(#context{user_id=undefined} = Context) ->
%     Context1 = z_auth:logon_from_session(Context),
%     Context2 = z_notifier:foldl(#session_context{}, Context1, Context1),
%     set_nocache_headers(Context2);
% maybe_logon_from_session(Context) ->
%     Context.

-spec output( MixedHtml::term(), z:context() ) -> {iolist(), z:context()}.
output(MixedHtml, Context) ->
    z_render:render_html(MixedHtml, Context).

%% @doc Ensure that we have parsed the query string, fetch body if necessary.
%%      If this is a POST then the session/page-session might be continued after this call.
ensure_qs(#context{ props = Props } = Context) ->
    case maps:find('q', Props) of
        {ok, _Qs} ->
            Context;
        error ->
            Query = cowmachine_req:req_qs(Context),
            PathInfo = cowmachine_req:path_info(Context),
            PathArgs = [ {z_convert:to_binary(T), V} || {T,V} <- PathInfo ],
            QPropsUrl = Props#{ q => PathArgs++Query },
            {Body, ContextParsed} = parse_post_body(Context#context{ props = QPropsUrl }),
            QPropsAll = (ContextParsed#context.props)#{ q => PathArgs++Body++Query },
            ContextQs = ContextParsed#context{ props = QPropsAll },
            z_notifier:foldl(#request_context{}, ContextQs, ContextQs)
    end.


%% @doc Return the webmachine request data of the context
-spec get_reqdata(z:context()) -> cowboy_req:req() | undefined.
get_reqdata(Context) ->
    Context#context.req.

%% @doc Set the webmachine request data of the context
-spec set_reqdata(cowboy_req:req() | undefined, z:context()) -> z:context().
set_reqdata(Req, Context) when is_map(Req); Req =:= undefined ->
    Context#context{req=Req}.


%% @doc Get the resource module handling the request.
-spec get_controller_module(z:context()) -> atom() | undefined.
get_controller_module(Context) ->
    Context#context.controller_module.

-spec set_controller_module(Module::atom(), z:context()) -> z:context().
set_controller_module(Module, Context) ->
    Context#context{controller_module=Module}.

-spec get_render_state( z:context() ) -> term().
get_render_state(#context{ render_state = RS }) ->
    RS.

-spec set_render_state( term(), z:context() ) -> z:contextC().
set_render_state(RS, Context) ->
    Context#context{ render_state = RS }.


%% @doc Set the value of a request parameter argument
-spec set_q(string(), any(), z:context()) -> z:context().
set_q(Key, Value, Context) ->
    Qs = get_q_all(Context),
    Qs1 = proplists:delete(Key, Qs),
    z_context:set('q', [{Key,Value}|Qs1], Context).


%% @doc Get a request parameter, either from the query string or the post body.  Post body has precedence over the query string.
-spec get_q(string()|atom()|binary()|list(), z:context()) -> binary() | string() | #upload{} | undefined | list().
get_q([Key|_] = Keys, Context) when is_list(Key); is_atom(Key); is_binary(Key) ->
    lists:foldl(fun(K, Acc) ->
                    case get_q(K, Context) of
                        undefined -> Acc;
                        Value -> [{K, Value}|Acc]
                    end
                end,
                [],
                Keys);
get_q(Key, Context) when is_list(Key) ->
    case get_q(list_to_binary(Key), Context) of
        Value when is_binary(Value) -> binary_to_list(Value);
        Value -> Value
    end;
get_q(Key, #context{ props = Props }) ->
    case maps:find(q, Props) of
        {ok, Qs} -> proplists:get_value(z_convert:to_binary(Key), Qs);
        error -> undefined
    end.


%% @doc Get a request parameter, either from the query string or the post body.  Post body has precedence over the query string.
-spec get_q(binary()|string()|atom(), z:context(), term()) -> binary() | string() | #upload{} | term().
get_q(Key, Context, Default) when is_list(Key) ->
    case get_q(list_to_binary(Key), Context, Default) of
        Value when is_binary(Value) -> binary_to_list(Value);
        Value -> Value
    end;
get_q(Key, #context{ props = Props }, Default) ->
    case maps:find(q, Props) of
        {ok, Qs} -> proplists:get_value(z_convert:to_binary(Key), Qs, Default);
        error -> Default
    end.


%% @doc Get all parameters.
-spec get_q_all(z:context()) -> list({binary(), binary()|#upload{}}).
get_q_all(#context{ props = Props }) ->
    case maps:find(q, Props) of
        {ok, Qs} -> Qs;
        error -> []
    end.


%% @doc Get the all the parameters with the same name, returns the empty list when non found.
-spec get_q_all(string()|atom()|binary(), z:context()) -> list().
get_q_all(Key, Context) when is_list(Key) ->
    Values = get_q_all(z_convert:to_binary(Key), Context),
    [
        case is_binary(V) of
            true -> binary_to_list(V);
            false -> V
        end
        || V <- Values
    ];
get_q_all(Key, #context{ props = Props }) ->
    case maps:find(q, Props) of
        {ok, Qs} -> proplists:get_all_values(z_convert:to_binary(Key), Qs);
        error -> []
    end.


%% @doc Get all query/post args, filter the zotonic internal args.
-spec get_q_all_noz(z:context()) -> list({string(), term()}).
get_q_all_noz(Context) ->
    lists:filter(fun({X,_}) -> not is_zotonic_arg(X) end, z_context:get_q_all(Context)).

is_zotonic_arg(<<"zotonic_host">>) -> true;  % backwards compatibility
is_zotonic_arg(<<"zotonic_site">>) -> true;
is_zotonic_arg(<<"zotonic_dispatch">>) -> true;
is_zotonic_arg(<<"zotonic_dispatch_path">>) -> true;
is_zotonic_arg(<<"zotonic_dispatch_path_rewrite">>) -> true;
is_zotonic_arg(<<"postback">>) -> true;
is_zotonic_arg(<<"triggervalue">>) -> true;
is_zotonic_arg(<<"z_trigger_id">>) -> true;
is_zotonic_arg(<<"z_target_id">>) -> true;
is_zotonic_arg(<<"z_delegate">>) -> true;
is_zotonic_arg(<<"z_message">>) -> true;
is_zotonic_arg(<<"z_transport">>) -> true;
is_zotonic_arg(<<"z_sid">>) -> true;
is_zotonic_arg(<<"z_pageid">>) -> true;
is_zotonic_arg(<<"z_v">>) -> true;
is_zotonic_arg(<<"z_msg">>) -> true;
is_zotonic_arg(<<"z_comet">>) -> true;
is_zotonic_arg(_) -> false.


%% @doc Fetch a query parameter and perform the validation connected to the parameter. An exception {not_validated, Key}
%%      is thrown when there was no validator, when the validator is invalid or when the validation failed.
-spec get_q_validated(string()|atom()|binary(), z:context()) -> string() | term() | undefined.
get_q_validated([Key|_] = Keys, Context) when is_list(Key); is_atom(Key) ->
    lists:foldl(fun (K, Acc) ->
                    case get_q_validated(K, Context) of
                        undefined -> Acc;
                        Value -> [{K, Value}|Acc]
                    end
                end,
                [],
                Keys);
get_q_validated(Key, Context) when is_list(Key) ->
    case get_q_validated(list_to_binary(Key), Context) of
        V when is_binary(V) -> binary_to_list(V);
        V -> V
    end;
get_q_validated(Key, #context{ props = #{ q_validated := Qs } }) ->
    case proplists:lookup(z_convert:to_binary(Key), Qs) of
        {_Key, Value} -> Value;
        none -> throw({not_validated, Key})
    end;
get_q_validated(Key, _Context) ->
    throw({not_validated, Key}).


%% ------------------------------------------------------------------------------------
%% Communicate with pages, session and user processes
%% ------------------------------------------------------------------------------------

% %% @doc Add the script from the context to all pages of the session.
% add_script_session(Context) ->
%     Script = z_script:get_script(Context),
%     add_script_session(Script, Context).


% %% @doc Add the script from the context to the page in the user agent.
% add_script_page(Context) ->
%     Script = z_script:get_script(Context),
%     add_script_page(Script, Context).


% %% @doc Add a script to the all pages of the session. Used for comet feeds.
% add_script_session(Script, Context) ->
%     z_session:add_script(Script, Context#context.session_pid).


% %% @doc Add a script to the page in the user agent.  Used for comet feeds.
% add_script_page(Script, Context) ->
%     z_session_page:add_script(Script, Context#context.page_pid).


% %% @doc Spawn a new process, link it to the session process.
% spawn_link_session(Module, Func, Args, Context) ->
%     z_session:spawn_link(Module, Func, Args, Context).

% %% @doc Spawn a new process, link it to the page process.  Used for comet feeds.
% spawn_link_page(Module, Func, Args, Context) ->
%     z_session_page:spawn_link(Module, Func, Args, Context).


%% ------------------------------------------------------------------------------------
%% Set lager metadata for the current process
%% ------------------------------------------------------------------------------------

lager_md(ContextOrReq) ->
    lager_md([], ContextOrReq).

lager_md(MD, #context{} = Context) when is_list(MD) ->
    RD = get_reqdata(Context),
    lager:md([
            {site, site(Context)},
            {user_id, Context#context.user_id},
            {controller, Context#context.controller_module},
            {dispatch, get(zotonic_dispatch, Context)},
            {method, m_req:get(method, RD)},
            {remote_ip, m_req:get(peer, RD)},
            {is_ssl, m_req:get(is_ssl, RD)},
            % {session_id, Context#context.session_id},
            % {page_id, Context#context.page_id},
            {req_id, m_req:get(req_id, RD)}
            | MD
        ]);
lager_md(MD, Req) when is_map(Req) ->
    PathInfo = cowmachine_req:path_info(Req),
    lager:md([
            {site, z_context:site(Req)},
            {dispatch, proplists:get_value(zotonic_dispatch, PathInfo)},
            {method, m_req:get(method, Req)},
            {remote_ip, m_req:get(peer, Req)},
            {is_ssl, m_req:get(is_ssl, Req)},
            {req_id, m_req:get(req_id, Req)}
            | MD
        ]).

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

%% @doc Return the current client id (if any)
-spec client_id( z:context() ) -> binary() | undefined.
client_id(#context{ client_id = ClientId }) ->
    ClientId.

%% @doc Ensure that we have an id for the visitor
persistent_id(Context) ->
    undefined.
    % z_session:persistent_id(Context).

%% @spec set_persistent(Key, Value, Context) -> Context
%% @doc Set the value of the visitor variable Key to Value
set_persistent(Key, Value, Context) ->
    Context.
    % z_session:set_persistent(Key, Value, Context).


%% @spec get_persistent(Key, Context) -> Value
%% @doc Fetch the value of the visitor variable Key
get_persistent(_Key, Context) ->
    undefined.
    % z_session:get_persistent(Key, Context).


%% @spec set_session(Key, Value, Context) -> Context
%% @doc Set the value of the session variable Key to Value
set_session(Key, Value, Context) ->
    % z_session:set(Key, Value, Context#context.session_pid),
    Context.

%% @spec get_session(Key, Context) -> Value
%% @doc Fetch the value of the session variable Key
get_session(Key, Context) ->
    undefined.
    % z_session:get(Key, Context#context.session_pid).

%% @spec get_session(Key, Context, DefaultValue) -> Value
%% @doc Fetch the value of the session variable Key, falling back to default.
get_session(_Key, Context, DefaultValue) ->
    DefaultValue.
% get_session(Key, Context, DefaultValue) ->
%     z_session:get(Key, Context#context.session_pid, DefaultValue).

%% @spec incr_session(Key, Increment, Context) -> {NewValue, NewContext}
%% @doc Increment the session variable Key
incr_session(Key, Value, Context) ->
    {Value, Context}.
    % {z_session:incr(Key, Value, Context#context.session_pid), Context}.

%% @spec set_page(Key, Value, Context) -> Context
%% @doc Set the value of the page variable Key to Value
set_page(Key, Value, Context) ->
    Context.
    % z_session_page:set(Key, Value, Context#context.page_pid),

%% @spec get_page(Key, Context) -> Value
%% @doc Fetch the value of the page variable Key
get_page(_Key, Context) ->
    undefined.


%% @spec incr_page(Key, Increment, Context) -> {NewValue, NewContext}
%% @doc Increment the page variable Key
incr_page(Key, Value, Context) ->
    {Value, Context}.
    % {z_session_page:incr(Key, Value, Context#context.session_pid), Context}.


%% @spec set(Key, Value, Context) -> Context
%% @doc Set the value of the context variable Key to Value
set(Key, Value, #context{ props = Props } = Context) ->
    Context#context{ props = Props#{ Key => Value } }.


%% @spec set(PropList, Context) -> Context
%% @doc Set the value of the context variables to all {Key, Value} properties.
set(PropList, Context) when is_list(PropList) ->
    NewProps = lists:foldl(
        fun
            ({Key,Value}, Props) -> Props#{ Key => Value };
            (Key, Props) -> Props#{ Key => true }
        end,
        Context#context.props,
        PropList),
    Context#context{ props = NewProps }.


%% @spec get(Key, Context) -> Value | undefined
%% @doc Fetch the value of the context variable Key, return undefined when Key is not found.
get(Key, Context) ->
    get(Key, Context, undefined).

%% @spec get(Key, Context, Default) -> Value | Default
%% @doc Fetch the value of the context variable Key, return Default when Key is not found.
get(Key, Context, Default) ->
    get_1(Key, Context, Default).

get_1(Key, #context{ props = Props } = Context, Default) ->
    case maps:find(Key, Props) of
        {ok, Value} -> Value;
        false -> get_maybe_path_info(Key, Context, Default)
    end.

get_maybe_path_info(z_language, Context, _Default) ->
    z_context:language(Context);
get_maybe_path_info(zotonic_site, Context, _Default) ->
    z_context:site(Context);
get_maybe_path_info(zotonic_dispatch, Context, Default) ->
    get_path_info(zotonic_dispatch, Context, Default);
get_maybe_path_info(zotonic_dispatch_path, Context, Default) ->
    get_path_info(zotonic_dispatch_path, Context, Default);
get_maybe_path_info(zotonic_dispatch_path_rewrite, Context, Default) ->
    get_path_info(zotonic_dispatch_path_rewrite, Context, Default);
get_maybe_path_info(_, _Context, Default) ->
    Default.

get_path_info(Key, Context, Default) ->
    case cowmachine_req:req(Context) of
        undefined ->
            Default;
        Req ->
            case lists:keyfind(Key, 1, cowmachine_req:path_info(Req)) of
                {Key, Value} -> Value;
                false -> Default
            end
    end.


%% @doc Return a proplist with all context variables.
-spec get_all( z:context() ) -> map().
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
-spec language(z:context()) -> atom().
language(Context) ->
    % A check on atom must exist because the language setting may be stored in mnesia and
    % passed to the context when the site starts
    case Context#context.language of
        [Language|_] -> Language;
        Language -> Language
    end.

%% @doc Return the first fallback language of the Context
-spec fallback_language(z:context()) -> atom().
fallback_language(Context) ->
    % Take the second item of the list, if it exists
    case Context#context.language of
        [_|[Fallback|_]] -> Fallback;
        _ -> undefined
    end.

%% @doc Set the language of the context, either an atom (language) or a list (language and fallback languages)
-spec set_language(atom()|binary()|string()|list(), z:context()) -> z:context().
set_language('x-default', Context) ->
    Lang = z_language:default_language(Context),
    Context#context{language=[Lang,'x-default']};
set_language(Lang, Context) when is_atom(Lang) ->
    Context#context{language=[Lang]};
set_language(Langs, Context) when is_list(Langs) ->
    Context#context{language=Langs};
set_language(Lang, Context) ->
    case z_language:is_valid(Lang) of
        true -> set_language(z_convert:to_atom(Lang), Context);
        false -> Context
    end.

%% @doc Return the selected timezone of the Context; defaults to the site's timezone
-spec tz(z:context()) -> binary().
tz(#context{tz=TZ}) when TZ =/= undefined; TZ =/= <<>> ->
    TZ;
tz(Context) ->
    tz_config(Context).

%% @doc Return the site's configured timezone.
-spec tz_config(z:context()) -> binary().
tz_config(Context) ->
    case m_config:get_value(mod_l10n, timezone, Context) of
        None when None =:= undefined; None =:= <<>> ->
            z_config:get(timezone);
        TZ ->
            TZ
    end.

%% @doc Set the timezone of the context.
-spec set_tz(string()|binary(), z:context()) -> z:context().
set_tz(Tz, Context) when is_list(Tz) ->
    set_tz(z_convert:to_binary(Tz), Context);
set_tz(Tz, Context) when is_binary(Tz), Tz =/= <<>> ->
    Context#context{tz=z_convert:to_binary(Tz)};
set_tz(true, Context) ->
    Context#context{tz= <<"UTC">>};
set_tz(1, Context) ->
    Context#context{tz= <<"UTC">>};
set_tz(Tz, Context) ->
    lager:error("Unknown timezone ~p", [Tz]),
    Context.

%% @doc Set a response header for the request in the context.
%% @spec set_resp_header(Header, Value, Context) -> NewContext
-spec set_resp_header(binary(), binary(), z:context()) -> z:context().
set_resp_header(Header, Value, #context{req=Req} = Context) when is_map(Req) ->
    cowmachine_req:set_resp_header(Header, Value, Context).

%% @doc Get a response header
-spec get_resp_header(binary(), z:context()) -> binary() | undefined.
get_resp_header(Header, #context{req=Req} = Context) when is_map(Req) ->
    cowmachine_req:get_resp_header(Header, Context).

%% @doc Get a request header. The header MUST be in lower case.
-spec get_req_header(binary(), z:context()) -> binary() | undefined.
get_req_header(Header, #context{req=Req} = Context) when is_map(Req) ->
    cowmachine_req:get_req_header(Header, Context).


%% @doc Return the request path
-spec get_req_path(z:context()) -> binary().
get_req_path(#context{req=Req} = Context) when is_map(Req) ->
    cowmachine_req:raw_path(Context).


%% @doc Fetch the cookie domain, defaults to 'undefined' which will equal the domain
%% to the domain of the current request.
-spec cookie_domain(z:context()) -> binary() | undefined.
cookie_domain(Context) ->
    case m_site:get(cookie_domain, Context) of
        Empty when Empty =:= undefined; Empty =:= []; Empty =:= <<>> ->
            undefined;
        Domain ->
            z_convert:to_binary(Domain)
    end.


%% ------------------------------------------------------------------------------------
%% Local helper functions
%% ------------------------------------------------------------------------------------

%% @doc Return the keys in the body of the request, only if the request is application/x-www-form-urlencoded
-spec parse_post_body(z:context()) -> {list({binary(),binary()}), z:context()}.
parse_post_body(Context) ->
    case cowmachine_req:get_req_header(<<"content-type">>, Context) of
        <<"application/x-www-form-urlencoded", _/binary>> ->
            case cowmachine_req:req_body(Context) of
                {undefined, Context1} ->
                    {[], Context1};
                {Body, Context1} ->
                    {cowmachine_util:parse_qs(Body), Context1}
            end;
        <<"multipart/form-data", _/binary>> ->
            {Form, ContextRcv} = z_parse_multipart:recv_parse(Context),
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
-spec set_nocache_headers(z:context()) -> z:context().
set_nocache_headers(Context = #context{req=Req}) when is_map(Req) ->
    C1 = cowmachine_req:set_resp_header(<<"cache-control">>, <<"no-store, no-cache, must-revalidate, post-check=0, pre-check=0">>, Context),
    C2 = cowmachine_req:set_resp_header(<<"expires">>, <<"Wed, 10 Dec 2008 14:30:00 GMT">>, C1),
    % This let IE accept our cookies, basically we tell IE that our cookies do not contain any private data.
    cowmachine_req:set_resp_header(<<"p3p">>, <<"CP=\"NOI ADM DEV PSAi COM NAV OUR OTRo STP IND DEM\"">>, C2).

%% @doc Set the noindex header if the config is set, or the webmachine resource opt is set.
-spec set_noindex_header(z:context()) -> z:context().
set_noindex_header(Context) ->
    set_noindex_header(false, Context).

%% @doc Set the noindex header if the config is set, the webmachine resource opt is set or Force is set.
-spec set_noindex_header(Force::term(), z:context()) -> z:context().
set_noindex_header(Force, Context) ->
    case z_convert:to_bool(m_config:get_value(seo, noindex, Context))
         orelse get(seo_noindex, Context, false)
         orelse z_convert:to_bool(Force)
    of
       true -> set_resp_header(<<"x-robots-tag">>, <<"noindex">>, Context);
       _ -> Context
    end.

%% @doc Set a cookie value with default options.
-spec set_cookie(binary(), binary(), z:context()) -> z:context().
set_cookie(Key, Value, Context) ->
    set_cookie(Key, Value, [], Context).

%% @doc Set a cookie value with cookie options.
-spec set_cookie(binary(), binary(), list(), z:context()) -> z:context().
set_cookie(Key, Value, Options, Context) ->
    case controller_websocket:is_websocket_request(Context) of
        true ->
            % Store the cookie in the session and trigger an ajax cookie fetch.
            z_session:add_cookie(Key, Value, Options, Context),
            % add_script_page(<<"z_fetch_cookies();">>, Context),
            Context;
        false ->
            % Add domain to cookie if not set
            ValueBin = z_convert:to_binary(Value),
            Options1 = case proplists:lookup(domain, Options) of
                           {domain, _} -> Options;
                           none -> [{domain, z_context:cookie_domain(Context)}|Options]
                       end,
            Options2 = secure_cookie_options(Key, Options1, Context),
            Options3 = z_notifier:foldl(#cookie_options{name=Key, value=ValueBin}, Options2, Context),
            cowmachine_req:set_resp_cookie(Key, ValueBin, Options3, Context)
    end.

%% @doc Ensure that the session and autologon cookies are set to 'secure' (ssl only).
secure_cookie_options(Name, Options, Context) ->
    case is_ssl_site(Context) of
        true ->
            secure_cookie(Options);
        false ->
            case m_req:get(is_ssl, Context) 
                % andalso is_session_cookie(Name, Context)
                andalso z_convert:to_bool(m_config:get(site, secure_session_cookie, Context))
            of
                true -> secure_cookie(Options);
                false -> Options
            end
    end.

secure_cookie(Options) ->
    [{secure, true} | proplists:delete(secure, Options)].

% is_session_cookie(<<"z_sid">>, _Context) -> true;
% is_session_cookie(<<"z_logon">>, _Context) -> true;
% is_session_cookie(Cookie, Context) when is_binary(Cookie) ->
%     z_session_manager:get_session_cookie_name(Context) =:= Cookie.


%% @doc Read a cookie value from the current request.
-spec get_cookie(binary(), z:context()) -> binary() | undefined.
get_cookie(Key, #context{req=Req} = Context) when is_map(Req) ->
    cowmachine_req:get_cookie_value(Key, Context).

%% @doc Read all cookie values with a certain key from the current request.
-spec get_cookies(binary(), z:context()) -> [ binary() ].
get_cookies(Key, #context{req=Req} = Context) when is_map(Req), is_binary(Key) ->
    proplists:get_all_values(Key, cowmachine_req:req_cookie(Context)).
