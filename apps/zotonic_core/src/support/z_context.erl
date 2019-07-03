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
    set_q/2,
    add_q/3,
    add_q/2,
    get_q/2,
    get_q/3,
    get_q_all/1,
    get_q_all/2,
    get_q_all_noz/1,
    get_q_validated/2,

    q_upload_keepalive/2,

    is_zotonic_arg/1,

    lager_md/1,
    lager_md/2,

    client_id/1,
    client_topic/1,

    set/3,
    set/2,
    get/2,
    get/3,
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

    set_security_headers/1,

    set_cookie/3,
    set_cookie/4,
    get_cookie/2,
    get_cookies/2,

    set_state_cookie/2,
    get_state_cookie/1,
    reset_state_cookie/1,

    cookie_domain/1
]).

-include_lib("zotonic.hrl").

-define(STATE_SECRET_COOKIE, <<"z.state">>).


%% @doc Return a new empty context, no request is initialized.
-spec new( z:context() | atom() | cowboy_req:req() ) -> z:context().
new(#context{} = C) ->
    #context{
        site = C#context.site,
        language = C#context.language,
        tz = C#context.tz,
        depcache = C#context.depcache,
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
        undefined -> <<"https:", Url/binary>>;
        https -> <<"https:", Url/binary>>;
        http -> <<"http:", Url/binary>>
    end;
abs_url(<<$/, _/binary>> = Url, Context) ->
    case z_notifier:first(#url_abs{ url = Url }, Context) of
        undefined ->
            Hostname = hostname(Context),
            case z_config:get(ssl_port) of
                443 -> <<"https://", Hostname/binary, Url/binary>>;
                Port -> <<"https://", Hostname/binary, $:, (integer_to_binary(Port))/binary, Url/binary>>
            end;
        AbsUrl ->
            AbsUrl
    end;
abs_url(Url, Context) ->
    case has_url_protocol(Url) of
        true -> Url;
        false -> abs_url(<<$/, Url/binary>>, Context)
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

%% @doc Fetch the protocol for absolute urls referring to the site (always https).
-spec site_protocol(z:context()) -> binary().
site_protocol(_Context) ->
    <<"https">>.

%% @doc Check if the preferred protocol of the site is https (always true)
-spec is_ssl_site(z:context()) -> boolean().
is_ssl_site(_Context) ->
    true.

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
    z_output_html:output(MixedHtml, Context).


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
            ContextQs = Context#context{ props = QPropsUrl },
            % Auth user via cookie - set language
            ContextReq = z_notifier:foldl(#request_context{}, ContextQs, ContextQs),
            % Parse the POST body (if any)
            {Body, ContextParsed} = parse_post_body(ContextReq),
            QPropsAll = (ContextParsed#context.props)#{ q => PathArgs++Body++Query },
            ContextParsed#context{ props = QPropsAll }
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
%%      Always filter the #upload{} arguments to prevent upload of non-temp files.
-spec set_q(binary() | string(), any(), z:context()) -> z:context().
set_q(Key, #upload{ tmpfile = TmpFile } = Upload, Context) when TmpFile =/= undefined ->
    set_q(Key, Upload#upload{ tmpfile = undefined }, Context);
set_q(Key, Value, Context) when is_binary(Key) ->
    Qs = get_q_all(Context),
    Qs1 = lists:keydelete(Key, 1, Qs),
    z_context:set('q', [{Key,Value}|Qs1], Context);
set_q(Key, Value, Context) ->
    set_q(z_convert:to_binary(Key), Value, Context).

%% @doc Set the value of multiple request parameter arguments
-spec set_q( list(), z:context() ) -> z:context().
set_q(KVs, Context) when is_map(KVs) ->
    maps:fold(
        fun(K, V, Ctx) ->
            set_q(K, V, Ctx)
        end,
        Context,
        KVs);
set_q(KVs, Context) when is_list(KVs) ->
    lists:foldl(
        fun({K, V}, Ctx) ->
            set_q(K, V, Ctx)
        end,
        Context,
        KVs).

%% @doc Add the value of a request parameter argument
%%      Always filter the #upload{} arguments to prevent upload of non-temp files.
-spec add_q(binary() | string(), any(), z:context()) -> z:context().
add_q(Key, #upload{ tmpfile = TmpFile } = Upload, Context) when TmpFile =/= undefined ->
    add_q(Key, Upload#upload{ tmpfile = undefined }, Context);
add_q(Key, Value, Context) when is_binary(Key) ->
    Qs = get_q_all(Context),
    z_context:set('q', [{Key,Value}|Qs], Context);
add_q(Key, Value, Context) ->
    set_q(z_convert:to_binary(Key), Value, Context).

%% @doc Add the value of multiple request parameter arguments
-spec add_q( list(), z:context() ) -> z:context().
add_q(KVs, Context) ->
    lists:foldl(
        fun({K, V}, Ctx) ->
            add_q(K, V, Ctx)
        end,
        Context,
        KVs).

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
is_zotonic_arg(<<"zotonic_http_", _/binary>>) -> true;
is_zotonic_arg(<<"zotonic_topic_", _/binary>>) -> true;
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
is_zotonic_arg(<<"z_submitter">>) -> true;
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


%% @doc Keep the tempfiles alive by attaching the current process to its monitors
-spec q_upload_keepalive( boolean(), z:context() ) -> ok.
q_upload_keepalive(true, Context) ->
    Qs = get_q_all(Context),
    lists:map(
        fun
            (#upload{ tmpmonitor = Pid }) when is_pid(Pid) ->
                z_tempfile:monitored_attach(Pid);
            (_) ->
                ok
        end,
        Qs),
    ok;
q_upload_keepalive(false, Context) ->
    Qs = get_q_all(Context),
    lists:map(
        fun
            (#upload{ tmpmonitor = Pid }) when is_pid(Pid) ->
                z_tempfile:monitored_detach(Pid);
            (_) ->
                ok
        end,
        Qs),
    ok.

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


%% @doc Return the current client id (if any)
-spec client_id( z:context() ) -> binary() | undefined.
client_id(#context{ client_id = ClientId }) ->
    ClientId.

%% @doc Return the current client bridge topic (if any)
-spec client_topic( z:context() ) -> mqtt_sessions:topic() | undefined.
client_topic(#context{ client_topic = ClientTopic }) ->
    ClientTopic.

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
        error -> get_maybe_path_info(Key, Context, Default)
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
-spec get_all( z:context() ) -> list().
get_all(Context) ->
    maps:to_list(Context#context.props).


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
    Langs1 = lists:filter(fun z_language:is_valid/1, Langs),
    Context#context{language=Langs1};
set_language(Lang, Context) ->
    case z_language:is_valid(Lang) of
        true -> set_language(z_convert:to_atom(Lang), Context);
        false -> Context
    end.

%% @doc Return the selected timezone of the Context; defaults to the site's timezone
-spec tz(z:context()) -> binary().
tz(#context{ tz = TZ }) when TZ =/= undefined; TZ =/= <<>> ->
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
-spec set_tz(string()|binary()|boolean(), z:context()) -> z:context().
set_tz(Tz, Context) when is_list(Tz) ->
    set_tz(unicode:characters_to_binary(Tz, utf8), Context);
set_tz(Tz, Context) when is_binary(Tz), Tz =/= <<>> ->
    case m_l10n:is_timezone(Tz) of
        true ->
            Context#context{ tz = Tz };
        false ->
            lager:info("Dropping unknown timezone: ~p", [ Tz ]),
            Context
    end;
set_tz(true, Context) ->
    Context#context{ tz = <<"UTC">> };
set_tz(1, Context) ->
    Context#context{ tz = <<"UTC">> };
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
            FileArgs = [
                {Name, #upload{filename=Filename, tmpfile=TmpFile, tmpmonitor=TmpPid}}
                || {Name, Filename, TmpFile, TmpPid} <- Form#multipart_form.files
            ],
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
    cowmachine_req:set_resp_headers([
            {<<"cache-control">>, <<"no-store, no-cache, must-revalidate, post-check=0, pre-check=0">>},
            {<<"expires">>, <<"Wed, 10 Dec 2008 14:30:00 GMT">>},
            {<<"p3p">>, <<"CP=\"NOI ADM DEV PSAi COM NAV OUR OTRo STP IND DEM\"">>},
            {<<"pragma">>, <<"nocache">>}
        ],
        Context).

%% @doc Set security related headers. This can be modified by observing the
%%      'security_headers' notification.
-spec set_security_headers( z:context() ) -> z:context().
set_security_headers(Context) ->
    Default = [ {<<"x-xss-protection">>, <<"1">>},
                {<<"x-content-type-options">>, <<"nosniff">>},
                {<<"x-permitted-cross-domain-policies">>, <<"none">>},
                {<<"referrer-policy">>, <<"origin-when-cross-origin">>} ],
    Default1 = case z_context:get(allow_frame, Context, false) of
        true -> Default;
        false -> [ {<<"x-frame-options">>, <<"sameorigin">>} | Default ]
    end,
    SecurityHeaders = case z_notifier:first(#security_headers{ headers = Default1 }, Context) of
        undefined -> Default1;
        Custom -> Custom
    end,
    cowmachine_req:set_resp_headers(SecurityHeaders, Context).

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
    % Add domain to cookie if not set
    ValueBin = z_convert:to_binary(Value),
    Options1 = case proplists:lookup(domain, Options) of
                   {domain, _} -> Options;
                   none -> [{domain, z_context:cookie_domain(Context)}|Options]
               end,
    Options2 = [ {secure, true} | proplists:delete(secure, Options1) ],
    cowmachine_req:set_resp_cookie(Key, ValueBin, Options2, Context).

%% @doc Read a cookie value from the current request.
-spec get_cookie(binary(), z:context()) -> binary() | undefined.
get_cookie(Key, #context{req=Req} = Context) when is_map(Req) ->
    cowmachine_req:get_cookie_value(Key, Context).

%% @doc Read all cookie values with a certain key from the current request.
-spec get_cookies(binary(), z:context()) -> [ binary() ].
get_cookies(Key, #context{req=Req} = Context) when is_map(Req), is_binary(Key) ->
    proplists:get_all_values(Key, cowmachine_req:req_cookie(Context)).


%% @doc Set a cookie on the user-agent, holding secret information.
%%      The state cookie is used during OAuth key exchanges, against
%%      csrf attacks.
-spec set_state_cookie( term(), z:context() ) -> z:context().
set_state_cookie( Data, Context ) ->
    Secret = state_cookie_secret(Context),
    Encoded = termit:encode_base64(Data, Secret),
    Opts = [
        {path, <<"/">>},
        {http_only, true},
        {secure, true},
        {same_site, lax}
    ],
    set_cookie(?STATE_SECRET_COOKIE, Encoded, Opts, Context).

%% @doc Get the state cookie and decode it.
-spec get_state_cookie( z:context() ) -> {ok, term()} | {error, term()}.
get_state_cookie(Context) ->
    case get_cookie(?STATE_SECRET_COOKIE, Context) of
        undefined ->
            {error, enoent};
        Cookie ->
            Secret = state_cookie_secret(Context),
            termit:decode_base64(Cookie, Secret)
    end.

%% @doc Delete the state cookie.
-spec reset_state_cookie( z:context() ) -> z:context().
reset_state_cookie(Context) ->
    Opts = [
        {max_age, 0},
        {path, <<"/">>},
        {http_only, true},
        {secure, true},
        {same_site, lax}
    ],
    set_cookie(?STATE_SECRET_COOKIE, <<>>, Opts, Context).

%% @doc Return the secret used to encode the state cookie.
-spec state_cookie_secret( z:context() ) -> binary().
state_cookie_secret(Context) ->
    case m_config:get_value(site, state_cookie_secret, Context) of
        None when None =:= undefined; None =:= <<>> ->
            Secret = z_ids:id(32),
            m_config:set_value(site, state_cookie_secret, Secret, Context),
            Secret;
        Secret when is_binary(Secret) ->
            Secret
    end.
