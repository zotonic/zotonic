%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2023  Marc Worrell
%% @doc Request context for Zotonic request evaluation.
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

-module(z_context).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    new/1,
    new/2,
    new/3,

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
    is_hostname_redirect_configured/1,

    prune_for_spawn/1,
    prune_for_async/1,
    prune_for_database/1,
    prune_for_scomp/1,

    is_site_url/2,
    site_url/2,
    abs_url/2,

    pickle/1,
    depickle/1,
    depickle_site/1,

    init_cowdata/3,

    get_reqdata/1,
    set_reqdata/2,

    get_envdata/1,
    set_envdata/2,

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
    delete_q/2,
    get_q/2,
    get_q/3,
    get_qargs/1,
    get_q_all/1,
    get_q_all/2,
    get_q_all_noz/1,
    get_q_validated/2,
    get_q_map/1,
    get_q_map_noz/1,
    set_q_all/2,

    q_upload_keepalive/2,

    without_zotonic_args/1,
    is_zotonic_arg/1,

    logger_md/1,
    logger_md/2,
    ensure_logger_md/1,

    client_id/1,
    client_topic/1,
    set_client_context/2,

    session_id/1,
    set_session_id/2,

    set/3,
    set/2,
    get/2,
    get/3,
    get_all/1,

    language/1,
    languages/1,
    set_language/2,

    tz/1,
    tz_config/1,
    set_tz/2,

    set_csp_nonce/1,
    csp_nonce/1,

    set_resp_header/3,
    set_resp_headers/2,
    get_resp_header/2,
    get_req_header/2,

    get_req_path/1,

    set_req_metrics/2,

    set_nocache_headers/1,
    set_noindex_header/1,
    set_noindex_header/2,

    set_resource_headers/2,

    set_security_headers/1,
    set_cors_headers/2,

    set_cookie/3,
    set_cookie/4,
    get_cookie/2,
    get_cookies/2,

    set_state_cookie/2,
    get_state_cookie/1,
    state_cookie_secret/1,
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
        set_server_names(#context{ site = Site })).

%% @doc Create a new context record for a site with a certain language
-spec new( Site :: atom(), Language :: atom() | [ atom() ] ) -> z:context().
new(Site, Lang) when is_atom(Site) ->
    Context = new(Site),
    set_language(Lang, Context).

-spec new( Site :: atom(), Language :: atom() | [ atom() ], Timezone :: binary() ) -> z:context().
new(Site, Lang, Timezone) when is_atom(Site), is_binary(Timezone) ->
    Context = new(Site, Lang),
    Context#context{ tz = Timezone }.


-spec set_default_language_tz(z:context()) -> z:context().
set_default_language_tz(Context) ->
    try
        F = fun() ->
            {z_language:enabled_languages(Context), tz_config(Context)}
        end,
        {DefaultLangs, TzConfig} = z_depcache:memo(F, default_language_tz, ?DAY, [config], Context),
        Context#context{
            language = DefaultLangs,
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
                language = [ en ],
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
        <<"none">> ->
            case is_request(Context) of
                true -> cowmachine_req:host(Context);
                false -> <<"localhost">>
            end;
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
is_request(#context{ cowreq = undefined }) -> false;
is_request(#context{}) -> true.

%% @doc Check if the current context has an active MQTT session.
%%      This is never true for the first request.
-spec is_session( z:context() ) -> boolean().
is_session(#context{ client_topic = undefined }) ->
    false;
is_session(#context{ client_topic = ClientTopic }) ->
    is_list(ClientTopic).

%% @doc Return true if redirect to the preferred hostname is configured
%%      for the current site.
-spec is_hostname_redirect_configured( z:context() ) -> boolean().
is_hostname_redirect_configured(Context) ->
    case m_site:get(redirect, Context) of
        undefined -> true;
        R -> z_convert:to_bool(R)
    end.

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
        cowreq = prune_reqdata(Context#context.cowreq),
        cowenv = prune_envdata(Context#context.cowenv),
        render_state = undefined
    }.

prune_reqdata(undefined) ->
    undefined;
prune_reqdata(Req) ->
    %% @todo: prune this better, also used by the websocket connection.
    Req#{
        bindings => #{},
        headers => #{},
        path => <<>>,
        qs => <<>>,
        pid => undefined,
        streamid => undefined
    }.

prune_envdata(undefined) ->
    undefined;
prune_envdata(Env) ->
    %% @todo: prune this better, also used by the websocket connection.
    Env#{
        cowmachine_cookies => [],
        cowmachine_resp_body => <<>>
    }.


%% @doc Check if the URL is an URL of the local site
-spec is_site_url( undefined | string() | binary(), z:context() ) -> boolean().
is_site_url(undefined, _Context) -> false;
is_site_url(<<"//", _/binary>> = Url, Context) -> is_site_url_1(Url, Context);
is_site_url(<<"/", _/binary>>, _Context) -> true;
is_site_url(<<"#", _/binary>>, _Context) -> true;
is_site_url([ $/, $/ | _ ] = Url, Context) -> is_site_url_1(Url, Context);
is_site_url([$/ | _], _Context) -> true;
is_site_url([$# | _], _Context) -> true;
is_site_url(Url, Context) -> is_site_url_1(Url, Context).

is_site_url_1(Url, Context) ->
    case z_sites_dispatcher:get_site_for_url(Url) of
        {ok, Site} ->
            Site =:= site(Context);
        undefined ->
            false
    end.

%% @doc Ensure that an URL is an URL to the current site. If not then return
%% the URL of the homepage. If the URL is not a fragment then the returned URL
%% is always sanitized and absolute.
-spec site_url(Url, Context) -> SiteUrl when
    Url :: undefined | string() | binary(),
    Context :: z:context(),
    SiteUrl :: binary().
site_url(undefined, Context) ->
    abs_url(<<"/">>, Context);
site_url("#" ++ _ = Frag, _Context) ->
     z_sanitize:uri(z_convert:to_binary(Frag));
site_url(<<"#", _/binary>> = Frag, _Context) ->
     z_sanitize:uri(Frag);
site_url(Url, Context) ->
    Url1 = z_sanitize:uri(Url),
    case is_site_url(Url1, Context) of
        true ->
            abs_url(Url1, Context);
        false ->
            abs_url(<<"/">>, Context)
    end.

%% @doc Make the url an absolute url by prepending the hostname.
-spec abs_url(undefined | iodata(), z:context()) -> binary().
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
            Hostname = case not is_hostname_redirect_configured(Context) andalso is_request(Context) of
                           true -> cowmachine_req:host(Context);
                           false -> hostname(Context)
                       end,
            case z_config:get(ssl_port) of
                443 -> <<"https://", Hostname/binary, Url/binary>>;
                Port -> <<"https://", Hostname/binary, $:, (integer_to_binary(Port))/binary, Url/binary>>
            end;
        AbsUrl ->
            AbsUrl
    end;
abs_url(undefined, Context) ->
    abs_url(<<>>, Context);
abs_url(Url, Context) ->
    case has_url_protocol(Url) of
        true -> Url;
        false -> abs_url(<<$/, Url/binary>>, Context)
    end.

has_url_protocol(<<"http:", _/binary>>) -> true;
has_url_protocol(<<"https:", _/binary>>) -> true;
has_url_protocol(<<"ws:", _/binary>>) -> true;
has_url_protocol(<<"wss:", _/binary>>) -> true;
has_url_protocol(<<"ftp:", _/binary>>) -> true;
has_url_protocol(<<"email:", _/binary>>) -> true;
has_url_protocol(<<"file:", _/binary>>) -> true;
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
    Context = new(Site, Language),
    case UserId of
        undefined -> Context;
        _ -> z_acl:logon(UserId, Context)
    end;
depickle({pickled_context, Site, UserId, Language, Tz, _VisitorId}) ->
    Context = new(Site, Language, Tz),
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
            PathArgs = [ {z_convert:to_binary(T), V} || {T,V} <- maps:to_list( PathInfo ) ],
            QPropsUrl = Props#{ q => PathArgs++Query },
            ContextQs = Context#context{ props = QPropsUrl },
            % Auth user via cookie - set language
            ContextReq = z_notifier:foldl(#request_context{ phase = init }, ContextQs, ContextQs),
            ContextReq2 = z_notifier:foldl(#request_context{ phase = refresh }, ContextReq, ContextReq),
            % Parse the POST body (if any)
            {Body, ContextParsed} = parse_post_body(ContextReq2),
            QPropsAll = (ContextParsed#context.props)#{ q => PathArgs++Body++Query },
            ContextParsed#context{ props = QPropsAll }
    end.


%% @doc Return the cowmachine request data of the context
-spec get_reqdata(z:context()) -> cowboy_req:req() | undefined.
get_reqdata(Context) ->
    Context#context.cowreq.

%% @doc Set the cowmachine request data of the context
-spec set_reqdata(cowboy_req:req() | undefined, z:context()) -> z:context().
set_reqdata(Req, Context) when is_map(Req); Req =:= undefined ->
    Context#context{ cowreq = Req }.

%% @doc Return the cowmachine request data of the context
-spec get_envdata(z:context()) -> cowboy_middleware:env() | undefined.
get_envdata(Context) ->
    Context#context.cowenv.

%% @doc Set the cowmachine request data of the context
-spec set_envdata(cowboy_middleware:env() | undefined, z:context()) -> z:context().
set_envdata(Env, Context) when is_map(Env); Env =:= undefined ->
    Context#context{ cowenv = Env }.

%% @doc Set the cowmachine request data of the context
-spec init_cowdata(cowboy_req:req(), cowboy_middleware:env(), z:context()) -> z:context().
init_cowdata(Req, Env, Context) when is_map(Req); Req =:= undefined ->
    Context#context{
        cowreq = Req,
        cowenv = Env
    }.

%% @doc Get the resource module handling the request.
-spec get_controller_module(z:context()) -> atom() | undefined.
get_controller_module(Context) ->
    Context#context.controller_module.

-spec set_controller_module(Module::atom(), z:context()) -> z:context().
set_controller_module(Module, Context) ->
    Context#context{ controller_module = Module }.

-spec get_render_state( z:context() ) -> z_render:render_state() | undefined.
get_render_state(#context{ render_state = RS }) ->
    RS.

-spec set_render_state( z_render:render_state() | undefined, z:context() ) -> z:context().
set_render_state(RS, Context) ->
    Context#context{ render_state = RS }.


%% @doc Replace the value of a request parameter argument
%%      Always filter the #upload{} arguments to prevent upload of non-temp files.
-spec set_q(binary()|string()|atom(), z:qvalue(), z:context()) -> z:context().
set_q(Key, #upload{ tmpfile = TmpFile } = Upload, Context) when TmpFile =/= undefined ->
    set_q(Key, Upload#upload{ tmpfile = undefined }, Context);
set_q(Key, Value, Context) when is_binary(Key) ->
    Qs = get_q_all(Context),
    Qs1 = lists:keydelete(Key, 1, Qs),
    z_context:set('q', [{Key,Value}|Qs1], Context);
set_q(Key, Value, Context) ->
    set_q(z_convert:to_binary(Key), Value, Context).

%% @doc Set the value of multiple request parameter arguments
-spec set_q(Qs, Context) -> NewContext when
    Qs :: [ {Key, z:qvalue()} | Key ]
        | map()
        | [ list() ],
    Key :: binary() | string() | atom(),
    Context :: z:context(),
    NewContext :: z:context().
set_q(KVs, Context) when is_map(KVs) ->
    maps:fold(
        fun(K, V, Ctx) ->
            set_q(K, V, Ctx)
        end,
        Context,
        KVs);
set_q(KVs, Context) when is_list(KVs) ->
    lists:foldl(
        fun
            ({K, V}, Ctx) ->
                set_q(K, V, Ctx);
            ([K, V], Ctx) ->
                set_q(K, V, Ctx);
            (K, Ctx) ->
                set_q(K, true, Ctx)
        end,
        Context,
        KVs).

%% @doc Add the value of a request parameter argument. This allows for multiple
%%      arguments with the same name. The new argument is pre-pended to the existing
%%      arguments.
%%      Always filter the #upload{} arguments to prevent upload of non-temp files.
-spec add_q(Key, Value, Context) -> NewContext when
    Key :: binary()|atom(),
    Value :: z:qvalue(),
    Context :: z:context(),
    NewContext :: z:context().
add_q(Key, #upload{ tmpfile = TmpFile } = Upload, Context) when TmpFile =/= undefined ->
    add_q(Key, Upload#upload{ tmpfile = undefined }, Context);
add_q(Key, Value, Context) when is_binary(Key) ->
    Qs = get_q_all(Context),
    z_context:set('q', [{Key,Value}|Qs], Context);
add_q(Key, Value, Context) when is_atom(Key) ->
    add_q(z_convert:to_binary(Key), Value, Context);
add_q(_, _Value, Context) ->
    Context.

%% @doc Add the value of multiple request parameter arguments. This allows for the
%% insertion of multiple keys with the same value. The new arguments are prepended
%% before the existing arguments.
-spec add_q(KeyValues, Context) -> NewContext when
    KeyValues :: list() | map(),
    Context :: z:context(),
    NewContext :: z:context().
add_q(KVs, Context) when is_list(KVs) ->
    lists:foldr(
        fun
            ({K, V}, Ctx) ->
                add_q(K, V, Ctx);
            ([K, V], Ctx) ->
                add_q(K, V, Ctx);
            (K, Ctx) ->
                add_q(K, true, Ctx)
        end,
        Context,
        KVs);
add_q(KVs, Context) when is_map(KVs) ->
    maps:fold(
        fun(K, V, Ctx) ->
            add_q(K, V, Ctx)
        end,
        Context,
        KVs).

%% @doc Delete all values of one or more request parameter arguments.
-spec delete_q(Keys, Context) -> NewContext when
    Keys :: Key | [ Key ],
    Key :: binary() | atom() | string(),
    Context :: z:context(),
    NewContext :: z:context().
delete_q([C|_] = Key, Context) when is_integer(C) ->
    delete_q(list_to_binary(Key), Context);
delete_q(Key, Context) when is_binary(Key); is_atom(Key) ->
    Key1 = z_convert:to_binary(Key),
    Qs = get_q_all(Context),
    z_context:set('q', proplists:delete(Key1, Qs), Context);
delete_q(Keys, Context) when is_list(Keys) ->
    Qs = get_q_all(Context),
    Qs1 = lists:foldl(
        fun(K, Acc) ->
            K1 = z_convert:to_binary(K),
            proplists:delete(K1, Acc)
        end,
        Qs,
        Keys),
    z_context:set('q', Qs1, Context).

%% @doc Get a request parameter, either from the query string or the post body.  Post body has precedence over the query string.
%%      Note that this can also be populated from a JSON MQTT call, and as such contain arbitrary data.
-spec get_q(string()|atom()|binary()|list(), z:context()) -> undefined | z:qvalue().
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
-spec get_q(binary()|string()|atom(), z:context(), term()) -> z:qvalue().
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


%% @doc Fetch all arguments starting with a 'q'. This is used for queries.
-spec get_qargs( z:context() ) -> list( {binary(), z:qvalue()} ).
get_qargs(Context) ->
    Qs = get_q_all(Context),
    lists:foldr(fun
                    ({<<"q", _/binary>>, _Value} = A, Acc) ->
                        [ A | Acc ];
                    (_, Acc) ->
                        Acc
                end,
                [],
                Qs).

%% @doc Get all parameters.
-spec get_q_all(z:context()) -> list({binary(), z:qvalue()}).
get_q_all(#context{ props = Props }) ->
    case maps:find(q, Props) of
        {ok, Qs} -> Qs;
        error -> []
    end.

%% @doc Replace all parameters.
-spec set_q_all(list({binary(), z:qvalue()}), z:context()) -> z:context().
set_q_all(QArgs, #context{ props = Props } = Context) when is_list(QArgs) ->
    Context#context{ props = Props#{ q => QArgs }}.

%% @doc Get the all the parameters with the same name, returns the empty list when non found.
-spec get_q_all(string()|atom()|binary(), z:context()) -> list( z:qvalue() ).
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
-spec get_q_all_noz(z:context()) -> list({binary(), z:qvalue()}).
get_q_all_noz(Context) ->
    lists:filter(fun({X,_}) -> not is_zotonic_arg(X) end, get_q_all(Context)).

%% @doc Get all query/post args, transformed into a map.
-spec get_q_map(z:context()) -> map().
get_q_map(Context) ->
    Qs = get_q_all(Context),
    {ok, Props} = z_props:from_qs(Qs),
    maps:remove(<<"*">>, Props).

%% @doc Get all query/post args, transformed into a map.
%% Removes Zotonic vars and the dispatcher '*' variable.
-spec get_q_map_noz(z:context()) -> map().
get_q_map_noz(Context) ->
    Qs = get_q_all_noz(Context),
    {ok, Props} = z_props:from_qs(Qs),
    maps:remove(<<"*">>, Props).

%% @doc Filter all Zotonic and dispatcher vars from a map.
-spec without_zotonic_args(map()) -> map().
without_zotonic_args(Map) ->
    maps:fold(
        fun(K, V, Acc) ->
            case is_zotonic_arg(K) of
                true -> Acc;
                false -> Acc#{ K => V }
            end
        end,
        #{},
        Map).


% Known Zotonic rguments:
%
% - postback
% - triggervalue
% - zotonic_host
% - zotonic_site
% - zotonic_dispatch
% - zotonic_dispatch_path
% - zotonic_dispatch_path_rewrite
% - zotonic_ticket
% - zotonic_http_*
% - zotonic_topic_*
% - z_submitter
% - z_postback
% - z_trigger_id
% - z_target_id
% - z_delegate
% - z_message
% - z_transport
% - z_sid
% - z_pageid
% - z_v
% - z_msg
% - z_comet
% - z_postback_data

-spec is_zotonic_arg(binary()) -> boolean().
is_zotonic_arg(<<"postback">>) -> true;
is_zotonic_arg(<<"triggervalue">>) -> true;
is_zotonic_arg(<<"zotonic_", _/binary>>) -> true;
is_zotonic_arg(<<"z_", _/binary>>) -> true;
is_zotonic_arg(_) -> false.


%% @doc Fetch a query parameter and perform the validation connected to the parameter. An exception {not_validated, Key}
%%      is thrown when there was no validator, when the validator is invalid or when the validation failed.
-spec get_q_validated(string()|atom()|binary(), z:context()) -> z:qvalue() | undefined.
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
%% Set logger metadata for the current process
%% ------------------------------------------------------------------------------------

%% @doc Set the logger metadata for the current site or context.
-spec logger_md( z:context() | atom() ) -> ok.
logger_md(Site) when is_atom(Site) ->
    logger_md(z_context:new(Site));
logger_md(Context) ->
    logger_md(#{}, Context).

%% @doc Set the logger metadata, add the current site or context
-spec logger_md( map() | list(), z:context() ) -> ok.
logger_md(MetaData, #context{} = Context) when is_list(MetaData) ->
    logger_md(maps:from_list(MetaData), Context);
logger_md(MetaData, #context{} = Context) when is_map(MetaData) ->
    SessionId = case session_id(Context) of
        {ok, Sid} -> Sid;
        {error, _} -> undefined
    end,
    logger:set_process_metadata(MetaData#{
        site => site(Context),
        environment => m_site:environment(Context),
        user_id => Context#context.user_id,
        language => language(Context),
        timezone => tz(Context),
        controller => Context#context.controller_module,
        dispatch => get(zotonic_dispatch, Context),
        method => m_req:get(method, Context),
        user_agent => m_req:get(user_agent, Context),
        path => m_req:get(raw_path, Context),
        remote_ip => m_req:get(peer, Context),
        is_ssl => m_req:get(is_ssl, Context),
        session_id => SessionId,
        correlation_id => m_req:get(req_id, Context),
        node => node()
    }).


%% @doc Ensure that the logger metadata for this site and process is set.
-spec ensure_logger_md( z:context() | atom() ) -> ok.
ensure_logger_md(Context) ->
    case logger:get_process_metadata() of
        #{ site := _ } -> ok;
        _ -> z_context:logger_md(Context)
    end.

%% ------------------------------------------------------------------------------------
%% Set/get/modify state properties
%% ------------------------------------------------------------------------------------


%% @doc Return the current client id (if any)
-spec client_id( z:context() ) -> {ok, binary()} | {error, no_client}.
client_id(#context{ client_id = ClientId }) when is_binary(ClientId) ->
    {ok, ClientId};
client_id(#context{}) ->
    {error, no_client}.

%% @doc Return the current client bridge topic (if any)
-spec client_topic( z:context() ) -> {ok, mqtt_sessions:topic()} | {error, no_client}.
client_topic(#context{ client_topic = ClientTopic, client_id = ClientId }) when is_binary(ClientId), is_binary(ClientTopic) ->
    {ok, mqtt_packet_map_topic:normalize_topic(ClientTopic)};
client_topic(#context{ client_topic = ClientTopic, client_id = ClientId }) when is_binary(ClientId), is_list(ClientTopic) ->
    {ok, ClientTopic};
client_topic(#context{}) ->
    {error, no_client}.

%% @doc Merge a context with client information into a request context.
%% This is used to merge a client context obtained from a MQTT ticket into
%% the contex of an out of band MQTT post.
%%
%% Access control, timezone, language and client information is copied over
%% from the client context to the request context.
-spec set_client_context( ClientContext::z:context(), ReqContext::z:context() ) -> z:context().
set_client_context(ClientContext, RequestContext) ->
    Ctx = RequestContext#context{
        client_id = ClientContext#context.client_id,
        client_topic = ClientContext#context.client_topic,
        routing_id = ClientContext#context.routing_id,
        user_id = ClientContext#context.user_id,
        acl = ClientContext#context.acl,
        acl_is_read_only = ClientContext#context.acl_is_read_only,
        tz = ClientContext#context.tz,
        language = ClientContext#context.language
    },
    case maps:find(auth_options, ClientContext#context.props) of
        {ok, AuthOptions} ->
            z_context:set(auth_options, AuthOptions, Ctx);
        error ->
            Ctx
    end.

%% @doc Return the unique random session id for the current client auth.
%%      This session_id is re-assigned when the authentication of a client
%%      changes.
-spec session_id( z:context() ) -> {ok, binary()} | {error, no_session}.
session_id(Context) ->
    case get(session_id, Context) of
        Sid when is_binary(Sid), Sid =/= <<>> ->
            {ok, Sid};
        _ ->
            {error, no_session}
    end.

%% @doc Set the cotonic session id. Mostly used when on a request with
%%      a cotonic session id in the cookie.
-spec set_session_id( binary(), z:context() ) -> z:context().
set_session_id(Sid, Context) ->
    set(session_id, Sid, Context).

%% @doc Set the value of the context variable Key to Value
-spec set( atom(), term(), z:context() ) -> z:context().
set(Key, Value, #context{ props = Props } = Context) ->
    Context#context{ props = Props#{ Key => Value } }.

%% @doc Set the value of the context variables to all {Key, Value} properties.
-spec set( proplists:proplist(), z:context() ) -> z:context().
set(PropList, Context) when is_list(PropList) ->
    NewProps = lists:foldl(
        fun
            ({Key,Value}, Props) -> Props#{ Key => Value };
            (Key, Props) -> Props#{ Key => true }
        end,
        Context#context.props,
        PropList),
    Context#context{ props = NewProps }.


%% @doc Fetch the value of the context variable Key, return undefined when Key is not found.
-spec get( atom(), z:context() ) -> term() | undefined.
get(Key, Context) ->
    get(Key, Context, undefined).

%% @doc Fetch the value of the context variable Key, return Default when Key is not found.
-spec get( atom(), z:context(), term() ) -> term().
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
    case is_request(Context) of
        true ->
            case maps:find(Key, cowmachine_req:path_info(Context)) of
                {ok, Value} -> Value;
                error -> Default
            end;
        false ->
            Default
    end.


%% @doc Return a proplist with all context variables.
-spec get_all(Context) -> ContextVars when
    Context :: z:context(),
    ContextVars :: list( {Key, Value} ),
    Key :: term(),
    Value :: term().
get_all(Context) ->
    maps:to_list(Context#context.props).


%% @doc Return the primary selected language of the Context
-spec language(Context) -> Language when
    Context :: z:context(),
    Language :: z_language:language_code().
language(#context{ language = [] } = Context) ->
    z_language:default_language(Context);
language(#context{ language = [Lang|_] }) ->
    Lang.

%% @doc Return the language preference list.
-spec languages(Context) -> Languages when
    Context :: z:context(),
    Languages :: [ z_language:language_code() ].
languages(#context{ language = [] } = Context) ->
    z_language:enabled_languages(Context);
languages(#context{ language = Languages }) when is_list(Languages) ->
    Languages.

%% @doc Set the language of the context, either an atom (language) or a list (language and fallback languages)
-spec set_language(Language, Context) -> LangContext when
    Language :: undefined | z_language:language_code() | binary() | list( z_language:language() ),
    Context :: z:context(),
    LangContext :: z:context().
set_language([], Context) ->
    Context;
set_language(undefined, Context) ->
    set_language('x-default', Context);
set_language('x-default', Context) ->
    Context#context{language=['x-default'|lists:delete('x-default', languages(Context))]};
set_language(Lang, Context) when is_atom(Lang) ->
    Context#context{language=[Lang|lists:delete(Lang, languages(Context))]};
set_language(Langs, Context) when is_list(Langs) ->
    Langs1 = lists:filtermap(
        fun(Lang) ->
            case z_language:to_language_atom(Lang) of
                {ok, Code} -> {true, Code};
                {error, _} -> false
            end
        end, Langs),
    Context#context{language= Langs1 ++ (Context#context.language -- Langs1)};
set_language(Lang, Context) when is_binary(Lang) ->
    case z_language:to_language_atom(Lang) of
        {ok, Code} -> set_language(Code, Context);
        {error, _} -> Context
    end.

%% @doc Return the selected timezone of the Context; defaults to the site's timezone
-spec tz(Context) -> Timezone when
    Context :: z:context(),
    Timezone :: binary().
tz(#context{ tz = TZ }) when TZ =/= undefined; TZ =/= <<>> ->
    TZ;
tz(Context) ->
    tz_config(Context).

%% @doc Return the site's configured timezone.
-spec tz_config(Context) -> Timezone when
    Context :: z:context(),
    Timezone :: binary().
tz_config(Context) ->
    case m_config:get_value(mod_l10n, timezone, Context) of
        None when None =:= undefined; None =:= <<>> ->
            z_config:get(timezone);
        TZ ->
            TZ
    end.

%% @doc Set the timezone of the context.
-spec set_tz(MaybeTimezone, Context) -> TzContext when
    MaybeTimezone :: string() | binary() | boolean() | 1 | 0 | term(),
    Context :: z:context(),
    TzContext :: z:context().
set_tz(undefined, Context) ->
    Context;
set_tz(<<>>, Context) ->
    Context;
set_tz("", Context) ->
    Context;
set_tz(true, Context) ->
    Context#context{ tz = <<"UTC">> };
set_tz(false, Context) ->
    Context;
set_tz(1, Context) ->
    Context#context{ tz = <<"UTC">> };
set_tz(0, Context) ->
    Context;
set_tz(Tz, Context) when is_list(Tz) ->
    set_tz(unicode:characters_to_binary(Tz, utf8), Context);
set_tz(Tz, Context) when is_binary(Tz) ->
    case m_l10n:is_timezone(Tz) of
        true ->
            Context#context{ tz = Tz };
        false ->
            ?LOG_INFO(#{
                text => <<"Ignoring unknown timezone">>,
                in => zotonic_core,
                tz => Tz
            }),
            Context
    end;
set_tz(Tz, Context) ->
    ?LOG_ERROR(#{
        text => <<"Ignoring unknown timezone">>,
        in => zotonic_core,
        tz => Tz
    }),
    Context.


%% @doc Set the Content-Security-Policy nonce for the request.
-spec set_csp_nonce( z:context() ) -> z:context().
set_csp_nonce(Context) ->
    case get(csp_nonce, Context) of
        undefined ->
            Nonce = z_ids:id(),
            set(csp_nonce, Nonce, Context);
        Nonce when is_binary(Nonce) ->
            Context
    end.

%% @doc Return the Content-Security-Policy nonce for the request.
-spec csp_nonce( z:context() ) -> binary().
csp_nonce(Context) ->
    case get(csp_nonce, Context) of
        undefined ->
            ?LOG_WARNING(#{
                text => <<"csp_nonce requested but not set">>,
                in => zotonic_core
            }),
            <<>>;
        Nonce when is_binary(Nonce) ->
            Nonce
    end.


%% @doc Set a response header for the request in the context.
-spec set_resp_header(binary(), binary(), z:context()) -> z:context().
set_resp_header(<<"vary">>, <<"*">>, #context{cowreq=Req} = Context) when is_map(Req) ->
    cowmachine_req:set_resp_header(<<"vary">>, <<"*">>, Context);
set_resp_header(<<"vary">>, Value, #context{cowreq=Req} = Context) when is_map(Req) ->
    case cowmachine_req:get_resp_header(<<"vary">>, Context) of
        undefined ->
            cowmachine_req:set_resp_header(<<"vary">>, Value, Context);
        <<"*">> ->
            Context;
        Curr ->
            Value1 = <<Curr/binary, ", ", Value/binary>>,
            cowmachine_req:set_resp_header(<<"vary">>, Value1, Context)
    end;
set_resp_header(Header, Value, #context{cowreq=Req} = Context) when is_map(Req) ->
    cowmachine_req:set_resp_header(Header, Value, Context).

%% @doc Set multiple response headers for the request in the context.
-spec set_resp_headers([ {binary(), binary()} ], z:context()) -> z:context().
set_resp_headers(Headers, #context{cowreq=Req} = Context) when is_map(Req) ->
    cowmachine_req:set_resp_headers(Headers, Context).

%% @doc Get a response header
-spec get_resp_header(binary(), z:context()) -> binary() | undefined.
get_resp_header(Header, #context{cowreq=Req} = Context) when is_map(Req) ->
    cowmachine_req:get_resp_header(Header, Context).

%% @doc Get a request header. The header MUST be in lower case.
-spec get_req_header(binary(), z:context()) -> binary() | undefined.
get_req_header(Header, #context{cowreq=Req} = Context) when is_map(Req) ->
    cowmachine_req:get_req_header(Header, Context).


%% @doc Return the request path
-spec get_req_path(z:context()) -> binary().
get_req_path(#context{cowreq=Req} = Context) when is_map(Req) ->
    cowmachine_req:raw_path(Context).

%% @doc Add metrics data to the Cowboy request, will be added to the metrics notifications.
-spec set_req_metrics( map(), z:context() ) -> ok.
set_req_metrics(Metrics, #context{ cowreq = Req }) when is_map(Req), is_map(Metrics) ->
    cowboy_req:cast({set_options, #{ metrics_user_data => Metrics }}, Req),
    ok;
set_req_metrics(_Metrics, _Context) ->
    ok.

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
            {Form, ContextRcv} = z_multipart_parse:recv_parse(Context),
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
set_nocache_headers(Context = #context{cowreq=Req}) when is_map(Req) ->
    cowmachine_req:set_resp_headers([
            {<<"cache-control">>, <<"no-store, no-cache, must-revalidate, private, post-check=0, pre-check=0">>},
            {<<"expires">>, <<"Wed, 10 Dec 2008 14:30:00 GMT">>},
            {<<"p3p">>, <<"CP=\"NOI ADM DEV PSAi COM NAV OUR OTRo STP IND DEM\"">>},
            {<<"pragma">>, <<"nocache">>}
        ],
        Context).

%% @doc Set security related headers. This can be modified by observing the
%%      'security_headers' notification.
-spec set_security_headers( z:context() ) -> z:context().
set_security_headers(Context) ->
    Default = [
        % {<<"content-security-policy">>, <<"script-src 'self' 'nonce-'">>}
        {<<"x-content-type-options">>, <<"nosniff">>},
        {<<"x-permitted-cross-domain-policies">>, <<"none">>},
        {<<"referrer-policy">>, <<"origin-when-cross-origin">>}
    ],
    Default1 = case z_context:get(allow_frame, Context, false) of
        true -> Default;
        false -> [ {<<"x-frame-options">>, <<"sameorigin">>} | Default ]
    end,
    HSTSHeaders = case hsts_header(Context) of
        {_,_} = H -> [ H | Default1 ];
        _ -> Default1
    end,
    SecurityHeaders = case z_notifier:first(#security_headers{ headers = HSTSHeaders }, Context) of
        undefined -> HSTSHeaders;
        Custom -> Custom
    end,
    SecurityHeaders1 = case proplists:get_value(<<"content-security-policy">>, SecurityHeaders) of
        undefined ->
            SecurityHeaders;
        CSPHdr ->
            Nonce = csp_nonce(Context),
            CSPHdr1 = binary:replace(
                        CSPHdr,
                        <<"'nonce-'">>,
                        <<"'nonce-", Nonce/binary, $'>>),
            [
                {<<"content-security-policy">>, CSPHdr1}
                | proplists:delete(<<"content-security-policy">>, SecurityHeaders)
            ]
    end,
    cowmachine_req:set_resp_headers(SecurityHeaders1, Context).

%% @doc Create a hsts header based on the current settings. The result is cached
%%      for quick access.
-spec hsts_header( z:context() ) -> undefined | {_, _}.
hsts_header(Context) ->
    case z_convert:to_bool(m_config:get_value(site, hsts, false, Context)) of
        true ->
            F = fun() ->
                MaxAge = z_convert:to_integer(m_config:get_value(site, hsts_maxage, ?HSTS_MAXAGE, Context)),
                IncludeSubdomains = z_convert:to_bool(m_config:get_value(site, hsts_include_subdomains, false, Context)),
                Preload = z_convert:to_bool(m_config:get_value(site, preload, false, Context)),
                Options = case {IncludeSubdomains, Preload} of
                    {true, true} -> <<"; includeSubDomains; preload">>;
                    {true, _} -> <<"; includeSubDomains">>;
                    {_, true} -> <<"; preload">>;
                    {_, _} -> <<"">>
                end,
                HSTS = iolist_to_binary([ <<"max-age=">>, z_convert:to_binary(MaxAge), Options ]),
                {<<"strict-transport-security">>, HSTS}
            end,
            z_depcache:memo(F, hsts_header, ?DAY, [config], Context);
        false ->
            undefined
    end.


%% @doc Set Cross-Origin Resource Sharing (CORS) headers. The caller must
%%      specify default headers to be used in case there are no observers for
%%      the #cors_headers{} notification.
-spec set_cors_headers([{binary(), binary()}], z:context()) -> z:context().
set_cors_headers(Default, Context) ->
    CorsHeaders = case z_notifier:first(#cors_headers{ headers = Default }, Context) of
        undefined -> Default;
        Custom -> Custom
    end,
    set_resp_headers(CorsHeaders, Context).

%% @doc Set the noindex header if the config is set, or the webmachine resource opt is set.
-spec set_noindex_header(z:context()) -> z:context().
set_noindex_header(Context) ->
    set_noindex_header(false, Context).

%% @doc Set the noindex header if the config is set, the webmachine resource opt is set or Force is set.
-spec set_noindex_header(Force::term(), z:context()) -> z:context().
set_noindex_header(Force, Context) ->
    case m_config:get_boolean(seo, noindex, Context)
         orelse get(seo_noindex, Context, false)
         orelse z_convert:to_bool(Force)
    of
       true ->
            set_resp_header(<<"x-robots-tag">>, <<"noindex,nofollow">>, Context);
       _ ->
            Context
    end.

%% @doc Set resource specific headers. Examples are the non-informational resource uri and WebSub headers.
-spec set_resource_headers( m_rsc:resource_id() | undefined, z:context() ) -> z:context().
set_resource_headers(Id, Context) when is_integer(Id) ->
    Uri = z_dispatcher:url_for(id, [ {id, Id} ], set_language('x-default', Context)),
    Hs = [ {<<"x-resource-uri">>, abs_url(Uri, Context)} ],
    Hs1 = z_notifier:foldl(#resource_headers{ id = Id }, Hs, Context),
    set_resp_headers(Hs1, Context);
set_resource_headers(_Id, Context) ->
    Hs = z_notifier:foldl(#resource_headers{ id = undefined }, [], Context),
    set_resp_headers(Hs, Context).


%% @doc Set a cookie value with default options.
-spec set_cookie(binary(), binary(), z:context()) -> z:context().
set_cookie(Key, Value, Context) ->
    set_cookie(Key, Value, [], Context).

%% @doc Set a cookie value with cookie options.
-spec set_cookie(binary(), binary(), list(), z:context()) -> z:context().
set_cookie(_Key, _Value, _Options, #context{ cowreq = undefined } = Context) ->
    Context;
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
get_cookie(_Key, #context{cowreq = undefined}) ->
    undefined;
get_cookie(Key, #context{cowreq=Req} = Context) when is_map(Req) ->
    cowmachine_req:get_cookie_value(Key, Context).

%% @doc Read all cookie values with a certain key from the current request.
-spec get_cookies(binary(), z:context()) -> [ binary() ].
get_cookies(_Key, #context{cowreq = undefined}) ->
    [];
get_cookies(Key, #context{cowreq=Req} = Context) when is_map(Req), is_binary(Key) ->
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
