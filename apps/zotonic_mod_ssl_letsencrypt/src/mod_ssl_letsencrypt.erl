%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell, Maas-Maarten Zeeman
%%
%% @doc Certificate handling for Let's Encrypt

%% Copyright 2016 Marc Worrell, Maas-Maarten Zeeman
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

-module(mod_ssl_letsencrypt).

-mod_title("SSL - Let's Encrypt").
-mod_description("Use SSL Certificate from Let's Encrypt.").
-mod_provides([]).
-mod_depends([cron]).
-mod_prio(200).

-behaviour(gen_server).

-author('Marc Worrell <marc@worrell.nl>').

-export([
    observe_ssl_options/2,
    observe_tick_24h/2,
    event/2,

    is_self_ping/2,
    get_self_ping/1,
    get_challenge/1,

    status/1,

    load_cert/1
]).

-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(SNI_CACHE_TIME, 60).
-define(KEY_BITS, 2048).
-define(CA_CERT_URL, "https://letsencrypt.org/certs/lets-encrypt-x3-cross-signed.pem").

-ifdef(TEST).
-define(ACME_SRV_OPTS, [staging]).
-else.
-define(ACME_SRV_OPTS, []).
-endif.


-record(state, {
    site :: atom(),
    self_ping :: binary() | undefined,
    % State information when requesting a new cert
    request_letsencrypt_pid = undefined :: undefined | pid(),
    request_monitor = undefined :: undefined | reference(),
    request_hostname = undefined :: binary() | undefined,
    request_san = [] :: list(binary()),
    request_start = undefined :: undefined | calendar:datetime(),
    request_status = none :: none | requesting | ok | error,
    % Information about the current certificate
    cert_is_valid = false :: boolean(),
    cert_hostname = undefined :: binary() | undefined,
    cert_san = [] :: list( binary() ),
    cert_valid_till = undefined :: undefined | calendar:datetime()
}).


%% @doc Return the certificates of this site.
observe_ssl_options(#ssl_options{server_name=_NormalizedHostnameBin}, Context) ->
    z_depcache:memo(
        fun() ->
            case status(Context) of
                {ok, Status} ->
                    case proplists:get_value(cert_is_valid, Status) of
                        true -> ssl_options(Context);
                        false -> undefined
                    end;
                {error, _} ->
                    undefined
            end
        end,
        sni_ssl_letsencrypt,
        ?SNI_CACHE_TIME,
        Context).

%% @doc Period tick, used to check for cert upgrade
observe_tick_24h(tick_24h, Context) ->
    load_cert(Context),
    gen_server:cast(z_utils:name_for_site(?MODULE, Context), renewal_check).

%% @doc Handle UI events
%% @todo ACL check
event(#submit{message = {request_cert, Args}}, Context) ->
    case z_acl:is_admin_editable(Context) of
        true ->
            {hostname, Hostname} = proplists:lookup(hostname, Args),
            {wrapper, Wrapper} = proplists:lookup(wrapper, Args),
            SANs = z_context:get_q_all(<<"san">>, Context),
            SANs1 = [ San || San <- SANs, San /= <<>> ],
            case gen_server:call(z_utils:name_for_site(?MODULE, Context), {cert_request, Hostname, SANs1}) of
                ok ->
                    z_render:update(Wrapper,
                                    #render{
                                        template="_admin_ssl_letsencrypt_running.tpl",
                                        vars=[
                                            {hostname, Hostname},
                                            {san, SANs1}
                                        ]},
                                    Context);
                {error, Reason} ->
                    ?LOG_ERROR(#{
                        text => <<"Could not start Letsencrypt cert request">>,
                        in => zotonic_mod_ssl_letsencrypt,
                        result => error,
                        reason => Reason,
                        hostname => Hostname,
                        san => SANs1
                    }),
                    z_render:wire({alert, [
                                    {title, ?__(<<"SSL Let’s Encrypt Certificate"/utf8>>, Context)},
                                    {text, ?__("Could not start fetching the SSL certificate. Try again later.", Context)},
                                    {button, ?__("Cancel", Context)}
                                ]},
                                Context)
            end;
        false ->
            z_render:wire({alert, [
                            {title, ?__(<<"SSL Let’s Encrypt Certificate"/utf8>>, Context)},
                            {text, ?__("You need to be an administrator to request certificates.", Context)},
                            {button, ?__("Cancel", Context)}
                        ]},
                        Context)
    end;
event(_Event, Context) ->
    Context.


%% @doc Generate a new self-ping value
-spec get_self_ping(#context{}) -> {ok, binary()}.
get_self_ping(Context) ->
    gen_server:call(z_utils:name_for_site(?MODULE, Context), get_self_ping).

%% @doc Check if the returned ping is the generated ping
-spec is_self_ping(binary()|string(), #context{}) -> boolean().
is_self_ping(Ping, Context) ->
    gen_server:call(z_utils:name_for_site(?MODULE, Context), {is_self_ping, Ping}).

%% @doc Fetch the challenge requested by the ACME handshake
-spec get_challenge(#context{}) -> {ok, map()}.
get_challenge(Context) ->
    gen_server:call(z_utils:name_for_site(?MODULE, Context), get_challenge).

%% @doc Check if the returned ping is the generated ping
-spec status(#context{}) -> {ok, list()} | {error, term()}.
status(Context) ->
    gen_server:call(z_utils:name_for_site(?MODULE, Context), status).

%% @doc Load the current certificate metadata
-spec load_cert( z:context() ) -> ok.
load_cert(Context) ->
    gen_server:cast(z_utils:name_for_site(?MODULE, Context), load_cert).


%%====================================================================
%% API
%%====================================================================
%% @spec start_link(Args) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    gen_server:start_link({local, z_utils:name_for_site(?MODULE, Context)}, ?MODULE, Args, []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    Site = z_context:site(Context),
    logger:set_process_metadata(#{
        site => Site,
        module => ?MODULE
    }),
    gen_server:cast(self(), load_cert),
    {ok, #state{site=Site, self_ping = undefined }}.

handle_call(get_self_ping, _From, State) ->
    Ping = z_ids:id(),
    {reply, {ok, Ping}, State#state{self_ping = Ping}};
handle_call({is_self_ping, SelfPing}, _From, #state{self_ping = Ping} = State) ->
    {reply, z_convert:to_binary(SelfPing) =:= Ping, State};
handle_call({cert_request, _Hostname, _SANs}, _From, #state{request_letsencrypt_pid = Pid} = State) when is_pid(Pid) ->
    ?LOG_ERROR("Letsencrypt cert request whilst another request is running"),
    {reply, {error, busy}, State};
handle_call({cert_request, Hostname, SANs}, _From, State) ->
    case start_cert_request(Hostname, SANs, State) of
        {ok, State1} ->
            z_mqtt:publish(<<"model/letsencrypt/event/status">>, <<"started">>, z_acl:sudo(z_context:new(State#state.site))),
            {reply, ok, State1};
        {error, Reason, State1} ->
            {reply, {error, Reason}, State1}
    end;
handle_call(get_challenge, _From, #state{request_letsencrypt_pid = undefined} = State) ->
    ?LOG_ERROR("Fetching Letsencrypt challenge but no request running"),
    {reply, {ok, #{}}, State};
handle_call(get_challenge, _From, #state{request_letsencrypt_pid = _Pid} = State) ->
    case z_letsencrypt:get_challenge() of
        error ->
            ?LOG_ERROR("Error fetching Letsencrypt challenge."),
            {reply, {ok, #{}}, State};
        Map when is_map(Map) ->
            {reply, {ok, Map}, State}
    end;
handle_call(status, _From, State) ->
    Props = [
        {request_status, State#state.request_status},
        {request_start, State#state.request_start},
        {request_hostname, State#state.request_hostname},
        {request_san, State#state.request_san},

        {cert_is_valid, State#state.cert_is_valid},
        {cert_hostname, State#state.cert_hostname},
        {cert_san, State#state.cert_san},
        {cert_valid_till, State#state.cert_valid_till}
    ],
    {reply, {ok, Props}, State};
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast(load_cert, State) ->
    State1 = do_load_cert(State),
    z_mqtt:publish(<<"model/letsencrypt/event/status">>, <<"reload">>, z_acl:sudo(z_context:new(State#state.site))),
    {noreply, State1};
handle_cast({complete, Ret, LetsPid}, #state{request_letsencrypt_pid = LetsPid} = State) ->
    State1 = handle_letsencrypt_result(Ret, State),
    erlang:demonitor(State#state.request_monitor),
    gen_server:cast(self(), load_cert),
    {noreply, State1#state{
        request_letsencrypt_pid = undefined,
        request_monitor = undefined
    }};
handle_cast(renewal_check, #state{cert_is_valid = false} = State) ->
    {noreply, State};
handle_cast(renewal_check, #state{cert_is_valid = true, cert_hostname = Hostname, cert_san = SANs} = State) ->
    % We try renewal during the last month of validity
    % After the last validity we stop trying, as there is clearly something wrong.
    Now = calendar:universal_time(),
    NextMonth = z_datetime:next_month(Now),
    case NextMonth > State#state.cert_valid_till
        andalso Now < State#state.cert_valid_till
    of
        true ->
            SANs1 = lists:usort(SANs) -- [Hostname],
            case start_cert_request(Hostname, SANs1, State) of
                {ok, State1} ->
                    z_mqtt:publish(<<"model/letsencrypt/event/status">>, <<"started">>, z_acl:sudo(z_context:new(State#state.site))),
                    {noreply, State1};
                {error, _Reason, State1} ->
                    {noreply, State1}
            end;
        false ->
            {noreply, State}
    end;
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

handle_info({'DOWN', MRef, process, _Pid, normal}, #state{request_monitor = MRef} = State) ->
    gen_server:cast(self(), load_cert),
    {noreply, State#state{
        request_monitor = undefined,
        request_letsencrypt_pid = undefined,
        request_status = error
    }};
handle_info({'DOWN', MRef, process, _Pid, Reason}, #state{request_monitor = MRef} = State) ->
    ?LOG_ERROR(#{
        text => <<"Letsencrypt went down whilst requesting cert">>,
        in => zotonic_mod_ssl_letsencrypt,
        result => error,
        reason => Reason,
        hostname => State#state.request_hostname,
        san => State#state.request_san
    }),
    gen_server:cast(self(), load_cert),
    {noreply, State#state{
        request_monitor = undefined,
        request_letsencrypt_pid = undefined,
        request_status = error
    }};
handle_info({'DOWN', _MRef, process, _Pid, normal}, #state{request_monitor = undefined} = State) ->
    % Late down message
    {noreply, State};
handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State};
handle_info(Info, State) ->
    ?LOG_WARNING(#{
        text => <<"Letsencrypt unknown info message">>,
        in => zotonic_mod_ssl_letsencrypt,
        message => Info
    }),
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

% @doc Load the Letsencrypt certificate and extract hostnames and validity.
do_load_cert(State) ->
    Context = z_context:new(State#state.site),
    {ok, Files} = cert_files(Context),
    {certfile, CertFile} = proplists:lookup(certfile, Files),
    case filelib:is_file(CertFile) of
        true ->
            case zotonic_ssl_certs:decode_cert(CertFile) of
                {ok, CertMap} ->
                    State#state{
                        cert_is_valid = true,
                        cert_hostname = maps:get(common_name, CertMap),
                        cert_san = maps:get(subject_alt_names, CertMap, []),
                        cert_valid_till = maps:get(not_after, CertMap)
                    };
                {error, Reason} ->
                    ?LOG_ERROR(#{
                        text => <<"Could not decode Letsencrypt crt file">>,
                        in => zotonic_mod_ssl_letsencrypt,
                        result => error,
                        reason => Reason
                    }),
                    invalid_cert_status(State)
            end;
        false ->
            invalid_cert_status(State)
    end.

invalid_cert_status(State) ->
    State#state{
        cert_is_valid = false,
        cert_hostname = undefined,
        cert_san = [],
        cert_valid_till = undefined
    }.

%% @doc Letsencrypt finished, perform housekeeping and logging
handle_letsencrypt_result({ok, LEFiles}, State) ->
    ?LOG_NOTICE(#{
        text => <<"Letsencrypt successfully requested cert">>,
        in => zotonic_mod_ssl_letsencrypt,
        result => ok,
        hostname => State#state.request_hostname,
        san => State#state.request_san
    }),
    Context = z_context:new(State#state.site),
    {ok, MyFiles} = cert_files_all(Context),
    {certfile, CertFile} = proplists:lookup(certfile, MyFiles),
    {cacertfile, CaCertFile} = proplists:lookup(cacertfile, MyFiles),
    {keyfile, KeyFile} = proplists:lookup(keyfile, MyFiles),
    {CertData, IntermediateData} = split_cert_chain_file(maps:get(cert, LEFiles)),
    ok = file:write_file(CertFile, CertData),
    case IntermediateData of
        none ->
            _ = file:delete(CaCertFile),
            _ = download_cacert(Context);
        _ ->
            ok = file:write_file(CaCertFile, IntermediateData),
            _ = file:change_mode(CaCertFile, 8#00644)
    end,
    {ok, _} = file:copy(maps:get(key, LEFiles), KeyFile),
    _ = file:change_mode(CertFile, 8#00644),
    _ = file:change_mode(KeyFile, 8#00600),
    State#state{
        request_status = ok
    };
handle_letsencrypt_result({error, Reason}, State) ->
    ?LOG_ERROR(#{
        text => <<"Letsencrypt error whilst requesting cert">>,
        in => zotonic_mod_ssl_letsencrypt,
        result => error,
        reason => Reason,
        hostname => State#state.request_hostname,
        san => State#state.request_san
    }),
    State#state{
        request_status = error
    }.

start_cert_request(Hostname, SANs, #state{site = Site, request_letsencrypt_pid = undefined} = State) ->
    Context = z_context:new(Site),
    {ok, KeyFile} = ensure_key_file(Context),
    TempDir = cert_temp_dir(Context),
    ok = case file:make_dir(TempDir) of
            {error, eexist} -> ok;
            ok -> ok
         end,
    CertPath = cert_temp_dir(Context),
    LetsOpts = [
        {cert_path, CertPath},
        {key_file, KeyFile}
        | ?ACME_SRV_OPTS
    ],
    {ok, Pid} = z_letsencrypt_job:request(self(), Hostname, SANs, LetsOpts),
    {ok, State#state{
        request_letsencrypt_pid = Pid,
        request_monitor = erlang:monitor(process, Pid),
        request_hostname = Hostname,
        request_san = SANs,
        request_start = calendar:universal_time(),
        request_status = requesting
    }};
start_cert_request(_Hostname, _SANs, #state{request_letsencrypt_pid = _Pid} = State) ->
    {error, already_started, State}.


%% @doc Split the returned cert data in the certificate and the intermediate chain certs.
split_cert_chain_file(File) ->
    {ok, Data} = file:read_file(File),
    Parts = binary:split(Data, <<"-----END CERTIFICATE-----">>, [ global ]),
    Parts1 = lists:filtermap(
        fun(D) ->
            case z_string:trim(D) of
                <<>> -> false;
                D1 -> {true, <<D1/binary, 10, "-----END CERTIFICATE-----", 10>>}
            end
        end,
        Parts),
    case Parts1 of
        [ Cert ] ->
            {Cert, none};
        [ Cert | Chain ] ->
            Chain1 = lists:join(<<10>>, Chain),
            {Cert, iolist_to_binary(Chain1)}
    end.


ssl_options(Context) ->
    {ok, CertFiles} = cert_files(Context),
    CertFile = proplists:get_value(certfile, CertFiles),
    KeyFile = proplists:get_value(keyfile, CertFiles),
    case {filelib:is_file(CertFile), filelib:is_file(KeyFile)} of
        {false, false} ->
            ?LOG_NOTICE(#{
                text => <<"mod_ssl_letsencrypt: no cert and key files, skipping.">>,
                in => zotonic_mod_ssl_letsencrypt,
                cert_filename => CertFile,
                key_filename => KeyFile
            }),
            undefined;
        {false, true} ->
            ?LOG_NOTICE(#{
                text => <<"mod_ssl_letsencrypt: no cert file (though there is a key file), skipping.">>,
                in => zotonic_mod_ssl_letsencrypt,
                cert_filename => CertFile,
                key_filename => KeyFile
            }),
            undefined;
        {true, false} ->
            ?LOG_NOTICE(#{
                text => <<"mod_ssl_letsencrypt: no key file (though there is a cert file), skipping.">>,
                in => zotonic_mod_ssl_letsencrypt,
                cert_filename => CertFile,
                key_filename => KeyFile
            }),
            undefined;
        {true, true} ->
            case check_keyfile(KeyFile, Context) of
                ok -> {ok, CertFiles};
                {error, _} -> undefined
            end
    end.

cert_files(Context) ->
    SSLDir = cert_dir(Context),
    Hostname = z_context:hostname(Context),
    Files = [
        {certfile, z_convert:to_list(filename:join(SSLDir, <<Hostname/binary, ".crt">>))},
        {keyfile, z_convert:to_list(filename:join(SSLDir, <<Hostname/binary, ".key">>))}
    ] ++ z_ssl_dhfile:dh_options(),
    CaCertFile = filename:join(SSLDir, <<Hostname/binary, ".ca.crt">>),
    case filelib:is_file(CaCertFile) of
        false -> {ok, Files};
        true -> {ok, [{cacertfile, CaCertFile} | Files]}
    end.

cert_files_all(Context) ->
    SSLDir = cert_dir(Context),
    Hostname = z_context:hostname(Context),
    {ok, [
        {certfile, z_convert:to_list(filename:join(SSLDir, <<Hostname/binary, ".crt">>))},
        {cacertfile, z_convert:to_list(filename:join(SSLDir, <<Hostname/binary, ".ca.crt">>))},
        {keyfile, z_convert:to_list(filename:join(SSLDir, <<Hostname/binary, ".key">>))}
    ]}.

cert_dir(Context) ->
    PrivSSLDir = filename:join([z_path:site_dir(Context), "priv", "security", "letsencrypt"]),
    case filelib:is_dir(PrivSSLDir) of
        true ->
            PrivSSLDir;
        false ->
            {ok, SecurityDir} = z_sites_config:security_dir(z_context:site(Context)),
            filename:join([ SecurityDir, "letsencrypt" ])
    end.

cert_temp_dir(Context) ->
    filename:join([cert_dir(Context), "tmp"]).

-spec check_keyfile(string(), z:context()) -> ok | {error, no_private_keys_found|term()}.
check_keyfile(KeyFile, Context) ->
    Hostname = z_context:hostname(Context),
    case file:read_file(KeyFile) of
        {ok, Bin} ->
            case public_key:pem_decode(Bin) of
                [] ->
                    ?LOG_ERROR(#{
                        text => <<"No private keys for Letsencrypt found">>,
                        in => zotonic_mod_ssl_letsencrypt,
                        result => error,
                        hostname => Hostname,
                        reason => no_private_keys_found,
                        key_file => KeyFile
                    }),
                    {error, no_private_keys_found};
                _ ->
                    ok
            end;
        {error, Reason} = Error ->
            ?LOG_ERROR(#{
                text => <<"Cannot read Letsencrypt key file">>,
                in => zotonic_mod_ssl_letsencrypt,
                key_file => KeyFile,
                result => error,
                hostname => Hostname,
                reason => Reason
            }),
            Error
    end.

%% @doc Ensure that we have a RSA key for Letsencrypt.
-spec ensure_key_file(z:context()) -> {ok, string()} | {error, openssl|term()}.
ensure_key_file(Context) ->
    SSLDir = cert_dir(Context),
    KeyFile = filename:join(SSLDir, "letsencrypt_api.key"),
    case filelib:is_file(KeyFile) of
        true ->
            {ok, KeyFile};
        false ->
            ?LOG_NOTICE(#{
                text => <<"Generating RSA key for LetsEncrypt">>,
                in => zotonic_mod_ssl_letsencrypt,
                key_file => KeyFile
            }),
            ok = z_filelib:ensure_dir(KeyFile),
            _ = file:change_mode(filename:basename(KeyFile), 8#00700),
            Escaped = z_filelib:os_filename(KeyFile),
            Cmd = "openssl genrsa -out "
                    ++ Escaped
                    ++ " "
                    ++ z_convert:to_list(?KEY_BITS),
            Result = os:cmd(Cmd),
            case filelib:is_file(KeyFile) of
                true ->
                    _ = file:change_mode(KeyFile, 8#00600),
                    case check_keyfile(KeyFile, Context) of
                        ok ->
                            {ok, KeyFile};
                        {error, _} = Error ->
                            Error
                    end;
                false ->
                    ?LOG_ERROR(#{
                        text => <<"Error generating RSA key for LetsEncrypt">>,
                        in => zotonic_mod_ssl_letsencrypt,
                        key_file => KeyFile,
                        result => error,
                        reason => Result
                    }),
                    {error, openssl}
            end
    end.

% @doc Download the intermediate certificates
-spec download_cacert(z:context()) -> ok | {error, term()}.
download_cacert(Context) ->
    case z_url_fetch:fetch(?CA_CERT_URL, []) of
        {ok, {_Url, Hs, _Size, Cert}} ->
            case proplists:get_value("content-type", Hs) of
                "application/x-x509-ca-cert" ->
                    save_ca_cert(Cert, Context);
                "application/x-pem-file" ->
                    save_ca_cert(Cert, Context);
                CT ->
                    ?LOG_ERROR(#{
                        text => <<"Download of cert file returned unexpected content-type">>,
                        in => zotonic_mod_ssl_letsencrypt,
                        result => error,
                        reason => content_type,
                        url => ?CA_CERT_URL,
                        content_type => CT
                    }),
                    {error, content_type}
            end;
        {error, Reason} = Error ->
            ?LOG_ERROR(#{
                text => <<"Download of cert file failed">>,
                in => zotonic_mod_ssl_letsencrypt,
                result => error,
                reason => Reason,
                url => ?CA_CERT_URL
            }),
            Error
    end.

save_ca_cert(Cert, Context) ->
    SSLDir = cert_dir(Context),
    Hostname = z_context:hostname(Context),
    CaCertFile = filename:join(SSLDir, <<Hostname/binary, ".ca.crt">>),
    case file:write_file(CaCertFile, Cert) of
        ok ->
            _ = file:change_mode(CaCertFile, 8#00644),
            ok;
        {error, _} = Error ->
            Error
    end.
