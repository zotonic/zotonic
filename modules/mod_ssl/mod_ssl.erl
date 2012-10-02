%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%%
%% @doc SSL pages for Zotonic.

%% Copyright 2012 Marc Worrell
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

-module(mod_ssl).

-mod_title("HTTPS / SSL").
-mod_description("Provides https/ssl connectivity.").
-mod_provides([ssl]).
-mod_prio(100).

-author('Marc Worrell <marc@worrell.nl>').

-behaviour(supervisor).

-export([
	start_link/1,
	init/1
	]).

-export([
	observe_module_activate/2,
	pid_observe_dispatch_rules/4,
	observe_cookie_options/3,

	cert_files/1,
	check_certs/1
	]).

-include_lib("zotonic.hrl").
-include_lib("wm_host_dispatch_list.hrl").

start_link(Args) ->
	supervisor:start_link(?MODULE, Args).


init(_Args) ->
	{ok, {{one_for_one, 20, 10}, []}}.


%% @doc Ensure that all session and autologon cookies are set to 'secure' (ssl only).
observe_cookie_options(#cookie_options{name=Name}, Options, Context) ->
	case is_ssl(Context) andalso is_secure_cookie(Name) of
		true ->
			case z_convert:to_bool(m_config:get_value(mod_ssl, is_secure, Context)) of
				true ->	[{secure, true} | proplists:delete(secure, Options)];
				false -> Options
			end;
		false ->
			Options
	end.

is_ssl(Context) ->
	wrq:is_ssl(z_context:get_reqdata(Context)).

is_secure_cookie("z_sid") -> true;
is_secure_cookie("z_logon") -> true;
is_secure_cookie(_) -> false.


%% @doc Start the SSL listener.
%% TODO: better is to make the startup a process linked in our own supervisor
observe_module_activate(#module_activate{module=?MODULE, pid=SupPid}, Context) ->
	spawn_link(fun() -> check_start_ssl(SupPid, Context) end);
observe_module_activate(#module_activate{}, _Context) ->
	ok.

%% @doc Filter all dispatch rules, set the correct protocol options
% TODO: use the public port, not the listen port
pid_observe_dispatch_rules(_Pid, dispatch_rules, #wm_host_dispatch_list{dispatch_list=Dispatch} = HD, Context) ->
	{ok, SSLPort} = get_ssl_port(Context),
	Port = get_http_port(Context),
	IsSecure = z_convert:to_bool(m_config:get_value(mod_ssl, is_secure, false, Context)),
	Dispatch1 = map_ssl_option(IsSecure, Port, SSLPort, Dispatch, []),
	HD#wm_host_dispatch_list{dispatch_list=Dispatch1}.


map_ssl_option(_IsSecure, _Port, _SSLPort, [], Acc) ->
	lists:reverse(Acc);
map_ssl_option(IsSecure, Port, SSLPort, [{DispatchName, PathSchema, Mod, Props}|Rest], Acc) ->
	Props1= case proplists:get_value(ssl, Props) of
				any -> 
					[{protocol, keep} | Props];
				true ->
					[{protocol, {https, SSLPort}} | Props ];
				false ->
					[{protocol, {http, Port}} | Props ];
				undefined ->
					case IsSecure of
						true -> [{protocol, keep} | Props];
						false -> Props
					end
			end,
    map_ssl_option(IsSecure, Port, SSLPort, Rest, [{DispatchName, PathSchema, Mod, Props1}|Acc]).



check_start_ssl(SupPid, Context) ->
	case is_running(SupPid) of
	 	false -> start_ssl(SupPid, Context);
	 	true -> ok
	 end. 

is_running(SupPid) ->
	supervisor:which_children(SupPid) =/= [].


start_ssl(SupPid, Context) ->
	?zInfo("SSL: starting (~p)", [SupPid], Context),
	case check_certs(Context) of
		{ok, Certs} ->
			case ensure_port(Context) of
				{ok, Port} -> start_listener(SupPid, Port, Certs, Context);
				{error, Reason} -> ?zWarning("SSL: Can't find port: ~p", [Reason], Context)
			end;
		{error, Reason} ->
			?zWarning("SSL: problem with certificates: ~p", [Reason], Context)
	end.


%% @doc Add the SSL listeners to the mod_ssl supervisor
start_listener(SupPid, SSLPort, Certs, Context) ->
	HttpIp = z_config:get(listen_ip),
    SSLOpts = [
    	{port, SSLPort},
    	{ssl, true},
    	{ssl_opts, Certs},
        {dispatcher, z_sites_dispatcher},
        {dispatch_list, []},
	    {backlog, z_config:get(inet_backlog)}
    ],
	Name4 = z_utils:name_for_host("mochiweb_v4_ssl", Context), 
    Ipv4 = {
    	webmachine_mochiweb_v4_ssl,
    	{webmachine_mochiweb, start, [Name4, [{ip, HttpIp}|SSLOpts]]},
    	permanent, 5000, worker, dynamic
    },
	?zInfo("SSL: starting IPv4 listener on ~p:~p", [HttpIp, SSLPort], Context),
    {ok, _} = supervisor:start_child(SupPid, Ipv4), 
    %% When binding to all IP addresses ('any'), also bind for ipv6 addresses
    case HttpIp of
		any -> 
			case ipv6_supported() of
				true ->
					Name6 = z_utils:name_for_host("mochiweb_v6_ssl", Context), 
				    Ipv6 = {
				    	webmachine_mochiweb_v6_ssl,
				    	{webmachine_mochiweb, start, [Name6, [{ip, any6}|SSLOpts]]},
				    	permanent, 5000, worker, dynamic
				    },
					?zInfo("SSL: starting IPv6 listener on any:~p", [SSLPort], Context),
				    {ok, _} = supervisor:start_child(SupPid, Ipv6),
				    ok;
				false ->
					ok
			end;
		_ -> 
			ok
	end.


%% @doc Check if the localhost can bind to ipv6 addresses
ipv6_supported() ->
    case (catch inet:getaddr("localhost", inet6)) of
        {ok, _Addr} -> true;
        {error, _} -> false
    end.


get_http_port(Context) ->
	Hostname = z_context:hostname_port(Context), 
	case lists:takewhile(fun(C) -> C /= $: end, Hostname) of
		[$:|Port] -> z_convert:to_integer(Port);
		_ -> 80
	end.

get_ssl_port(Context) ->
	case z_convert:to_integer(m_config:get_value(mod_ssl, port, Context)) of
		N when is_integer(N), N > 0 -> {ok, N};
		_ -> ensure_port(Context)
	end.

%% @doc Find the port for the SSL listener
ensure_port(Context) ->
	case m_config:get_value(mod_ssl, listen_port, Context) of
		undefined ->
			find_random_port(Context);
		Port ->
			case catch z_convert:to_integer(Port) of
				N when is_integer(N) ->
					{ok, N};
				_ ->
					find_random_port(Context)
			end
	end.


find_random_port(Context) ->
	InUse = site_ssl_ports(),
	case find_next_port(InUse) of
		{ok, Port} ->
			m_config:set_value(mod_ssl, listen_port, z_convert:to_binary(Port), Context),
			{ok, Port};
		Error ->
			Error
	end.

site_ssl_ports() ->
	[ z_convert:to_integer(m_config:get_value(mod_ssl, listen_port, C)) || C <- z_sites_manager:get_site_contexts() ].


%% @doc Find the next unused port, try opening a listener.
find_next_port(InUse) ->
	case gen_tcp:listen(0, []) of
		{ok, Fd} ->
			{ok, Port} = inet:port(Fd),
			gen_tcp:close(Fd),
			case lists:member(Port, InUse) of
				false -> {ok, Port};
				true ->	find_next_port(InUse)
			end;
		{error, _} = Error ->
			Error
	end.


%% @doc Check if all certificates are available in the site's ssl directory
check_certs(Context) ->
	{ok, Opts} = cert_files(Context),
	case filelib:is_file(proplists:get_value(certfile, Opts)) of
		false ->
			case filelib:is_file(proplists:get_value(keyfile, Opts)) of
				false ->
					generate_self_signed(Opts, Context);
				true ->
					{error, {missing_certfile, proplists:get_value(certfile, Opts)}}
			end;
		true ->
			case filelib:is_file(proplists:get_value(keyfile, Opts)) of
				false ->
					{error, {missing_pemfile, proplists:get_value(keyfile, Opts)}};
				true ->
					check_keyfile(Opts)
			end
	end.

check_keyfile(Opts) ->
	Filename = proplists:get_value(keyfile, Opts),
	case file:read_file(Filename) of
		{ok, <<"-----BEGIN PRIVATE KEY", _/binary>>} ->
			{error, {need_rsa_private_key, Filename, "use: openssl rsa -in sitename.key -out sitename.pem"}};
		{ok, Bin} ->
			case public_key:pem_decode(Bin) of
				[] -> {error, {no_private_keys_found, Filename}};
				_ -> {ok, Opts}
			end;
		Error ->
			{error, {cannot_read_pemfile, Filename, Error}}
	end.


generate_self_signed(Opts, Context) ->
	PemFile = proplists:get_value(keyfile, Opts),
	case filelib:ensure_dir(PemFile) of
		ok ->
			KeyFile = filename:rootname(PemFile) ++ ".key",
			CertFile = proplists:get_value(certfile, Opts),
			Hostname = z_context:hostname(Context),
			Command = "openssl req -x509 -nodes"
					++ " -days 3650"
					++ " -subj '/CN="++Hostname++"'"
					++ " -newkey rsa:2048 "
					++ " -keyout "++KeyFile
					++ " -out "++CertFile,
			lager:info("SSL: ~p", [Command]),
			Result = os:cmd(Command),
			lager:info("SSL: ~p", [Result]),
			case file:read_file(KeyFile) of
				{ok, <<"-----BEGIN PRIVATE KEY", _/binary>>} ->
					os:cmd("openssl rsa -in "++KeyFile++" -out "++PemFile),
					{ok, Opts};
				{ok, <<"-----BEGIN RSA PRIVATE KEY", _/binary>>} ->
					file:rename(KeyFile, PemFile), 
					{ok, Opts};
				_Error ->
					{error, "Could not generate self signed certificate"}
			end;
		{error, _} = Error ->
			{error, {ensure_dir, Error, PemFile}}
	end. 


cert_files(Context) ->
	SSLDir = filename:join(z_path:site_dir(Context), "ssl"),
	Sitename = z_convert:to_list(z_context:site(Context)), 
	Files = [
		{certfile, filename:join(SSLDir, Sitename++".crt")},
		{keyfile, filename:join(SSLDir, Sitename++".pem")},
		{password, m_config:get_value(mod_ssl, password, "", Context)}
	],
	CaCertFile = filename:join(SSLDir, Sitename++".ca.crt"),
	case filelib:is_file(CaCertFile) of
		false -> {ok, Files};
		true -> {ok, [{cacertfile, CaCertFile} | Files]}
	end.

