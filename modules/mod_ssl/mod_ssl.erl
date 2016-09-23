%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%%
%% @doc SSL pages for Zotonic.

%% Copyright 2012 - 2016 Marc Worrell, Maas-Maarten Zeeman
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
-mod_depends([ssl_certificates]).
-mod_prio(100).

-author('Marc Worrell <marc@worrell.nl>').

-export([
	pid_observe_dispatch_rules/4,
	observe_cookie_options/3,
	observe_url_abs/2
]).

-include_lib("zotonic.hrl").
-include_lib("wm_host_dispatch_list.hrl").

%% @doc Ensure that the right absolute url is returned when this is a 'secure' request.
observe_url_abs(#url_abs{url=Url}, Context) ->
	case is_ssl(Context) of
		false -> undefined;
		true ->
			z_url:abs_link(Url, secure_base_url(Context))
	end.

secure_base_url(Context) ->
	Base = case z_config:get(ssl_port) of
		443 ->
			[<<"https://">>, z_context:hostname(Context)];
		Port ->
			[<<"https://">>, z_context:hostname(Context), $:, z_convert:to_list(Port)]
	end,
	iolist_to_binary(Base).


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
	m_req:get(is_ssl, Context).

is_secure_cookie(<<"z_sid">>) -> true;
is_secure_cookie(<<"z_logon">>) -> true;
is_secure_cookie(Cookie) when is_binary(Cookie) -> false.

%% @doc Filter all dispatch rules, set the correct protocol options
pid_observe_dispatch_rules(_Pid, dispatch_rules, #wm_host_dispatch_list{dispatch_list=Dispatch} = HD, Context) ->
	SSLPort = z_config:get(ssl_port),
	Port = z_config:get(port),
	IsSecure = z_convert:to_bool(m_config:get_value(mod_ssl, is_secure, false, Context)),
	IsSSL = z_convert:to_bool(m_config:get_value(mod_ssl, is_ssl, false, Context)),
	Dispatch1 = map_ssl_option(IsSecure, IsSSL, Port, SSLPort, Dispatch, []),
	HD#wm_host_dispatch_list{dispatch_list=Dispatch1}.


map_ssl_option(_IsSecure, _IsSSL, _Port, _SSLPort, [], Acc) ->
	lists:reverse(Acc);
map_ssl_option(IsSecure, IsSSL, Port, SSLPort, [{DispatchName, PathSchema, Mod, Props}|Rest], Acc) ->
	Props1= case proplists:get_value(ssl, Props) of
				any ->
					case IsSSL of
						true -> [{protocol, {https, SSLPort}} | Props ];
						false -> [{protocol, keep} | Props]
					end;
				true ->
					[{protocol, {https, SSLPort}} | Props ];
				false ->
					[{protocol, {http, Port}} | Props ];
				undefined ->
					case IsSSL of
						true ->
							[{protocol, {https, SSLPort}} | Props ];
						false ->
							case IsSecure of
								true -> [{protocol, keep} | Props];
								false -> Props
							end
					end
			end,
    map_ssl_option(IsSecure, IsSSL, Port, SSLPort, Rest, [{DispatchName, PathSchema, Mod, Props1}|Acc]).

