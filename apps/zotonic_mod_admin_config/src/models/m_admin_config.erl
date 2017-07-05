%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell
%% @doc Model returning information about the available SSL certificates.

%% Copyright 2016 Marc Worrell
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

-module(m_admin_config).

-behaviour(gen_model).

-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2
    ]).

-export([
    ssl_certificates/1
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

m_find_value(ssl_certificates, #m{}, Context) ->
    ssl_certificates(Context).

m_to_list(#m{}, _Context) ->
    [].

m_value(#m{}, _Context) ->
    undefined.

ssl_certificates(Context) ->
    Observers = z_notifier:get_observers(ssl_options, Context),
    [ ssl_certificate(Observer, Context) || {_Prio, Observer} <- Observers ]
    ++ [ self_signed(Context) ].

ssl_certificate({Module, observe_ssl_options}, Context) ->
    Hostname = z_context:hostname(Context),
    case Module:observe_ssl_options(#ssl_options{server_name=Hostname}, Context) of
        undefined ->
            modinfo(Module);
        {ok, Options} when is_list(Options) ->
            case proplists:lookup(certfile, Options) of
                {certfile, CertFile} ->
                    case z_ssl_certs:decode_cert(CertFile) of
                        {ok, CertProps} ->
                            modinfo(Module) ++ [
                                {certificate, CertProps}
                            ];
                        {error, _} ->
                            modinfo(Module)
                    end;
                none ->
                    modinfo(Module)
            end
    end.

self_signed(Context) ->
    Options = z_ssl_certs:sni_self_signed(Context),
    {certfile, CertFile} = proplists:lookup(certfile, Options),
    case z_ssl_certs:decode_cert(CertFile) of
        {ok, CertProps} ->
            [
                {is_zotonic_self_signed, true},
                {certificate, CertProps}
            ];
        {error, _} ->
            [
                {is_zotonic_self_signed, true}
            ]
    end.

modinfo(Module) ->
    Props = Module:module_info(attributes),
    [
        {is_zotonic_self_signed, false},
        {module, Module},
        {mod_title, proplists:get_value(mod_title, Props)},
        {mod_description, proplists:get_value(mod_description, Props)}
    ].
