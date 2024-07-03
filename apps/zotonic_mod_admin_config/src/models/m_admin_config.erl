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

-behaviour(zotonic_model).

-export([
    m_get/3,
    ssl_certificates/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"ssl_certificates">> | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {ssl_certificates(Context), Rest}};
        false -> {ok, {[], Rest}}
    end;
m_get([ <<"security_dir">> | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            case z_config_files:security_dir() of
                {ok, SecDir} ->
                    {ok, {SecDir, Rest}};
                {error, _} ->
                    {ok, {<<>>, Rest}}
            end;
        false ->
            {ok, {<<>>, Rest}}
    end;
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.

ssl_certificates(Context) ->
    Observers = z_notifier:get_observers(ssl_options, Context),
    [ ssl_certificate(Observer, Context) || {_Prio, Observer, _Pid} <- Observers ]
    ++ [ self_signed(Context) ].

ssl_certificate({Module, observe_ssl_options}, Context) ->
    Hostname = z_context:hostname(Context),
    case Module:observe_ssl_options(#ssl_options{server_name=Hostname}, Context) of
        undefined ->
            modinfo(Module);
        {ok, Options} when is_list(Options) ->
            case proplists:lookup(certfile, Options) of
                {certfile, CertFile} ->
                    case zotonic_ssl_certs:decode_cert(CertFile) of
                        {ok, CertProps} ->
                            modinfo(Module) ++ [
                                {certificate, CertProps},
                                {directory, filename:dirname(CertFile)}
                            ];
                        {error, _} ->
                            modinfo(Module)
                    end;
                none ->
                    modinfo(Module)
            end
    end.

self_signed(Context) ->
    Options = z_ssl_certs:sni_self_signed(z_context:hostname(Context)),
    {certfile, CertFile} = proplists:lookup(certfile, Options),
    case zotonic_ssl_certs:decode_cert(CertFile) of
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
