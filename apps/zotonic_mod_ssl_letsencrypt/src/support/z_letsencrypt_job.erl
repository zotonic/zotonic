%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020-2024 Marc Worrell, Maas-Maarten Zeeman
%% @doc Interface to erlang-letsencrypt
%% @end

%% Copyright 2020-2024 Marc Worrell, Maas-Maarten Zeeman
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

-module(z_letsencrypt_job).

-export([
    request/4,
    request_process/4
    ]).

-include_lib("kernel/include/logger.hrl").


%% @doc Start a letsencrypt server for requesting this hostname
-spec request( pid(), binary(), list( binary() ), list()) -> {ok, pid()} | {error, overload}.
request(ModulePid, Hostname, SANs, LetsOpts) ->
    z_sidejob:start(?MODULE, request_process, [ ModulePid, Hostname, SANs, LetsOpts ]).

request_process(ModulePid, Hostname, SANs, LetsOpts) ->
    case global:trans(
        {?MODULE, self()},
        fun() ->
            CertOpts = #{
                san => SANs,
                async => false
            },
            case z_letsencrypt:start(LetsOpts) of
                {ok, _}  ->
                    Result = z_letsencrypt:make_cert(Hostname, CertOpts),
                    gen_server:cast(ModulePid, {complete, Result, self()}),
                    z_letsencrypt:stop(),
                    ok;
                {error, _} = Error ->
                    Error
            end
        end)
    of
        ok ->
            ok;
        {error, _} = Error ->
            Error;
        aborted ->
            ?LOG_ERROR(#{
                in => zotonic_mod_ssl_letsencrypt,
                text => <<"LetsEncrypt process aborted">>,
                result => error,
                reason => aborted,
                hostname => Hostname
            }),
            {error, aborted}
    end.

