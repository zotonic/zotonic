%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2016-2020 Arjan Scherpenisse
%% @doc DKIM signing of outgoing emails

%% Copyright 2016-2020 Arjan Scherpenisse
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

-module(m_email_dkim).
-author("Arjan Scherpenisse <arjan@miraclethings.nl>").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3,

    admin_info/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"admin_info">> | Rest ], _Msg, Context) ->
    case z_acl:is_allowed(use, mod_email_dkim, Context) of
        true ->
            {ok, {admin_info(Context), Rest}};
        false ->
            {error, eacces}
    end;
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.

admin_info(Context) ->
    {Priv, Pub} = z_email_dkim:cert_files(Context),
    #{
        <<"privkey_filepath">> => z_convert:to_binary(Priv),
        <<"pubkey_filepath">> => z_convert:to_binary(Pub),
        <<"dns_entry_domain">> => z_convert:to_binary(z_email_dkim:dns_entry_domain(Context)),
        <<"dns_entry">> => z_convert:to_binary(z_email_dkim:dns_entry(Context))
    }.
