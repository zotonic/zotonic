%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell
%% @doc Check if a hostname can be used for a letsencrypt certificate.

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

-module(filter_is_letsencrypt_valid_hostname).

-export([ is_letsencrypt_valid_hostname/2 ]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("kernel/include/inet.hrl").

is_letsencrypt_valid_hostname(undefined, _Context) ->
    false;
is_letsencrypt_valid_hostname(<<>>, _Context) ->
    false;
is_letsencrypt_valid_hostname([], _Context) ->
    false;
is_letsencrypt_valid_hostname(Hostname, Context) ->
    is_valid_hostname(Hostname)
    andalso is_non_local(Hostname)
    andalso is_this_site(Hostname, Context).

is_valid_hostname(Hostname) ->
    case re:run(Hostname, "^([a-z0-9\\-]+\\.)+[a-z][a-z]+$") of
        {match, _} -> true;
        nomatch -> false
    end.

is_non_local(Hostname) ->
    case inet:gethostbyname(z_convert:to_list(Hostname)) of
        {ok, #hostent{h_addr_list = []}} ->
            false;
        {error, nxdomain} ->
            false;
        {ok, #hostent{h_addr_list = Addrs}} ->
            lists:all(fun(Adr) -> not z_ip_address:is_local(Adr) end, Addrs)
    end.

%% Ping the url, should return our (random) secret
is_this_site(Hostname, Context) ->
    Url = z_dispatcher:url_for(letsencrypt_ping, Context),
    Url1 = "http://" ++ z_convert:to_list(Hostname) ++ z_convert:to_list(Url),
    case httpc:request(get,
                  {Url1, []},
                  [ {autoredirect, true}, {relaxed, true}, {timeout, 2000}, {connect_timeout, 2000} ],
                  [ {body_format, binary} ])
    of
        {ok, {{_Http, 200, _Ok}, _Hs, Body}} ->
            mod_ssl_letsencrypt:is_self_ping(Body, Context);
        _ ->
            false
    end.

