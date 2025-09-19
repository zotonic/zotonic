%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016-2024 Marc Worrell
%% @doc Check if a hostname can be used for a letsencrypt certificate.
%% @end

%% Copyright 2016-2024 Marc Worrell
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
-moduledoc("
See also

[mod\\_ssl\\_letsencrypt](/id/doc_module_mod_ssl_letsencrypt)

Test if a hostname can be used for a Let’s Encrypt certificate.

Criteria are:

1.   Does resolve using DNS
2.   The resolved address is not a LAN address
3.   The address is reachable
4.   And the current site is listening for the hostname on that address

The site must listen on port 80 for connections.

For example, check if the current site is reachable as *example.com*:


```django
{% if \"example.com\"|is_letsencrypt_valid_hostname %}
    Wow, this site is example.com!?!
{% endif %}
```
").

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
    ContextNoLang = z_context:set_language('x-default', Context),
    Url = z_dispatcher:url_for(letsencrypt_ping, ContextNoLang),
    Url1 = "http://" ++ z_convert:to_list(Hostname) ++ z_convert:to_list(Url),
    case z_fetch:fetch(Url1, [ insecure ], Context) of
        {ok, {_FinalUrl, _Hs, _Len, Body}} ->
            case mod_ssl_letsencrypt:is_self_ping(Body, Context) of
                false ->
                    ?LOG_INFO(#{
                        text => <<"The configured hostname is not mapped to this site">>,
                        in => zotonic_mod_ssl_letsencrypt,
                        result => error,
                        reason => self_ping,
                        hostname => z_convert:to_binary(Hostname),
                        url => z_convert:to_binary(Url1)
                    }),
                    false;
                true ->
                    ?LOG_DEBUG(#{
                        text => <<"The configured hostname is mapped to this site">>,
                        in => zotonic_mod_ssl_letsencrypt,
                        result => ok,
                        hostname => z_convert:to_binary(Hostname),
                        url => z_convert:to_binary(Url1)
                    }),
                    true
            end;
        {error, Reason} ->
            ?LOG_INFO(#{
                text => <<"The configured hostname could not be checked">>,
                in => zotonic_mod_ssl_letsencrypt,
                result => error,
                reason => Reason,
                hostname => z_convert:to_binary(Hostname),
                url => z_convert:to_binary(Url1)
            }),
            false
    end.
