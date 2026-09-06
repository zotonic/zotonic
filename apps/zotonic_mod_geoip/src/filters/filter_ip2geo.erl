%% @doc Map IP addresses to geo a country code.
%% @author Marc Worrell <marc@worrell.nl>
%% @end

%% Copyright 2019 Marc Worrell
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

-module(filter_ip2geo).
-moduledoc(#{
    zotonic_keywords => ["reference", "frontend_developer", "template_filter", "geolocation", "structured_data"]
}).
-moduledoc("
Return geographic information associated with an IP address.

The input can be a textual or tuple IP address. The result is a map supplied by
the configured GeoIP database, or `undefined` when the address cannot be
located.

For example:

```django
{% with \"8.8.8.8\"|ip2geo as geo %}
    {{ geo.country.iso_code }}
{% endwith %}
```
").

-export([
    ip2geo/2
    ]).

ip2geo(undefined, _Context) ->
    undefined;
ip2geo(IP, _Context) when is_binary(IP); is_list(IP); is_tuple(IP) ->
    case mod_geoip:lookup(IP) of
        {ok, Info} ->
            Info;
        {error, _} ->
            undefined
    end.
