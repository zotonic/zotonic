%% @doc Map IP addresses to geo a country code.
%% @author Marc Worrell <marc@worrell.nl>

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

-module(filter_ip2info).

-export([
    ip2info/2
    ]).

ip2info(undefined, _Context) ->
    undefined;
ip2info(IP, _Context) when is_binary(IP); is_list(IP); is_tuple(IP) ->
    case mod_geoip:lookup(IP) of
        {ok, Info} ->
            Info;
        {error, _} ->
            undefined
    end.
