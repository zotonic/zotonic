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

-module(filter_ip2country).

-export([
    ip2country/2
    ]).

ip2country(undefined, _Context) ->
    undefined;
ip2country(IP, _Context) when is_binary(IP); is_list(IP); is_tuple(IP) ->
    case locus:lookup(city, IP) of
        {ok, #{ <<"country">> := Country }} ->
            z_string:to_lower(maps:get(<<"iso_code">>, Country, <<>>));
        {ok, _} ->
            undefined;
        {error, _} ->
            undefined
    end.
