%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell, Maas-Maarten Zeeman
%%
%% @doc Fetch information about the SSL certificates

%% Copyright 2016 Marc Worrell, Maas-Maarten Zeeman
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

-module(m_ssl_letsencrypt).

-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,

    status/1
]).

-include("zotonic.hrl").

m_find_value(status, #m{}, Context) ->
    case status(Context) of
        {ok, Status} -> Status;
        {error, _} -> undefined
    end;
m_find_value(_, #m{}, _Context) ->
    undefined.

m_to_list(#m{}, _Context) ->
    [].

m_value(#m{}, _Context) ->
    undefined.


status(Context) ->
    mod_ssl_letsencrypt:status(Context).

