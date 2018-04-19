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

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/2,

    status/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), z:context() ) -> {term(), list()}.
m_get([ status | Rest ], Context) ->
    Status = case status(Context) of
        {ok, S} -> S;
        {error, _} -> undefined
    end,
    {Status, Rest};
m_get(Vs, _Context) ->
    lager:error("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {undefined, []}.


status(Context) ->
    mod_ssl_letsencrypt:status(Context).

