%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2013 Arjan Scherpenisse
%% @doc Zotonic: administrative functions

%% Copyright 2013 Arjan Scherpenisse
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

-module(m_admin_rsc).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include_lib("zotonic_core/include/zotonic.hrl").


%% interface functions
-export([
    m_get/2
]).

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), z:context() ) -> {term(), list()}.
m_get([ pivot_queue_count | Rest ], Context) ->
    {z_pivot_rsc:queue_count(Context), Rest};
m_get(Vs, _Context) ->
    lager:error("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {undefined, []}.
