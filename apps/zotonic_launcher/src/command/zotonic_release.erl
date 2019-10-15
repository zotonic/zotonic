%%%-------------------------------------------------------------------
%%% @author Blaise
%%% @doc
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%	 http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%%
%%% @end
%%% Created : 18. Dec 2017 4:53 PM
%%%-------------------------------------------------------------------
-module(zotonic_release).
-author("Blaise").

%% API
-export([run/0]).

-include_lib("zotonic_core/include/zotonic_release.hrl").

run() ->
    io:format("Zotonic ~s~n", [?ZOTONIC_VERSION]).
