%% @author %%FULLNAME%%
%% @copyright %%YEAR%% %%FULLNAME%%
%% Generated on %%DATE%%
%% @doc %%SITE%% root supervisor.

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

-module(%%SITE%%_sup).
-author("%%FULLNAME%%").

-behaviour(supervisor).

-export([ start_link/0, init/1 ]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 1,
        period => 5
    },
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.
