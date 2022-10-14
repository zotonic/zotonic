%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2014-2022 Arjan Scherpenisse
%% @doc Database pool worker behaviour definition
%% @end

%% Copyright 2014-2022 Arjan Scherpenisse
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

-module(z_db_worker).

-callback test_connection(WorkerArgs) -> ok | {error, Reason} when
      WorkerArgs :: proplists:proplist(),
      Reason     :: term().

-callback squery(Worker, Sql, Timeout) -> epgsql:reply(epgsql:squery_row()) when
      Worker :: pid(),
      Sql :: string(),
      Timeout :: non_neg_integer().

-callback equery(Worker, Sql, Parameters, Timeout) -> epgsql:reply(epgsql:equery_row()) when
      Worker :: pid(),
      Sql :: string(),
      Parameters :: [epgsql:bind_param()],
      Timeout :: non_neg_integer().
