%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Retry policy for media processing. Unknown failures retain recoverable inputs.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(z_media_error).
-export([classify/1]).

-spec classify(Reason) -> retry | pause | terminal | obsolete when Reason :: term().
classify(stale_medium) -> obsolete;
classify({media_runner_processing, <<"worker_failed">>}) -> retry;
classify({media_runner_processing, <<"job_expired">>}) -> retry;
classify({media_runner_processing, Reason}) when
        Reason =:= <<"command_failed">>; Reason =:= <<"command_timeout">>;
        Reason =:= <<"output_limit">> -> terminal;
classify({media_runner_unavailable, _}) -> retry;
classify(media_runner_busy) -> retry;
classify(timeout) -> retry;
classify(deleting) -> retry;
classify(enoent) -> retry;
classify(convert) -> terminal;
classify(convert_error) -> terminal;
classify({exit_status, _}) -> terminal;
classify(_) -> pause.
