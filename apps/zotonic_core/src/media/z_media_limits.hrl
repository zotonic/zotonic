%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Shared media execution limits. Sizes are bytes; timeouts are milliseconds.
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

-ifndef(Z_MEDIA_LIMITS_HRL).
-define(Z_MEDIA_LIMITS_HRL, true).

-define(DEFAULT_MEDIA_LIMIT, 17179869184).  % 16GiB
-define(DEFAULT_JSON_LIMIT, 135266304).     % 129MiB
-define(DEFAULT_JOB_TIMEOUT, 14400000).     % 4 hours in milliseconds
-define(MAX_JOB_TIMEOUT, 43200000).         % 12 hours in milliseconds
-define(MAX_JOB_CMDSIZE, 65536).            % 64KiB
-define(MAX_JOB_FILECOUNT, 32).             % 32 files
-define(MAX_CONSOLE_SIZE, 16777216).        % 16MiB
-define(MAX_URL_SIZE, 2048).                % 2KiB
-define(MAX_HOSTNAME_SIZE, 253).            % RFC 1035, minus two bytes

-define(UPLOAD_TIMEOUT, 3600000).           % 1 hour in milliseconds
-define(CONNECT_TIMEOUT, 5000).             % 5 seconds in milliseconds


%% Client callback budget includes the longest render plus five minutes of slack.
-define(DEFAULT_RUNNER_WAIT_SECONDS, (?MAX_JOB_TIMEOUT div 1000 + 300)).

%% Smaller defaults for work in the general pool.
-define(IMAGE_TIMEOUT, 120000).             % 2 minutes
-define(PREVIEW_TIMEOUT, 120000).           % 2 minutes
-define(PROBE_TIMEOUT, 60000).              % 1 minute
-define(FILE_TIMEOUT, 10000).               % 10 seconds
-define(MAX_IMAGE_TIMEOUT, 600000).         % 10 minutes
-define(MAX_PREVIEW_TIMEOUT, 600000).       % 10 minutes
-define(MAX_PROBE_TIMEOUT, 600000).         % 10 minutes
-define(MAX_FILE_TIMEOUT, 60000).           % 1 minute
-define(IMAGE_FILE_SIZE, 1073741824).       % 1 GiB
-define(PROBE_FILE_SIZE, 1048576).          % 1 MiB
-define(SMALL_CONSOLE_SIZE, 1048576).       % 1 MiB
-define(FILE_CONSOLE_SIZE, 65536).          % 64 KiB
-define(MEDIA_MEMORY, 4294967296).          % 4 GiB
-define(PROBE_MEMORY, 2147483648).          % 2 GiB
-define(FILE_MEMORY, 536870912).            % 512 MiB

-endif.
