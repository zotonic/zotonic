%% @doc Placeholder, code moved to src/support/csv/z_csv_parser.erl
%% @author Arjan Scherpenisse <arjan@scherpenisse.net>

%% Copyright 2010-2013 Arjan Scherpenisse
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

-module(parse_csv).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
    scan_lines/1,
    scan_lines/2,
    cleanup_field/1
]).

-define(CHUNK_SIZE, 4096).


scan_lines(DeviceOrFilename) ->
    z_csv_parser:scan_lines(DeviceOrFilename).

scan_lines(Filename, FieldSep) when is_list(Filename); is_binary(Filename) ->
    z_csv_parser:scan_lines(Filename, FieldSep).

cleanup_field(L) ->
    z_csv_parser:cleanup_field(L).
