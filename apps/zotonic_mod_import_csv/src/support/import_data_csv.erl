%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @author Marc Worrell <marc@worrell.nl>
%% @doc Import a csv file according to the derived file/record definitions.
%% @end

%% Copyright 2010-2025 Marc Worrell, Arjan Scherpenisse
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


-module(import_data_csv).

-author("Arjan Scherpenisse <arjan@scherpenisse.net>").
-author("Marc Worrell <marc@worrell.nl>").

-export([
    import/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc The import function, read the csv file and fetch all records.
%% This function is run as a spawned process.  The Context should have the right permissions for inserting
%% or updating the resources.
-spec import(File, DataDef, IsReset, Context) -> import_rows_data:importresults() when
    File :: file:filename_all(),
    DataDef :: #import_data_def{},
    IsReset :: boolean(),
    Context :: z:context().
import(File, Def, IsReset, Context) ->
    StartDate = erlang:universaltime(),
    %% Read and parse all rows
    {ok, Device} = file:open(File, [read, binary, {encoding, utf8}]),
    Rows = z_csv_parser:scan_lines(Device, Def#import_data_def.colsep),
    file:close(Device),
    %% Import all rows
    Result = import_rows_data:import_rows(Rows, Def, IsReset, Context),
    %% Return the stats from this import run
    [
        {file, filename:basename(File)},
        {date_start, StartDate}
        | proplists:delete(date_start, Result)
    ].

