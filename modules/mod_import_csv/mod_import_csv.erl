%% @doc Import tab separated files.  There must be an import definition for the file to be accepted.
%% @author Marc Worrell <marc@worrell.nl>
%% Date: 2010-06-26

%% Copyright 2010-2011 Marc Worrell
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

-module(mod_import_csv).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Import CSV Data").
-mod_description("Import files with tab separated data.").
-mod_prio(20).

%% interface functions
-export([
	observe_dropbox_file/2,
	can_handle/2,
	event/2
]).

-include_lib("zotonic.hrl").
-include_lib("include/import_csv.hrl").

%% @doc Handle a dropbox file when it is a tsv file we know.
observe_dropbox_file({dropbox_file, F}, Context) ->
    case filename:extension(F) of
        ".csv" ->
            %% Correct file type, see if we can handle the file.
            %% Either a module has a definition or there are correct header lines.
            case can_handle(F, Context) of
                {ok, Definition} -> handle_spawn(Definition, Context), true;
                false -> undefined
            end;
        _ ->
            undefined
    end.


%% @doc Uploading a CSV file through the web interface.
event({submit, {csv_upload, []}, _TriggerId, _TargetId}, Context) ->
    #upload{filename=OriginalFilename, tmpfile=TmpFile} = z_context:get_q_validated("upload_file", Context),

    %% Move temporary file to processing directory
    Dir = z_path:files_subdir_ensure("processing", Context),
    Target = filename:join([Dir, OriginalFilename]),
    file:delete(Target),
    {ok, _} = file:copy(TmpFile, Target),
    file:delete(TmpFile),

    %% Process the file
    Context2 = case can_handle(Target, Context) of
                   {ok, Definition} ->
                       handle_spawn(Definition, Context),
                       z_render:growl(?__("Please hold on while the file is importing. You will get a notification when it is ready.", Context), Context);
                   false ->
                       file:delete(Target),
                       z_render:growl_error(?__("This file cannot be imported.", Context), Context)
               end,
    z_render:wire([{dialog_close, []}], Context2).


%%====================================================================
%% Internal functions
%%====================================================================


handle_spawn(Def, Context) ->
    {ok, Def1} = to_importing_dir(Def, Context),
    spawn(fun() -> import_csv:import(Def1, z_acl:sudo(z_context:new(Context))) end).



%%====================================================================
%% File handling
%%====================================================================

%% @doc Move the to be imported file to the importing dir, from the processing dir.
to_importing_dir(Def, Context) ->
    ImportDir = z_path:files_subdir_ensure("importing", Context),
    Target = filename:join([ImportDir, filename:basename(Def#filedef.filename)]),
    file:delete(Target),
    ok = file:rename(Def#filedef.filename, Target),
    {ok, Def#filedef{filename=Target}}.


%% @doc Check if we can import this file
can_handle(Filename, Context) ->
    %% @todo Add here a notify to the modules to see if they have an import definition for basename(Filename)
    FSize = filelib:file_size(Filename),
    case z_notifier:first({import_csv_definition, filename:basename(Filename), Filename}, Context) of
        {ok, #import_data_def{colsep=ColSep, skip_first_row=SkipFirstRow, record=Record, importdef=ImportDef}} ->
            {ok, #filedef{
                        filename=Filename, 
                        file_size=FSize, 
                        colsep=ColSep, 
                        columns=unknown,
                        skip_first_row=SkipFirstRow,
                        record=Record,
                        importdef=ImportDef
                }};
        undefined ->
            case file:open(Filename, [read, binary]) of
                {ok, Device} ->
                    case file:read(Device, min(4096,FSize)) of
                        {ok, Data} ->
                            file:close(Device),
                            case is_valid_file(Data) of
                                {ok, Cols, Sep} ->
                                    {ok, #filedef{
                                                filename=Filename, 
                                                file_size=FSize, 
                                                colsep=Sep, 
                                                columns=Cols,
                                                skip_first_row=true,
                                                record=cols2record(Cols),
                                                importdef=cols2importdef(Cols)
                                        }};
                                _ ->
                                    false
                            end;
                        {error, _Reason} ->
                            file:close(Device),
                            false
                    end;
                {error, _Reason} ->
                    false
            end
    end.


min(A,B) when A < B -> A;
min(_,B) -> B.


%% @doc Check if the first row is made up of column headers.
%% The file must have at least a name and a category column.
is_valid_file(<<>>) ->
    false;
is_valid_file(B) ->
    case fetch_line(B) of
        {ok, Line} ->
            {ok, Tabs} = parse_line(Line, $\t, [], []),
            {ok, Comma} = parse_line(Line, $,, [], []),
            {Cols, Sep} = case length(Tabs) > length(Comma) of
                                true -> {Tabs, $\t};
                                false -> {Comma, $,}
                            end,
            case lists:member("name", Cols) andalso lists:member("category", Cols) of
                true -> {ok, Cols, Sep};
                false -> false
            end;
        _ ->
            false
    end.

fetch_line(B) ->
    fetch_line(B, []).

fetch_line(<<>>, _Line) ->
    false;
fetch_line(<<10, _/binary>>, Line) ->
    {ok, lists:reverse(Line)};
fetch_line(<<13, _/binary>>, Line) ->
    {ok, lists:reverse(Line)};
fetch_line(<<C, B/binary>>, Line) ->
    fetch_line(B, [C|Line]).

%% @doc Try to parse the line with the given field escape and quote chars.
parse_line([], _Sep, Col, Cols) ->
    {ok, lists:reverse([parse_csv:trim_field(lists:reverse(Col))|Cols])};
parse_line([Sep|Rest], Sep, Col, Cols) ->
    parse_line(Rest, Sep, [], [parse_csv:trim_field(lists:reverse(Col))|Cols]);
parse_line([C|Rest], Sep, Col, Cols) ->
    parse_line(Rest, Sep, [C|Col], Cols).
    

%%====================================================================
%% Default import definitions
%%====================================================================

%% @doc Straight forward mapping of column names to property names
cols2importdef(Cols) ->
    Cols1 = [ list_to_atom(Col) || Col <- Cols ],
    [
        {
        % Field mapping
        [
         {name, {concat, [name_prefix, name]}}
         | 
         lists:filter(fun(X) -> X =/= undefined end, [cols2importdef_map(Col) || Col <- unique(Cols1,[])])
        ]
        ,
        % Edges
        []
        }
    ].

%% @doc The names of the columns are as in the column header
cols2record(Cols) ->
    Cols1 = [ z_string:trim(Col) || Col <- Cols],
    [ list_to_atom(Col) || Col <- Cols1 ].

unique([], Acc) ->
    lists:reverse(Acc);
unique([C|Cs], Acc) ->
    case lists:member(C, Acc) of
        true -> unique(Cs, Acc);
        false -> unique(Cs, [C|Acc])
    end.

%% @doc Maps well-known column names to an import definition.
cols2importdef_map('') -> undefined;
cols2importdef_map(name) -> undefined;
cols2importdef_map(name_prefix) -> undefined;
cols2importdef_map(date_start) ->  {date_start, {datetime, date_start, <<"00:00:00">>}};
cols2importdef_map(date_end) ->  {date_end, {datetime, date_end, <<"23:59:59">>}};
cols2importdef_map(X) -> {X, X}.
