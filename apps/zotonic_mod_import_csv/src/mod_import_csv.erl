%% @doc Import tab or comma separated files. There must be an import definition for the file to be accepted.
%% @author Marc Worrell <marc@worrell.nl>
%% @end

%% Copyright 2010-2025 Marc Worrell
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
-mod_schema(1).

%% interface functions
-export([
    observe_dropbox_file/2,
    observe_admin_menu/3,
    can_handle/3,
    event/2,
    inspect_file/1,
    manage_schema/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_import_csv/include/import_csv.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").


%% @doc Handle a drop folder file when it is a tsv/csv file we know.
observe_dropbox_file(#dropbox_file{ filename = F }, Context) ->
    case is_csv( filename:extension(F) ) of
        true ->
            %% Correct file type, see if we can handle the file.
            %% Either a module has a definition or there are correct header lines.
            case can_handle(F, F, Context) of
                {ok, Definition} ->
                    % Import in background, let dropbox keep the file in processing.
                    handle_spawn(Definition, false, z_acl:sudo(Context)),
                    {ok, processing};
                ok ->
                    % Handled by the notifier - dropbox can move the file to handled.
                    ok;
                {error, _} ->
                    undefined
            end;
        false ->
            undefined
    end.

is_csv(".csv") -> true;
is_csv(<<".csv">>) -> true;
is_csv(_) -> false.

%% @doc Add menu item to 'Content' admin menu
-spec observe_admin_menu(#admin_menu{}, list(), z:context()) -> list().
observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
        #menu_separator{
            parent = admin_content,
            visiblecheck = {acl, use, mod_import_csv}
        },
        #menu_item{
            id = admin_import,
            parent = admin_content,
            label = ?__("Import content", Context),
            url = {admin_import},
            visiblecheck = {acl, use, mod_import_csv}
        }|
        Acc
    ].

%% @doc Uploading a CSV file through the web interface.
event(#submit{message={csv_upload, []}}, Context) ->
    case z_acl:is_allowed(use, mod_import_csv, Context) of
        true ->
            #upload{filename=OriginalFilename, tmpfile=TmpFile} = z_context:get_q_validated(<<"upload_file">>, Context),
            IsReset = z_convert:to_bool(z_context:get_q(<<"reset">>, Context)),

            % Move temporary file to the dropbox processing directory
            % It will be deleted/moved away by either:
            % - the dropbox code when it is more than 10 hours old; or
            % - the import code after processing.
            ProcessingDir = z_dropbox:dropbox_processing_dir(Context),
            Filename = iolist_to_binary([
                z_string:truncatechars(z_string:to_name(OriginalFilename), 50, <<>>),
                $-,
                z_ids:identifier(10)]),
            ProcessingFile = filename:join([ProcessingDir, Filename]),
            ok = z_filelib:rename(TmpFile, ProcessingFile),
            Context2 = case can_handle(OriginalFilename, ProcessingFile, Context) of
                {ok, Definition} ->
                    handle_spawn(Definition, IsReset, Context),
                    z_render:growl(?__("Please hold on while the file is importing. You will get a notification when it is ready.", Context), Context);
                ok ->
                    z_render:growl(?__("The uploaded file has been imported.", Context), Context);
                {error, _} ->
                    file:delete(ProcessingFile),
                    z_render:growl_error(?__("This file cannot be imported.", Context), Context)
            end,
            z_render:dialog_close(Context2);
        false ->
            z_render:growl_error(?__("Only admins can import CSV files.", Context), Context)
    end.

manage_schema(What, Context) ->
    m_import_csv_data:install(What, Context).


%%====================================================================
%% Internal functions
%%====================================================================

handle_spawn(Def, IsReset, Context) ->
    ContextAsync = z_context:prune_for_async(Context),
    z_proc:spawn_md(
          fun() ->
            import_csv:import(Def, IsReset, ContextAsync),
            to_handled_dir(Def, Context),
            Context1 = z_render:growl_error(?__("The uploaded file has been imported.", Context), Context),
            z_transport:reply_actions(Context1)
          end).


%%====================================================================
%% File handling
%%====================================================================

%% @doc Move the imported file from the processing to the handled dir.
to_handled_dir(Def, Context) ->
    ToDir = z_dropbox:dropbox_handled_dir(Context),
    Target = filename:join([ToDir, filename:basename(Def#filedef.filename)]),
    case filelib:is_regular(Def#filedef.filename) of
        true ->
            file:delete(Target),
            z_filelib:rename(Def#filedef.filename, Target);
        false ->
            ok
    end.


%% @doc Check if we can import this file
can_handle(OriginalFilename, DataFile, Context) ->
    case z_notifier:first(#import_csv_definition{basename=filename:basename(OriginalFilename), filename=DataFile}, Context) of
        {ok, #import_data_def{colsep=ColSep, skip_first_row=SkipFirstRow, columns=Columns, importdef=ImportDef}} ->
            % Column definition of the CSV file, to be handled by our CSV importer.
            {ok, #filedef{
                        filename=DataFile,
                        file_size=filelib:file_size(DataFile),
                        colsep=ColSep,
                        columns=Columns,
                        skip_first_row=SkipFirstRow,
                        importdef=ImportDef
                }};
        ok ->
            % Handled by the notifier - dropbox can move the file to handled.
            ok;
        {error, _} = Error ->
            Error;
        undefined ->
            case inspect_file(DataFile) of
                {ok, #filedef{ columns = Cols } = FD} ->
                    case lists:member(<<"name">>, Cols) andalso lists:member(<<"category">>, Cols) of
                        true ->
                            {ok, FD};
                        false ->
                            ?LOG_WARNING(#{
                                text => <<"Invalid CSV file, missing 'name' and/or 'category' columns">>,
                                in => zotonic_mod_import_csv,
                                result => error,
                                reason => missing_columns,
                                columns => Cols,
                                file => DataFile
                            }),
                            {error, invalid_csv_file}
                    end;
                {error, Reason} = Error ->
                    ?LOG_WARNING(#{
                        text => <<"Invalid CSV file, error during inspect">>,
                        in => zotonic_mod_import_csv,
                        result => error,
                        reason => Reason,
                        file => DataFile
                    }),
                    Error
            end
    end.

%% @doc Inspect the first line of a CSV file, extract the column headers
-spec inspect_file( file:filename_all() ) -> {ok, #filedef{}} | {error, term()}.
inspect_file(Filename) ->
    case z_csv_parser:inspect_file(Filename) of
        {ok, Cols, Sep} ->
            Cols1 = [ to_property_name(Col) || Col <- Cols ],
            {ok, #filedef{
                filename = Filename,
                file_size = filelib:file_size(Filename),
                colsep = Sep,
                columns = Cols1,
                skip_first_row = true,
                importdef = cols2importdef(Cols1)
            }};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% Default import definitions
%%====================================================================

%% @doc Map column names to names that can be handled by the import routines and m_rsc:update/3
cols2importdef(Cols) ->
    ImportDefMap = [ cols2importdef_map(Col) || Col <- unique(Cols,[]) ],
    [
        {
            % Field mapping
            [
             {<<"name">>, {concat, [<<"name_prefix">>, <<"name">>]}}
             | lists:filter(fun(X) ->
                                X =/= undefined
                            end,
                            ImportDefMap)
            ],
            % Edges
            []
        }
    ].

to_property_name(<<"block.", B/binary>>) ->
    <<"blocks.", B/binary>>;
to_property_name(Name) ->
    Name.

unique([], Acc) ->
    lists:reverse(Acc);
unique([C|Cs], Acc) ->
    case lists:member(C, Acc) of
        true -> unique(Cs, Acc);
        false -> unique(Cs, [C|Acc])
    end.

%% @doc Maps well-known column names to an import definition.
cols2importdef_map(<<>>)                    -> undefined;
cols2importdef_map(<<"name">>)              -> undefined;
cols2importdef_map(<<"name_prefix">>)       -> undefined;
cols2importdef_map(<<"date_start">>)        -> {<<"date_start">>, {datetime, <<"date_start">>}};
cols2importdef_map(<<"date_end">>)          -> {<<"date_end">>, {datetime, <<"date_end">>}};
cols2importdef_map(<<"publication_start">>) -> {<<"publication_start">>, {datetime, <<"publication_start">>}};
cols2importdef_map(<<"publication_end">>)   -> {<<"publication_end">>, {datetime, <<"publication_end">>}};
cols2importdef_map(X) -> {X, X}.
