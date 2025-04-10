%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2025 Marc Worrell
%% @doc Media archiving utilities.  Manages the files/archive directory of sites.
%% @end

%% Copyright 2009-2025 Marc Worrell
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

-module(z_media_archive).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    abspath/2,
    ensure_relative/2,
    ensure_relative/3,
    is_archived/2,
    archive_file/2,
    archive_file/3,
    archive_copy/2,
    archive_copy/3,
    archive_copy/4,
    archive_copy_opt/2,
    archive_copy_opt/3,
    archive_delete/2,
    archive_filename/2,
    preview_filename/2,
    rel_archive/2,
    safe_filename/1
]).

-include("../../include/zotonic.hrl").

%% @doc Return the absolute path of a file in the archive. The given filename
%% must be relative to the archive directory.
-spec abspath(File, Context) -> file:filename_all() when
    File :: file:filename_all(),
    Context :: z:context().
abspath(File, Context) ->
    filename:join([z_path:media_archive(Context), File]).

%% @doc Ensure that the filename is relative to the archive. If needed then move
%% the file to the archive. Return the path of the original or moved file relative
%% to the archive.
-spec ensure_relative(File, Context) -> file:filename_all() when
    File :: file:filename_all(),
    Context :: z:context().
ensure_relative(File, Context) ->
    ensure_relative(File, filename:basename(File), Context).

%% @doc Ensure that the file is in the archive, when not then generate a new filename
%% and move the file to the archive. If the file is already in the archive, then do
%% nothing. Return the filename relative to the archive directory.
-spec ensure_relative(File, NewBasenameIfMoved, Context) -> file:filename_all() when
    File :: file:filename_all(),
    NewBasenameIfMoved :: string() | binary(),
    Context :: z:context().
ensure_relative(File, NewBasenameIfMoved, Context) ->
    Fileabs = filename:absname(File),
    case is_archived(Fileabs, Context) of
        true ->
            rel_archive(Fileabs, Context);
        false ->
            % Not in the archive dir, move the file
            archive_file(Fileabs, NewBasenameIfMoved, Context)
    end.

%% @doc Move a file to the archive directory (when it is not archived yet)
-spec archive_file(Filename, Context) -> ArchivedFilename when
    Filename :: file:filename_all(),
    Context :: z:context(),
    ArchivedFilename :: file:filename_all().
archive_file(Filename, Context) ->
    archive_file(Filename, filename:basename(Filename), Context).

%% @doc Move a file to the archive directory (when it is not archived yet)
-spec archive_file(Filename, NewBasename, Context) -> ArchivedFilename when
    Filename :: file:filename_all(),
    NewBasename :: string() | binary(),
    Context :: z:context(),
    ArchivedFilename :: file:filename_all().
archive_file(Filename, NewBasename, Context) ->
    Fileabs = filename:absname(Filename),
    case is_archived(Fileabs, Context) of
        true ->
            rel_archive(Fileabs, Context);
        false ->
            NewFile = archive_filename(NewBasename, Context),
            AbsPath = abspath(NewFile, Context),
            ok = z_filelib:ensure_dir(AbsPath),
            case file:rename(Fileabs, AbsPath) of
                %% cross-fs rename is not supported by erlang, so copy and delete the file
                {error, exdev} ->
                    {ok, _BytesCopied} = file:copy(Fileabs, AbsPath),
                    ok = file:delete(Fileabs);
                ok ->
                    ok
            end,
            NewFile
    end.

%% @doc Always archive a copy of a file in the archive directory.
-spec archive_copy(Filename, Context) -> ArchivedFilename when
    Filename :: file:filename_all(),
    Context :: z:context(),
    ArchivedFilename :: file:filename_all().
archive_copy(Filename, Context) ->
    archive_copy(Filename, filename:basename(Filename), Context).

%% @doc Always archive a copy of a file in the archive directory.
-spec archive_copy(Filename, NewBasename, Context) -> ArchivedFilename when
    Filename :: file:filename_all(),
    NewBasename :: string() | binary(),
    Context :: z:context(),
    ArchivedFilename :: file:filename_all().
archive_copy(Filename, NewBasename, Context) ->
    archive_copy(archive, Filename, NewBasename, Context).

%% @doc Always archive a copy of a file in the archive directory.
-spec archive_copy(Type, Filename, NewBasename, Context) -> ArchivedFilename when
    Type :: archive | preview,
    Filename :: file:filename_all(),
    NewBasename :: string() | binary(),
    Context :: z:context(),
    ArchivedFilename :: file:filename_all().
archive_copy(archive, Filename, NewBasename, Context) ->
    NewFile = archive_filename(NewBasename, Context),
    archive_copy_1(Filename, NewFile, Context);
archive_copy(preview, Filename, NewBasename, Context) ->
    NewFile = preview_filename(NewBasename, Context),
    archive_copy_1(Filename, NewFile, Context).

archive_copy_1(Filename, NewFile, Context) ->
    Fileabs = filename:absname(Filename),
    AbsPath = abspath(NewFile, Context),
    ok = z_filelib:ensure_dir(AbsPath),
    {ok, _Bytes} = file:copy(Fileabs, AbsPath),
    NewFile.


%% @doc Optionally archive a copy of a file in the archive directory (when it is not archived yet)
-spec archive_copy_opt(Filename, Context) -> ArchivedFilename when
    Filename :: file:filename_all(),
    Context :: z:context(),
    ArchivedFilename :: file:filename_all().
archive_copy_opt(Filename, Context) ->
    archive_copy_opt(Filename, filename:basename(Filename), Context).

%% @doc Optionally archive a copy of a file in the archive directory (when it is not archived yet).
-spec archive_copy_opt(Filename, NewBasename, Context) -> ArchivedFilename when
    Filename :: file:filename_all(),
    NewBasename :: string() | binary(),
    Context :: z:context(),
    ArchivedFilename :: file:filename_all().
archive_copy_opt(Filename, NewBasename, Context) ->
    Fileabs = filename:absname(Filename),
    case is_archived(Fileabs, Context) of
        false ->
            NewFile = archive_filename(NewBasename, Context),
            AbsPath = abspath(NewFile, Context),
            ok = z_filelib:ensure_dir(AbsPath),
            {ok, _Bytes} = file:copy(Fileabs, AbsPath),
            NewFile;
        true ->
            rel_archive(Fileabs, Context)
    end.

%% @doc Delete a filename in the archive. The Filename is relative to the
%% base archive directory.
-spec archive_delete(Filename, Context) -> ok | {error, Reason} when
    Filename :: file:filename_all(),
    Context :: z:context(),
    Reason :: file:posix() | badarg.
archive_delete(Filename, Context) ->
    AbsPath = abspath(Filename, Context),
    file:delete(AbsPath).


%% @doc Return an unique filename for archiving the file. Used when storing a
%% new file after upload.
-spec archive_filename(Filename, Context) -> file:filename_all() when
    Filename :: file:filename_all(),
    Context :: z:context().
archive_filename(Filename, Context) ->
    {{Y,M,D}, _} = z_datetime:to_local(calendar:universal_time(), Context),
    Rootname = filename:rootname(filename:basename(Filename)),
    Extension = filename:extension(Filename),
    RelRoot = filename:join([
        integer_to_list(Y), integer_to_list(M), integer_to_list(D),
        safe_filename(Rootname)
    ]),
    make_unique(RelRoot, z_convert:to_binary(Extension), Context).

%% @doc Return an unique filename for archiving a preview of a file. Used for
%% storing a new file after preview generation.
-spec preview_filename(Filename, Context) -> file:filename_all() when
    Filename :: file:filename_all(),
    Context :: z:context().
preview_filename(Filename, Context) ->
    Rootname = filename:rootname(filename:basename(Filename)),
    Extension = filename:extension(Filename),
    RelRoot = filename:join([
                    "preview",
                    z_ids:identifier(2),
                    z_ids:identifier(2),
                    safe_filename(Rootname)]),
    make_unique(RelRoot, z_convert:to_list(Extension), Context).


safe_filename(<<$.,Rest/binary>>) ->
    safe_filename(<<$_, Rest/binary>>);
safe_filename(B) when is_binary(B) ->
    AsName = z_convert:to_binary(z_string:to_name(B)),
    safe_filename_1(AsName, <<>>);
safe_filename(L) when is_list(L) ->
    safe_filename(iolist_to_binary(L)).

safe_filename_1(<<>>, Acc) ->
    Acc;
safe_filename_1(<<C/utf8, Rest/binary>>, Acc)
    when (C >= $a andalso C =< $z)
        orelse (C >= $0 andalso C =< $9)
        orelse C == $. orelse C == $- orelse C == $_ ->
    safe_filename_1(Rest, <<Acc/binary,C>>);
safe_filename_1(<<_/utf8, Rest/binary>>, Acc) ->
    safe_filename_1(Rest, <<Acc/binary, $_>>).


%% @doc Make sure that the filename is unique by appending a number.
%% The number is always appended, to prevent problems with parallel
%% insertion of the like-named files.
make_unique(Rootname, Extension, Context) ->
    Nr = z_convert:to_binary(z_ids:number()),
    File = iolist_to_binary([Rootname, $-, Nr, Extension]),
    case m_media:is_unique_file(File, Context) of
        true ->
            File;
        false ->
            make_unique(Rootname, Extension, Context)
    end.

%% @doc Check if the file is in the archive directory.
-spec is_archived(Filename, Context) -> boolean() when
    Filename :: file:filename_all() | undefined,
    Context :: z:context().
is_archived(undefined, _Context) ->
    false;
is_archived(Filename, Context) ->
    Fileabs = z_convert:to_binary(Filename),
    PathToArchive = z_convert:to_binary([ z_path:media_archive(Context), $/ ]),
    ArchiveLen = size(PathToArchive),
    ArchiveLen == binary:longest_common_prefix([ Fileabs, PathToArchive ]).

%% @doc Remove the path to the archive directory, return a filename relative to the archive directory
%% Crash if the path is not in relative to the archive directory.
rel_archive(Filename, Context) ->
    Fileabs = z_convert:to_binary(filename:absname(Filename)),
    PathToArchive = z_convert:to_binary(z_path:media_archive(Context)),
    ArchiveLen = size(PathToArchive),
    <<Path:ArchiveLen/binary, $/, Relpath>> = Fileabs,
    Path = PathToArchive,  % Crash if not in archive directory
    Relpath.
