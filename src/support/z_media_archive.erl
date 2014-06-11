%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-10
%% @doc Media archiving utilities.  Manages the files/archive directory of sites.

%% Copyright 2009 Marc Worrell
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
    archive_copy_opt/2,
    archive_copy_opt/3,
    archive_delete/2,
    archive_filename/2,
    rel_archive/2,
    safe_filename/1
]).

-include_lib("zotonic.hrl").

%% @doc Return the absolute path name of a relative file in the archive
abspath(File, Context) ->
    filename:join([z_path:media_archive(Context), z_convert:to_list(File)]).

%% @doc Ensure that the filename is relative to the archive.  When needed move the file to the archive.  Return the relative path.
ensure_relative(File, Context) ->
    ensure_relative(File, filename:basename(File), Context).

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
%% @spec archive_file(Filename, Context) -> ArchivedFilename
archive_file(Filename, Context) ->
    archive_file(Filename, filename:basename(Filename), Context).

%% @doc Move a file to the archive directory (when it is not archived yet)
%% @spec archive_file(Filename, NewBasename, Context) -> ArchivedFilename
archive_file(Filename, NewBasename, Context) ->
    Fileabs = filename:absname(Filename),
    case is_archived(Fileabs, Context) of
        true ->
            rel_archive(Fileabs, Context);
        false ->
            NewFile = archive_filename(NewBasename, Context),
            AbsPath = abspath(NewFile, Context),
            ok = filelib:ensure_dir(AbsPath),
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

%% @doc Always archive a copy of a file in the archive directory
%% @spec archive_copy(Filename, Context) -> ArchivedFilename
archive_copy(Filename, Context) ->
    archive_copy(Filename, filename:basename(Filename), Context).

archive_copy(Filename, NewBasename, Context) ->
    Fileabs = filename:absname(Filename),
    NewFile = archive_filename(NewBasename, Context),
    AbsPath = abspath(NewFile, Context),
    ok = filelib:ensure_dir(AbsPath),
    {ok, _Bytes} = file:copy(Fileabs, AbsPath),
    NewFile.

%% @doc Optionally archive a copy of a file in the archive directory (when it is not archived yet)
%% @spec archive_copy_opt(Filename, Context) -> ArchivedFilename
archive_copy_opt(Filename, Context) ->
    archive_copy_opt(Filename, filename:basename(Filename), Context).

archive_copy_opt(Filename, NewBasename, Context) ->
    Fileabs = filename:absname(Filename),
    case is_archived(Fileabs, Context) of
        false ->
            NewFile = archive_filename(NewBasename, Context),
            AbsPath = abspath(NewFile, Context),
            ok = filelib:ensure_dir(AbsPath),
            {ok, _Bytes} = file:copy(Fileabs, AbsPath),
            NewFile;
        true ->
            rel_archive(Fileabs, Context)
    end.

%% @doc Delete a filename in the archive
archive_delete(Filename, Context) ->
    AbsPath = abspath(Filename, Context),
    file:delete(AbsPath). 


%% Return an unique filename for archiving the file
archive_filename(Filename, Context) ->
    {{Y,M,D}, _} = z_datetime:to_local(calendar:universal_time(), Context),
    Rootname = filename:rootname(filename:basename(Filename)),
    Extension = filename:extension(Filename),
    RelRoot = filename:join([integer_to_list(Y),integer_to_list(M),integer_to_list(D),safe_filename(Rootname)]),
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
    

%% @doc Make sure that the filename is unique by appending a number on filename clashes
make_unique(Rootname, Extension, Context) ->
    File = iolist_to_binary([Rootname, Extension]),
    case m_media:is_unique_file(File, Context) of
        true ->
            File;
        false -> 
            make_unique(Rootname, Extension, z_ids:number(), Context)
    end.

make_unique(Rootname, Extension, Nr, Context) ->
    File = iolist_to_binary([Rootname, $-, integer_to_list(Nr), Extension]),
    case m_media:is_unique_file(File, Context) of
        true ->
            File;
        false -> 
            make_unique(Rootname, Extension, z_ids:number(), Context)
    end.


%% @doc Check if the file is archived (ie. in the archive directory)
is_archived(undefined, _Context) ->
    false;
is_archived(Filename, Context) ->
    Fileabs = z_convert:to_list(filename:absname(Filename)),
    Archive = z_convert:to_list(z_path:media_archive(Context)) ++ "/",
    lists:prefix(Archive, Fileabs).
    

%% @doc Remove the path to the archive directory, return a filename relative to the archive directory
rel_archive(Filename, Context) ->
    Fileabs = z_convert:to_list(filename:absname(Filename)),
    Archive = z_convert:to_list(z_path:media_archive(Context)) ++ "/",
    true = lists:prefix(Archive, Fileabs),
    lists:nthtail(length(Archive), Fileabs).
	
