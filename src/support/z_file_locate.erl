%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014 Marc Worrell
%%
%% @doc Locate a file and (if needed) generate a preview. Used by z_file_entry.erl

%% Copyright 2014 Marc Worrell
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

-module(z_file_locate).

-include_lib("zotonic.hrl").
-include_lib("zotonic_file.hrl").
-include_lib("kernel/include/file.hrl").

%%% Used by z_file_entry
-export([
    locate_sources/4
    ]).

%%% Testing
-export([
    locate_file/2,
    locate_lib/2,
    extract_filters/3
    ]).


locate_file(File, Context) ->
    maybe_enoent(locate_sources([], [], [z_convert:to_binary(File)], Context)).

locate_lib(File, Context) ->
    maybe_enoent(locate_sources([lib], [], [z_convert:to_binary(File)], Context)).


locate_sources(Root, ImageFilters, Files, Context) ->
    FilesFilters = [ extract_filters(F, ImageFilters, Context) || F <- Files ],
    [ locate_source(Root, Path, OriginalFile, Filters, Context) || {Path, OriginalFile, Filters} <- FilesFilters ].

maybe_enoent([#part_missing{}]) ->
    {error, enoent};
maybe_enoent(Parts) ->
    Parts.

extract_filters(Path, OptFilters, Context) ->
    case safe_path(Path) of
        undefined ->
            lager:warning("Unsafe path ~p", Path),
            part_missing(Path);
        SafePath ->
            case binary:match(SafePath, <<"(">>) of
                nomatch ->
                    {SafePath, SafePath, OptFilters};
                {_,_} ->
                    PathS = z_convert:to_list(SafePath),
                    {OriginalFile, PreviewPropList, _Checksum, _ChecksumBaseString} = z_media_tag:url2props(PathS, Context),
                    {SafePath, OriginalFile, case OptFilters of undefined -> []; _ -> OptFilters end ++ PreviewPropList}
            end
    end.

% Find all files, possibly starting a preview-request
locate_source(NoRoots, Path, "lib/"++OriginalFile, Filters, Context) when NoRoots =:= undefined; NoRoots =:= [] ->
    locate_source([lib], Path, OriginalFile, Filters, Context);
locate_source(NoRoots, Path, <<"lib/",OriginalFile/binary>>, Filters, Context) when NoRoots =:= undefined; NoRoots =:= [] ->
    locate_source([lib], Path, OriginalFile, Filters, Context);
locate_source(NoRoots, Path, OriginalFile, Filters, Context) when NoRoots =:= undefined; NoRoots =:= [] ->
    case locate_source_uploaded(Path, OriginalFile, Filters, Context) of
        {error, preview_source_gone} ->
            throw(preview_source_gone);
        {error, _} = Error->
            lager:debug("Could not find ~p, error ~p, original ~p", [Path, Error, OriginalFile]),
            #part_missing{file = Path};
        {ok, Loc} ->
            Loc
    end;
locate_source([ModuleIndex|Roots], Path, OriginalFile, Filters, Context) when is_atom(ModuleIndex) ->
    case locate_source_module_indexer(ModuleIndex, Path, OriginalFile, Filters, Context) of
        {ok, Loc} ->
            Loc;
        {error, enoent} ->
            locate_source(Roots, Path, OriginalFile, Filters, Context)
    end;
locate_source([{module, Module} = M|Rs], Path, OriginalFile, Filters, Context) ->
    case Module:file_exists(Path, Context) of
        {true, ModuleDerivedPath} ->
            case part_file(ModuleDerivedPath, [{acl, M}]) of
                {ok, Loc} ->
                    Loc;
                {error, enoent} ->
                    locate_source(Rs, Path, OriginalFile, Filters, Context)
            end;
        false ->
            locate_source(Rs, Path, OriginalFile, Filters, Context)
    end;
locate_source([DirName|Rs], Path, OriginalFile, Filters, Context) ->
    NamePath = make_abs(filename:join([DirName,Path]), Context),
    case part_file(NamePath) of
        {ok, Loc} ->
            Loc;
        {error, enoent} ->
            locate_source(Rs, Path, OriginalFile, Filters, Context)
    end.


%% @doc Source file is located in the lib, template or some other index-category (mostly css, js or static images)
%%      Resized images are located in files/preview.
locate_source_module_indexer(ModuleIndex, Path, _OriginalFile, undefined, Context) ->
    case z_module_indexer:find(ModuleIndex, Path, Context) of
        {ok, #module_index{filepath=FoundFile}} ->
            part_file(FoundFile);
        {error, enoent} ->
            % Try to find ".tpl" version -> render and cache result
            TplFile = <<Path/binary, ".tpl">>,
            case z_module_indexer:find(ModuleIndex, TplFile, Context) of
                {ok, #module_index{} = M} ->
                    render(M, Context);
                {error, _} = Error ->
                    Error
            end
    end;
locate_source_module_indexer(ModuleIndex, Path, OriginalFile, Filters, Context) ->
    case locate_in_filestore(Path, z_path:media_preview(Context), [], Context) of
        {ok, Part} ->
            {ok, Part};
        {error, enoent} ->
            case z_module_indexer:find(ModuleIndex, OriginalFile, Context) of
                {ok, #module_index{filepath=FoundFile}} ->
                    maybe_generate_preview(Path, FoundFile, Filters, [], Context);
                {error, _} = Error ->
                    Error
            end
    end.

%% @doc Locate an uploaded file, stored in the archive.
locate_source_uploaded(<<"preview/", _/binary>> = Path, OriginalFile, Filters, Context) -> 
    locate_source_uploaded_1([], Path, OriginalFile, Filters, Context);
locate_source_uploaded(Path, OriginalFile, Filters, Context) -> 
    case m_media:get_by_filename(OriginalFile, Context) of
        undefined ->
            {error, enoent};
        Medium ->
            locate_source_uploaded_1(Medium, Path, OriginalFile, Filters, Context)
    end.

locate_source_uploaded_1(Medium, _Path, OriginalFile, undefined, Context) ->
    locate_in_filestore(OriginalFile, z_path:media_archive(Context), Medium, Context);
locate_source_uploaded_1(Medium, Path, OriginalFile, Filters, Context) ->
    case locate_in_filestore(Path, z_path:media_preview(Context), Medium, Context) of
        {ok, Part} ->
            {ok, Part};
        {error, enoent} ->
            maybe_generate_preview(Path, OriginalFile, Filters, Medium, Context)
    end.

locate_in_filestore(Path, InDir, Medium, Context) ->
    FSPath = z_convert:to_binary(filename:join(filename:basename(InDir), Path)),
    case z_notifier:first(#filestore{action=lookup, path=FSPath}, Context) of
        {ok, {filezcache, Pid, Opts}} when is_pid(Pid) ->
            {ok, #part_cache{
                cache_pid=Pid,
                cache_monitor=erlang:monitor(process, Pid), 
                modified=proplists:get_value(created, Opts),
                acl=proplists:get_value(id, Medium),
                size=proplists:get_value(size, Opts)
            }};
        {ok, {filename, FoundFilename, Opts}} ->
            part_file(FoundFilename, [{acl,proplists:get_value(id, Medium)}|Opts]);
        {ok, {data, Data, Opts}} when is_list(Opts) ->
            {ok, #part_data{
                data=Data,
                modified=proplists:get_value(modified, Opts),
                acl=proplists:get_value(id, Medium)
            }};
        undefined ->
            part_file(filename:join(InDir, Path))
    end.

part_missing(Filename) ->
    {ok, #part_missing{
        file = Filename
    }}.

part_file(Filename) ->
    part_file(Filename, []).

part_file(Filename, Opts) ->
    case file:read_file_info(Filename) of
        {ok, #file_info{size=Size, type=regular, mtime=MTime}} ->
            {ok, #part_file{
                    size=Size,
                    filepath=z_convert:to_binary(Filename),
                    modified=proplists:get_value(modified, Opts, MTime),
                    mtime=MTime,
                    acl=proplists:get_value(acl, Opts)
            }};
        {ok, #file_info{type=_NotAFile}} ->
            % directories and/or devices don't count as files
            {error, enoent};
        {error, enoent} ->
            {error, enoent};
        {error, _} = Error ->
            Error
    end.

render(ModuleIndex, Context) ->
    #part_data{
        acl=undefined,
        modified=calendar:local_time(),
        data=iolist_to_binary(z_template:render_to_iolist(ModuleIndex, [], Context))
    }.


maybe_generate_preview(Path, OriginalFile, Filters, Medium, Context) ->
    Mime = z_media_identify:guess_mime(OriginalFile),
    generate_preview(z_media_preview:can_generate_preview(Mime), Mime, Path, OriginalFile, Filters, Medium, Context).

generate_preview(true, _Mime, Path, OriginalFile, Filters, Medium, Context) ->
    case fetch_archive(OriginalFile, Context) of
        {ok, Filename} ->
            PreviewDir = z_path:media_preview(Context),
            PreviewFilePath = filename:join(PreviewDir, Path),
            case z_media_preview:convert(z_convert:to_list(Filename), OriginalFile, z_convert:to_list(PreviewFilePath), Filters, Context) of
                ok ->
                    FileStorePath = filename:join([filename:basename(PreviewDir), Path]),
                    z_notifier:first(#filestore{action=upload, path=FileStorePath}, Context),
                    case proplists:get_value(id, Medium) of
                        undefined ->
                            part_file(PreviewFilePath);
                        RscId ->
                            part_file(PreviewFilePath, [{acl,RscId}])
                    end;
                {error, enoent} ->
                    lager:warning("Convert error: input file disappeared, restarting ~p (~p)", [Path, Filename]),
                    {error, preview_source_gone};
                {error, _} = Error ->
                    lager:warning("Convert error: ~p for path ~p", [Error, Path]),
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
generate_preview(false, _Mime, _Path, _OriginalFile, _Filters, _Medium, _Context) ->
    {error, enoent}.

fetch_archive(File, Context) ->
    case locate_in_filestore(File, z_path:media_archive(Context), [], Context) of
        {ok, #part_file{filepath=Filename}} ->
            {ok, Filename};
        {ok, #part_cache{cache_pid=Pid}} ->
            case filezcache:lookup_file(Pid) of
                {ok, {file, _Size, Filename}} -> {ok, Filename};
                {error, _} = Error -> Error
            end;
        {error, _} = Error ->
            Error
    end.


make_abs([$/|_] = Path, _Context) -> Path;
make_abs(<<$/, _/binary>> = Path, _Context) -> Path;
make_abs(Path, Context) -> z_path:files_subdir(Path, Context). 


safe_path(<<$/, P/binary>>) ->
    safe_path(P);
safe_path(P) ->
    PS = z_convert:to_list(P),
    case mochiweb_util:safe_relative_path(PS) of
        undefined ->
            undefined;
        [$/ | SafePath] ->
            list_to_binary(SafePath);
        SafePath ->
            list_to_binary(SafePath)
    end.
