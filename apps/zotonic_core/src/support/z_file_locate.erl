%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2026 Marc Worrell
%% @doc Locate a file and (if needed) generate a preview. Used by z_file_entry.erl
%% @end

%% Copyright 2014-2026 Marc Worrell
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
            ?LOG_WARNING(#{
                text => <<"Unsafe path">>,
                in => zotonic_core,
                result => error,
                path => Path
            }),
            part_missing(Path);
        SafePath ->
            case binary:match(SafePath, <<"(">>) of
                nomatch ->
                    {SafePath, SafePath, OptFilters};
                {_,_} ->
                    case z_media_tag:url2props(SafePath, Context) of
                        {ok, {OriginalFile, PreviewPropList, _Checksum, _ChecksumBaseString}} ->
                            Filters1 = case OptFilters of
                                undefined -> [];
                                _ -> OptFilters
                            end,
                            {SafePath, z_convert:to_binary(OriginalFile), Filters1 ++ PreviewPropList};
                        {error, Reason} ->
                            ?LOG_NOTICE(#{
                                text => <<"Dropping path">>,
                                in => zotonic_core,
                                path => SafePath,
                                result => error,
                                reason => Reason
                            }),
                            part_missing(Path)
                    end
            end
    end.

% Find all files, possibly starting a preview-request
locate_source(NoRoots, Path, <<"lib/",OriginalFile/binary>>, Filters, Context) when NoRoots =:= undefined; NoRoots =:= [] ->
    locate_source([lib], Path, OriginalFile, Filters, Context);
locate_source(NoRoots, Path, OriginalFile, Filters, Context) when NoRoots =:= undefined; NoRoots =:= [] ->
    case locate_source_uploaded(Path, OriginalFile, Filters, Context) of
        {error, preview_source_gone} ->
            throw(preview_source_gone);
        {error, Reason} when Reason =/= enoent, Reason =/= eacces, Reason =/= checksum ->
            throw({file_unavailable, Reason});
        {error, Reason} ->
            ?LOG_DEBUG(#{
                text => <<"Could not find file">>,
                in => zotonic_core,
                path => Path,
                file => OriginalFile,
                result => error,
                reason => Reason
            }),
            #part_missing{file = Path};
        {ok, Loc} ->
            Loc
    end;
locate_source([archive|Roots], Path, OriginalFile, [], Context) ->
    case locate_source_uploaded_1(#{}, Path, OriginalFile, [], Context) of
        {error, Reason} when Reason =/= enoent, Reason =/= eacces, Reason =/= checksum ->
            throw({file_unavailable, Reason});
        {error, Reason} ->
            ?LOG_DEBUG(#{
                text => <<"Could not find file">>,
                in => zotonic_core,
                path => Path,
                file => OriginalFile,
                result => error,
                reason => Reason
            }),
            locate_source(Roots, Path, OriginalFile, [], Context);
        {ok, Loc} ->
            Loc
    end;
locate_source([ModuleIndex|Roots], Path, OriginalFile, Filters, Context) when is_atom(ModuleIndex) ->
    case locate_source_module_indexer(ModuleIndex, Path, OriginalFile, Filters, Context) of
        {ok, Loc} ->
            Loc;
        {error, checksum} ->
            #part_missing{file = Path};
        {error, eacces} ->
            ?LOG_NOTICE(#{
                text => <<"No access to file">>,
                in => zotonic_core,
                result => error,
                reason => eacces,
                path => Path,
                file => OriginalFile
            }),
            locate_source(Roots, Path, OriginalFile, Filters, Context);
        {error, enoent} ->
            locate_source(Roots, Path, OriginalFile, Filters, Context);
        {error, Reason} -> throw({file_unavailable, Reason})
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
locate_source([{id, Id}|Rs], Path, OriginalFile, Filters, Context) ->
    case m_rsc:rid(Id, Context) of
        undefined ->
            locate_source(Rs, Path, OriginalFile, Filters, Context);
        RscId ->
            case m_media:get(RscId, Context) of
                undefined ->
                    locate_source(Rs, Path, OriginalFile, Filters, Context);
                Medium ->
                    case maps:find(<<"filename">>, Medium) of
                        {ok, None} when None =:= undefined; None =:= <<>> ->
                            locate_source(Rs, Path, OriginalFile, Filters, Context);
                        {ok, MediumOriginalFile} ->
                            case locate_source_uploaded_1(Medium, Path, MediumOriginalFile, Filters, Context) of
                                {ok, Part} ->
                                    Part;
                                {error, _} ->
                                    locate_source(Rs, Path, OriginalFile, Filters, Context)
                            end;
                        error ->
                            locate_source(Rs, Path, OriginalFile, Filters, Context)
                    end
            end
    end;
locate_source([DirName|Rs], Path, OriginalFile, Filters, Context) ->
    NamePath = make_abs(filename:join([DirName,Path]), Context),
    case part_file(NamePath, []) of
        {ok, Loc} ->
            Loc;
        {error, enoent} ->
            locate_source(Rs, Path, OriginalFile, Filters, Context)
    end.


%% @doc Source file is located in the lib, template or some other index-category (mostly css, js or static images)
%%      Resized images are located in files/preview.
locate_source_module_indexer(lib, Path, OriginalFile, [], Context) ->
    locate_source_module_indexer(lib, Path, OriginalFile, undefined, Context);
locate_source_module_indexer(ModuleIndex, _Path, OriginalFile, undefined, Context) ->
    case z_module_indexer:find(ModuleIndex, OriginalFile, Context) of
        {ok, #module_index{filepath=FoundFile}} ->
            part_file(FoundFile, []);
        {error, enoent} ->
            % Try to find ".tpl" version -> render and cache result
            TplFile = <<OriginalFile/binary, ".tpl">>,
            case z_module_indexer:find(ModuleIndex, TplFile, Context) of
                {ok, #module_index{} = M} ->
                    {ok, render(M, Context)};
                {error, _} = Error ->
                    Error
            end
    end;
locate_source_module_indexer(ModuleIndex, Path, OriginalFile, Filters, Context) ->
    case m_cafs:is_enabled(Context) of
        false -> locate_local_preview(ModuleIndex, Path, OriginalFile, Filters, Context);
        true -> locate_module_preview(ModuleIndex, Path, OriginalFile, Filters, Context)
    end.

%% Database-free sites have only occasional library previews. Keep these local,
%% without archive lookup, filestore notifications, CAFS metadata or collection.
locate_local_preview(ModuleIndex, Path, OriginalFile, Filters, Context) ->
    Preview = filename:join(z_path:media_preview(Context), Path),
    case part_file(Preview, []) of
        {error, enoent} ->
            case z_module_indexer:find(ModuleIndex, OriginalFile, Context) of
                {ok, #module_index{filepath=Source}} ->
                    case z_media_preview:convert(z_convert:to_list(Source),
                            z_convert:to_list(Preview), Filters, Context) of
                        ok -> part_file(Preview, []);
                        Error -> Error
                    end;
                Error -> Error
            end;
        Result -> Result
    end.

locate_module_preview(ModuleIndex, Path, OriginalFile, Filters, Context) ->
    case locate_in_filestore(Path, z_path:media_preview(Context), true, #{}, Context) of
        {ok, Part} ->
            {ok, Part};
        {error, enoent} ->
            case z_module_indexer:find(ModuleIndex, OriginalFile, Context) of
                {ok, #module_index{filepath=FoundFile}} ->
                    maybe_generate_preview(Path, FoundFile, Filters, #{}, Context);
                {error, _} = Error ->
                    Error
            end
    end.

%% @doc Locate an uploaded file, stored in the archive.
locate_source_uploaded(<<"preview/", _/binary>> = Path, OriginalFile, Filters, Context) ->
    case z_db:q1("select id from medium where preview_filename=$1", [OriginalFile], Context) of
        undefined -> {error, enoent};
        Id -> locate_source_uploaded_1(m_media:get(Id, Context), Path, OriginalFile, Filters, Context)
    end;
locate_source_uploaded(Path, OriginalFile, Filters, Context) ->
    case m_media:get_by_filename(OriginalFile, Context) of
        undefined ->
            {error, enoent};
        Medium ->
            locate_source_uploaded_1(Medium, Path, OriginalFile, Filters, Context)
    end.

locate_source_uploaded_1(Medium, _Path, OriginalFile, undefined, Context) ->
    locate_in_filestore(OriginalFile, z_path:media_archive(Context), false, Medium, Context);
locate_source_uploaded_1(Medium, _Path, OriginalFile, [], Context) ->
    locate_in_filestore(OriginalFile, z_path:media_archive(Context), false, Medium, Context);
locate_source_uploaded_1(Medium, Path, OriginalFile, Filters, Context) ->
    case z_cafs_preview:prepare(Path, OriginalFile, Filters, Medium, Context) of
        {ok, Usage} ->
            case z_cafs_preview:lookup(Usage, Context) of
                {ok, Hash} -> locate_cafs_output(Hash, Path, Medium, Context);
                {error, enoent} ->
                    locate_source_preview(Medium#{cafs_preview_usage => Usage}, Path,
                        OriginalFile, Filters, Context)
            end;
        {error, enoent} = Error -> Error
    end.

locate_source_preview(Medium, Path, OriginalFile, Filters, Context) ->
    case locate_in_filestore(Path, z_path:media_preview(Context), true, Medium, Context) of
        {ok, Part} ->
            {ok, Part};
        {error, enoent} ->
            maybe_generate_preview(Path, OriginalFile, Filters, Medium, Context)
    end.

locate_in_filestore(Path, InDir, IsPreview, Medium, Context) ->
    LegacyPath = z_convert:to_binary(filename:join(filename:basename(InDir), Path)),
    BaseDirPath = case IsPreview of
        false ->
            case m_cafs:lookup(Path, Context) of
                {ok, Hash} -> m_cafs:path(Hash);
                {error, enoent} -> LegacyPath
            end;
        true -> LegacyPath
    end,
    ok = m_cafs:track(BaseDirPath, Context),
    LocalPath = z_path:files_subdir(BaseDirPath, Context),
    OptRscId = maps:get(<<"id">>, Medium, undefined),
    OptMime = case IsPreview of
        true ->
            z_media_identify:guess_mime(Path);
        false ->
            case maps:get(<<"preview_filename">>, Medium, undefined) of
                Path -> z_media_identify:guess_mime(Path);
                _ -> maps:get(<<"mime">>, Medium, undefined)
            end
    end,
    case z_notifier:first(#filestore{action=lookup, path=BaseDirPath, local_path=LocalPath}, Context) of
        {ok, {filezcache, Pid, #{ created := Created, size := Size }}} when is_pid(Pid) ->
            {ok, #part_cache{
                cache_pid=Pid,
                cache_monitor = erlang:monitor(process, Pid),
                modified = Created,
                acl = OptRscId,
                size = Size,
                mime = OptMime
            }};
        {ok, {filename, FoundFilename, #{ modified := Modified }}} ->
            part_file(FoundFilename, [ {acl,OptRscId}, {modified, Modified}, {mime, OptMime} ]);
        {ok, {data, Data, #{ modified := Modified }}} ->
            {ok, #part_data{
                data = Data,
                modified = Modified,
                acl = OptRscId,
                mime = OptMime
            }};
        undefined ->
            part_file(LocalPath, [{acl,OptRscId}, {mime, OptMime}])
    end.

part_missing(Filename) ->
    {ok, #part_missing{
        file = Filename
    }}.

part_file(Filename, Opts) ->
    case file:read_file_info(Filename, [raw, {time, universal}]) of
        {ok, #file_info{access = none}} ->
            % No access
            {error, eacces};
        {ok, #file_info{access = write}} ->
            % Only write access
            {error, eacces};
        {ok, #file_info{size=Size, type=regular, mtime=MTime}} ->
            {ok, #part_file{
                    size = Size,
                    filepath = z_convert:to_binary(Filename),
                    modified = proplists:get_value(modified, Opts, MTime),
                    mtime = MTime,
                    acl = proplists:get_value(acl, Opts),
                    mime = proplists:get_value(mime, Opts)
            }};
        {ok, #file_info{type=_NotAFile}} ->
            % directories and/or devices don't count as files
            {error, enoent};
        {error, enoent} ->
            {error, enoent};
        {error, _} = Error ->
            Error
    end.

-spec render(#module_index{}, #context{}) -> #part_data{}.
render(ModuleIndex, Context) ->
    {Data, _RenderState} = z_template:render_to_iolist(ModuleIndex, [], Context),
    #part_data{
        acl=undefined,
        modified=calendar:local_time(),
        data=iolist_to_binary(Data)
    }.


maybe_generate_preview(Path, OriginalFile, Filters, Medium, Context) ->
    Mime = z_media_identify:guess_mime(OriginalFile),
    generate_preview(z_media_preview:can_generate_preview(Mime), Path, OriginalFile, Filters, Medium, Context).

generate_preview(true, Path, OriginalFile, Filters, #{ <<"id">> := _ } = Medium, Context) ->
    case m_cafs:lookup(OriginalFile, Context) of
        {ok, Hash} -> shared_preview(Hash, Path, OriginalFile, Filters, Medium, Context);
        {error, enoent} ->
            case fetch_archive(OriginalFile, Context) of
                {ok, File} ->
                    case m_cafs:promote(OriginalFile, File, Context) of
                        {ok, Hash} -> shared_preview(Hash, Path, OriginalFile, Filters, Medium, Context);
                        Error -> Error
                    end;
                Error -> Error
            end
    end;
generate_preview(true, Path, OriginalFile, Filters, Medium, Context) ->
    case fetch_archive(OriginalFile, Context) of
        {ok, Filename} ->
            PreviewDir = z_path:media_preview(Context),
            PreviewFilePath = filename:join(PreviewDir, Path),
            case z_media_preview:convert(z_convert:to_list(Filename), OriginalFile, z_convert:to_list(PreviewFilePath), Filters, Context) of
                ok ->
                    FileStorePath = z_convert:to_binary(filename:join([filename:basename(PreviewDir), Path])),
                    z_notifier:first(#filestore{action=upload, path=FileStorePath}, Context),
                    case maps:get(<<"id">>, Medium, undefined) of
                        undefined ->
                            part_file(PreviewFilePath, []);
                        RscId ->
                            part_file(PreviewFilePath, [{acl,RscId}])
                    end;
                {error, enoent} ->
                    ?LOG_WARNING(#{
                        text => <<"Convert error: input file disappeared, restarting file entry">>,
                        in => zotonic_core,
                        result => error,
                        reason => enoent,
                        path => Path,
                        filename => Filename
                    }),
                    {error, preview_source_gone};
                {error, Reason} = Error ->
                    ?LOG_WARNING(#{
                        text => <<"Convert error">>,
                        in => zotonic_core,
                        result => error,
                        reason => Reason,
                        path => Path
                    }),
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
generate_preview(false, _Path, _OriginalFile, _Filters, _Medium, _Context) ->
    {error, enoent}.


%% Key by effective ordered operations, not by the requesting filename. The
%% extension controls the output encoder and must be included in the identity.
shared_preview(Hash, Path, OriginalFile, Filters, Medium, Context) ->
    %% Avoid fetching an original just to read metadata on an ordinary cache hit.
    Identify = case m_media:identify(OriginalFile, Context) of
        {ok, _} = Found -> Found;
        {error, _} ->
            m_cafs:with_local_file(Hash, fun(Input) ->
                z_media_identify:identify_file(Input, OriginalFile, Context)
            end, Context)
    end,
    case Identify of
        {ok, Props} ->
            shared_preview_identified(Hash, Path, Filters, Medium,
                maps:remove(<<"id">>, Props), Context);
        Error -> Error
    end.

shared_preview_identified(Hash, Path, Filters, Medium, FileProps, Context) ->
    RenderFilters = proplists:delete(editrev, Filters),
    case z_mediaclass:expand_mediaclass_checksum(RenderFilters) of
        {ok, Expanded} ->
            Extension = filename:extension(Path),
            Version = {1, z_media_imagemagick:selected(),
                z_config:get(default_colorspace, "sRGB"),
                m_config:get_value(site, cafs_renderer_version, <<"1">>, Context)},
            Generate = fun() ->
                m_cafs:with_local_file(Hash, fun(Input) ->
                    Tmp = z_tempfile:new(z_convert:to_list(Extension)),
                    Output = Tmp,
                    try
                        case z_media_preview:convert_with_metadata(z_convert:to_list(Input), Output,
                                Expanded, FileProps, Context) of
                            ok -> m_cafs:stage_file(Output, Context);
                            Error -> Error
                        end
                    after
                        file:delete(Output),
                        file:delete(Tmp)
                    end
                end, Context)
            end,
            Usage = maps:get(cafs_preview_usage, Medium),
            case m_cafs:derivative(Hash, {Extension, Expanded, FileProps}, Version, Usage, Generate, Context) of
                {ok, OutputHash} ->
                    %% Direct object lookup retains this alias's ACL and MIME.
                    locate_cafs_output(OutputHash, Path, Medium, Context);
                Error -> Error
            end;
        Error -> Error
    end.

locate_cafs_output(Hash, Path, Medium, Context) ->
    Physical = m_cafs:path(Hash),
    ok = m_cafs:track(Physical, Context),
    Local = z_path:files_subdir(Physical, Context),
    Id = maps:get(<<"id">>, Medium),
    Mime = z_media_identify:guess_mime(Path),
    case z_notifier:first(#filestore{action=lookup, path=Physical, local_path=Local}, Context) of
        undefined -> part_file(Local, [{acl, Id}, {mime, Mime}]);
        {ok, {filename, File, #{modified := Modified}}} ->
            part_file(File, [{acl, Id}, {mime, Mime}, {modified, Modified}]);
        {ok, {filezcache, Pid, #{created := Created, size := Size}}} ->
            {ok, #part_cache{cache_pid=Pid, cache_monitor=erlang:monitor(process, Pid),
                modified=Created, acl=Id, size=Size, mime=Mime}};
        Error -> Error
    end.


fetch_archive(File, Context) ->
    case locate_in_filestore(File, z_path:media_archive(Context), false, #{}, Context) of
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
    % TODO: replace with binary version
    PS = z_convert:to_list(P),
    case mochiweb_util:safe_relative_path(PS) of
        undefined ->
            undefined;
        [$/ | SafePath] ->
            list_to_binary(SafePath);
        SafePath ->
            list_to_binary(SafePath)
    end.
