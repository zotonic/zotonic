%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2010 Marc Worrell
%%
%% @doc Serve static (image) files from a configured list of directories or template lookup keys.  Caches files in the local depcache.
%% Is also able to generate previews (if configured to do so).

%% Copyright 2009-2010 Marc Worrell
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

%% Serves files like:
%% 
%% /image/2007/03/31/wedding.jpg(300x300)(crop-center)(709a-a3ab6605e5c8ce801ac77eb76289ac12).jpg
%% /media/inline/<filepath>
%% /media/attachment/<filepath>

-module(resource_file_readonly).
-export([
    init/1,
    service_available/2,
    allowed_methods/2,
    resource_exists/2,
    forbidden/2,
    last_modified/2,
    expires/2,
    content_types_provided/2,
    charsets_provided/2,
    encodings_provided/2,
    provide_content/2,
    finish_request/2
]).

-include_lib("webmachine_resource.hrl").
-include_lib("zotonic.hrl").

-record(cache, {path, fullpath, mime, last_modified, body}).

-define(MAX_AGE, 315360000).
-define(CHUNKED_CONTENT_LENGTH, 1048576).
-define(CHUNK_LENGTH, 65536).

init(ConfigProps) ->
    {ok, ConfigProps}.


%% @doc Initialize the context for the request. Continue session when available.
service_available(ReqData, ConfigProps) ->
    Context = z_context:set(ConfigProps, z_context:new(ReqData)),
    Context1 = z_context:ensure_qs(z_context:continue_session(Context)),
    {_, ReqData1, ContextFile} = ensure_file_info(ReqData, Context1),
    ContextMime = case z_context:get(mime, ContextFile) of
        undefined ->
            Path = case z_context:get(path, Context) of
                undefined -> mochiweb_util:unquote(wrq:disp_path(ReqData1));
                ConfiguredPath -> ConfiguredPath
            end, 
            CT = z_media_identify:guess_mime(Path),
            z_context:set(mime, CT, ContextFile);
        _Mime -> 
            ContextFile
    end,
    
    case filelib:file_size(z_context:get(fullpath, ContextMime)) of
        N when N > ?CHUNKED_CONTENT_LENGTH ->
            ContextChunked = z_context:set([{chunked, true}, {file_size, N}], ContextMime), 
            ?WM_REPLY(true, ContextChunked);
        _ ->
            ?WM_REPLY(true, ContextMime)
    end.


allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{z_context:get(mime, Context), provide_content}], ReqData, Context}.


%% @doc Oversimplified stub for access checks.
forbidden(ReqData, Context) ->
    case z_context:get(id, Context) of
        undefined ->
            case z_context:get(root, Context) of
                [{module, Module}] -> 
                    {Module:file_forbidden(z_context:get(fullpath, Context), Context), ReqData, Context};
                _ ->
                    {false, ReqData, Context}
            end;
        RscId ->
            {not z_acl:rsc_visible(RscId, Context), ReqData, Context}
    end.


encodings_provided(ReqData, Context) ->
    Encodings = case z_context:get(chunked, Context) of
        true ->
            [{"identity", fun(Data) -> Data end}];
        _ ->
            case z_context:get(mime, Context) of
                "image/" ++ _ ->  [{"identity", fun(Data) -> Data end}];
                "video/" ++ _ ->  [{"identity", fun(Data) -> Data end}];
                "audio/" ++ _ ->  [{"identity", fun(Data) -> Data end}];
                "application/x-gzip" ++ _ -> [{"identity", fun(Data) -> Data end}];
                "applicationzip" ++ _ -> [{"identity", fun(Data) -> Data end}];
                _ -> 
                    [{"identity", fun(Data) -> decode_data(identity, Data) end},
                     {"gzip",     fun(Data) -> decode_data(gzip, Data) end}]
            end
    end,
    {Encodings, ReqData, z_context:set(encode_data, length(Encodings) > 1, Context)}.


resource_exists(ReqData, Context) ->
    {z_context:get(fullpath, Context) =/= undefined, ReqData, Context}.

charsets_provided(ReqData, Context) ->
    case is_text(z_context:get(mime, Context)) of
        true -> {[{"utf-8", fun(X) -> X end}], ReqData, Context};
        _ -> {no_charset, ReqData, Context}
    end.
    
last_modified(ReqData, Context) ->
    RD1 = wrq:set_resp_header("Cache-Control", "public, max-age="++integer_to_list(?MAX_AGE), ReqData),
    case z_context:get(last_modified, Context) of
        undefined -> 
            LMod = filelib:last_modified(z_context:get(fullpath, Context)),
			[LModUTC|_] = calendar:local_time_to_universal_time_dst(LMod),
            {LModUTC, RD1, z_context:set(last_modified, LModUTC, Context)};
        LModUTC ->
            {LModUTC, RD1, Context}
    end.

expires(ReqData, Context) ->
    NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    {calendar:gregorian_seconds_to_datetime(NowSecs + ?MAX_AGE), ReqData, Context}.

provide_content(ReqData, Context) ->
    RD1 = case z_context:get(content_disposition, Context) of
        inline ->     wrq:set_resp_header("Content-Disposition", "inline", ReqData);
        attachment -> wrq:set_resp_header("Content-Disposition", "attachment", ReqData);
        undefined ->  ReqData
    end,
    case z_context:get(body, Context) of
        undefined ->
            case z_context:get(chunked, Context) of
                true ->
                    {ok, Device} = file:open(z_context:get(fullpath, Context), [read,raw,binary]),
                    FileSize = z_context:get(file_size, Context),
                    {   {stream, read_chunk(0, FileSize, Device)}, 
                        wrq:set_resp_header("Content-Length", integer_to_list(FileSize), ReqData),
                        z_context:set(use_cache, false, Context) };
                _ ->
                    {ok, Data} = file:read_file(z_context:get(fullpath, Context)),
                    Body = case z_context:get(encode_data, Context, false) of 
                        true -> encode_data(Data);
                        false -> Data
                    end,
                    {Body, RD1, z_context:set(body, Body, Context)}
            end;
        Body -> 
            {Body, RD1, Context}
    end.
    
    
    read_chunk(Offset, Size, Device) when Offset =:= Size ->
        file:close(Device),
        {<<>>, done};
    read_chunk(Offset, Size, Device) when Size - Offset =< ?CHUNK_LENGTH ->
        {ok, Data} = file:read(Device, Size - Offset),
        file:close(Device),
        {Data, done};
    read_chunk(Offset, Size, Device) ->
        {ok, Data} = file:read(Device, ?CHUNK_LENGTH),
        {Data, fun() -> read_chunk(Offset+?CHUNK_LENGTH, Size, Device) end}.


finish_request(ReqData, Context) ->
    case z_context:get(is_cached, Context) of
        false ->
            case z_context:get(body, Context) of
                undefined ->  
                    {ok, ReqData, Context};
                Body ->
                    case z_context:get(use_cache, Context, false) andalso z_context:get(encode_data, Context, false) of
                        true ->
                            % Cache the served file in the depcache.  Cache it for 3600 secs.
                            Path = z_context:get(path, Context),
                            Cache = #cache{
                                        path=Path,
                                        fullpath=z_context:get(fullpath, Context),
                                        mime=z_context:get(mime, Context),
                                        last_modified=z_context:get(last_modified, Context),
                                        body=Body
                                    },
                            z_depcache:set(cache_key(Path), Cache, Context),
                            {ok, ReqData, Context};
                        _ ->
                            % No cache or no gzip'ed version (file system cache is fast enough for image serving)
                            {ok, ReqData, Context}
                    end
            end;
        true ->
            {ok, ReqData, Context}
    end.


%%%%%%%%%%%%%% Helper functions %%%%%%%%%%%%%%

%% @doc Find the file referred to by the reqdata or the preconfigured path
ensure_file_info(ReqData, Context) ->
    {Path,ContextPath} = case z_context:get(path, Context) of
                            undefined ->
                                {mochiweb_util:unquote(wrq:disp_path(ReqData)), Context};
                            id -> 
                                RscId = m_rsc:rid(z_context:get_q("id", Context), Context),
                                ContextRsc = z_context:set(id, RscId, Context),
                                case m_media:get(RscId, ContextRsc) of
                                    undefined ->
                                        {undefined, ContextRsc};
                                    Media -> 
                                        {z_convert:to_list(proplists:get_value(filename, Media)), 
                                         z_context:set(mime, z_convert:to_list(proplists:get_value(mime, Media)), ContextRsc)}
                                end;
                            ConfiguredPath ->
                                {ConfiguredPath, Context}
                        end, 
    Cached = case z_context:get(use_cache, ContextPath) of
        true -> z_depcache:get(cache_key(Path), ContextPath);
        _    -> undefined
    end,
    case Cached of
        undefined ->
            case file_exists(Path, ContextPath) of 
            	{true, FullPath} -> 
            	    Context1 = z_context:set([ {path, Path}, {fullpath, FullPath} ], ContextPath),
            	    {true, ReqData, Context1};
            	_ -> 
            	    %% We might be able to generate a new preview
            	    case z_context:get(is_media_preview, ContextPath, false) of
            	        true ->
            	            % Generate a preview, recurse on success
            	            ensure_preview(ReqData, Path, ContextPath);
            	        false ->
                    	    {false, ReqData, ContextPath}
            	    end
            end;
        {ok, Cache} ->
            ContextCached = z_context:set([
                            {is_cached, true},
                            {path, Cache#cache.path},
                            {fullpath, Cache#cache.fullpath},
                            {mime, Cache#cache.mime},
                            {last_modified, Cache#cache.last_modified},
                            {body, Cache#cache.body}
                        ],
                        ContextPath),
            {true, ReqData, ContextCached}
    end.


cache_key(Path) ->
    {resource_file, Path}.

file_exists(undefined, _Context) ->
    false;
file_exists([], _Context) ->
    false;
file_exists(Name, Context) ->
    RelName = case hd(Name) of
        $/ -> tl(Name);
        _ -> Name
    end,
    case mochiweb_util:safe_relative_path(RelName) of
        undefined -> false;
        SafePath ->
            RelName = case hd(SafePath) of
                "/" -> tl(SafePath);
                _ -> SafePath
            end,
            Root = case z_context:get(root, Context) of
                undefined -> 
                    case z_context:get(is_media_preview, Context, false) of
                        true  -> [z_path:media_preview(Context)];
                        false -> [z_path:media_archive(Context)]
                    end;
                ConfRoot -> ConfRoot
            end,
            file_exists1(Root, RelName, Context)
    end.

file_exists1([], _RelName, _Context) ->
    false;
file_exists1([ModuleIndex|T], RelName, Context) when is_atom(ModuleIndex) ->
    case z_module_indexer:find(ModuleIndex, RelName, Context) of
        {ok, File} -> {true, File};
        {error, _} -> file_exists1(T, RelName, Context)
    end;
file_exists1([{module, Module}|T], RelName, Context) ->
    case Module:file_exists(RelName, Context) of
        false -> file_exists1(T, RelName, Context);
        Result -> Result
    end;
file_exists1([DirName|T], RelName, Context) ->
    NamePath = filename:join([DirName,RelName]),
    case filelib:is_regular(NamePath) of 
	true ->
	    {true, NamePath};
	false ->
	    file_exists1(T, RelName, Context)
    end.


%% @spec is_text(Mime) -> bool()
%% @doc Check if a mime type is textual
is_text("text/" ++ _) -> true;
is_text("application/x-javascript") -> true;
is_text("application/xhtml+xml") -> true;
is_text("application/xml") -> true;
is_text(_Mime) -> false.


%% @spec ensure_preview(ReqData, Path, Context) -> {Boolean, NewReqData, NewContext}
%% @doc Generate the file on the path from an archived media file.
%% The path is like: 2007/03/31/wedding.jpg(300x300)(crop-center)(709a-a3ab6605e5c8ce801ac77eb76289ac12).jpg
%% The original media should be in State#media_path (or z_path:media_archive)
%% The generated image should be created in State#root (or z_path:media_preview)
ensure_preview(ReqData, Path, Context) ->
    {Filepath, PreviewPropList, _Checksum, _ChecksumBaseString} = z_media_tag:url2props(Path, Context),
    case mochiweb_util:safe_relative_path(Filepath) of
        undefined ->
            {false, ReqData, Context};
        Safepath  ->
            MediaPath = case z_context:get(media_path, Context) of
                undefined -> z_path:media_archive(Context);
                ConfMediaPath -> ConfMediaPath
            end,
            
            MediaFile = case Safepath of 
                    "lib/" ++ LibPath ->  
                        case z_module_indexer:find(lib, LibPath, Context) of 
                            {ok, ModuleFilename} -> ModuleFilename; 
                            {error, _} -> filename:join(MediaPath, Safepath) 
                        end; 
                    _ -> 
                        filename:join(MediaPath, Safepath) 
            end,
            case filelib:is_regular(MediaFile) of
                true ->
                    % Media file exists, perform the resize
                    Root = case z_context:get(root, Context) of
                        [ConfRoot|_] -> ConfRoot;
                        _ -> z_path:media_preview(Context)
                    end,
                    PreviewFile = filename:join(Root, Path),
                    case z_media_preview:convert(MediaFile, PreviewFile, PreviewPropList, Context) of
                        ok -> {true, ReqData, z_context:set(fullpath, PreviewFile, Context)};
                        {error, Reason} -> throw(Reason)
                    end;
                false ->
                    {false, ReqData, Context}
            end
    end.


%% Encode the data so that the identity variant comes first and then the gzip'ed variant
encode_data(Data) when is_binary(Data) ->
	{Data, zlib:gzip(Data)}.

decode_data(gzip, Data) when is_binary(Data) ->
    zlib:gzip(Data);
decode_data(identity, Data) when is_binary(Data) ->
    Data;
decode_data(identity, {Data, _Gzip}) ->
	Data;
decode_data(gzip, {_Data, Gzip}) ->
	Gzip.

    