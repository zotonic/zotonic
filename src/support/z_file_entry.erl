%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014 Marc Worrell
%%
%% @doc Process holding information mapping a request path to one or more files.
%% The files can be a static file, a temporary file, cached file or a binary.

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

%% TODO:
%% - Keep medium-filename for content-disposition (also other meta info?)
%% - On resizing, if source image was gone, retry with new lookup (also check after resize-error if source image was still present)

-module(z_file_entry).

-behaviour(gen_fsm).

-include_lib("zotonic.hrl").
-include_lib("zotonic_file.hrl").
-include_lib("kernel/include/file.hrl").

%% API
-export([
    reg_name/2,
    where/2,
    lookup/1,
    lookup/2,
    stop/1,
    stop/2,
    force_stale/1,
    force_stale/2
    ]).

%% gen_fsm exports
-export([
    start_link/4,
    init/1,
    handle_sync_event/4,
    handle_event/3,
    handle_info/3,
    code_change/4,
    terminate/3
    ]).

%% FSM states
-export([
    locate/2,
    content_encoding_gzip/2,
    serving/2,
    stopping/2
    ]).

-record(state, {
        site,                   % site associated with this file
        request_path,           % request path of this file (binary)
        root,                   % Optional root where to find the files
        image_filters,          % Optional filters to apply to the found file
        is_found,               % false if request path does not exist
        acls = [],              % Associated resource ids for acl check
        mime,                   % content-type
        modified,               % modification time in localtime
        modifiedUTC,            % modification time in UTC
        last_stale_check,       % last time the stale check was run
        parts = [],             % one request can consist of multiple parts from multiple sources
        size = 0,               % total size of all parts
        gzip_part,              % gzip is always a single part (in memory or in cache)
        gzip_size,              % size of gzip encoded data
        gzipper,                % ref to identify gzip process
        waiting = [],           % List of waiting processes
        index_ref
    }).


%% Maximum uncompressed size that we will try to gzip.
%% This is to preserve memory.
-define(MAX_GZIP_SIZE, 16*1024*1024).

%% Period after a stop decision to handle late requests (5 secs)
-define(STOP_TIMEOUT, 5000).

%% Inactivity timeout when serving (1 hour)
-define(SERVING_TIMEOUT, 3600000).

%% Inactivity timeout for entries that are not found (60 secs)
-define(SERVING_ENOENT_TIMEOUT, 60000).

%% Period between stale checks (in usec, 2 seconds)
-define(STALE_TIMER_DIFF, 2000000).


%%% ------------------------------------------------------------------------------------
%%% API
%%% ------------------------------------------------------------------------------------

start_link(RequestPath, Root, OptFilterProps, Context) when is_binary(RequestPath) ->
    Args = [ RequestPath, Root, OptFilterProps, z_context:site(Context) ],
    gen_fsm:start_link({via, gproc, {n,l,reg_name(RequestPath, Context)}}, ?MODULE, Args, []).

reg_name(Path, #context{} = Context) ->
    reg_name(Path, z_context:site(Context));
reg_name(Path, Site) when is_binary(Path), is_atom(Site) ->
    {?MODULE, Site, Path}.

where(Path, ContextOrSite) ->
    gproc:where({n,l,reg_name(Path,ContextOrSite)}).

stop(Pid) when is_pid(Pid) ->
    gen_fsm:send_all_state_event(Pid, stop);
stop(undefined) ->
    ok.

stop(RequestPath, Context) when is_binary(RequestPath) ->
    stop(gproc:where({n,l,reg_name(RequestPath, Context)})).

force_stale(undefined) -> 
    {error, noproc};
force_stale(Pid) ->
    try
        gen_fsm:sync_send_all_state_event(Pid, force_stale, infinity)
    catch
        exit:{noproc, _} ->
            {error, noproc}
    end.

force_stale(RequestPath, Context) when is_binary(RequestPath) ->
    force_stale(gproc:where({n,l,reg_name(RequestPath, Context)})).

lookup(Pid) when is_pid(Pid) ->
    try
        gen_fsm:sync_send_all_state_event(Pid, lookup, infinity)
    catch
        exit:{noproc, _} ->
            {error, noproc}
    end;
lookup(undefined) ->
    {error, noproc}.

lookup(RequestPath, Context) when is_binary(RequestPath) ->
    lookup(gproc:where({n,l,reg_name(RequestPath, Context)})).

%%% ------------------------------------------------------------------------------------
%%% gen_fsm callbacks
%%% ------------------------------------------------------------------------------------

init([RequestPath, Root, OptFilterProps, Site]) ->
    State = #state{
        site=Site,
        request_path=RequestPath,
        root=Root,
        image_filters=OptFilterProps,
        mime = <<"application/octet-stream">>,
        modified = {{1970,1,1},{0,0,0}},
        modifiedUTC = {{1970,1,1},{0,0,0}},
        last_stale_check = os:timestamp()
    },
    {ok, locate, State, 0}.

%%% ------------------------------------------------------------------------------------
%%% gen_fsm states
%%% ------------------------------------------------------------------------------------

locate(timeout, State) ->
    Context = z_context:new(State#state.site),
    IndexRef = z_module_indexer:index_ref(Context), 
    Mime = z_convert:to_binary(z_media_identify:guess_mime(State#state.request_path)),
    Files = z_lib_include:uncollapse(State#state.request_path),
    try
        FilesFilters = [ extract_filters(F, State#state.image_filters, Context) || F <- Files ],
        Sources = locate_sources(State#state.root, FilesFilters, Context),
        Modified = newest_part(Sources, State#state.modified),
        Size = total_size(Sources),
        ModifiedUTC = hd(calendar:local_time_to_universal_time_dst(Modified)),
        State1 = State#state{
            is_found=true,
            mime=Mime,
            size=Size,
            parts=Sources,
            modified=Modified,
            modifiedUTC=ModifiedUTC,
            index_ref=IndexRef
        },
        {next_state, content_encoding_gzip, reply_waiting(State1), 0}
    catch
        error:checksum_invalid ->
            locate_enoent(State, IndexRef, Mime);
        throw:enoent ->
            locate_enoent(State, IndexRef, Mime)
    end.

locate_enoent(State, IndexRef, Mime) ->
    State1 = State#state{
        is_found = false,
        mime=Mime,
        parts=[],
        index_ref=IndexRef
    },
    {next_state, serving, reply_waiting(State1), ?SERVING_ENOENT_TIMEOUT}.

%% Add and cache the gzip content-encoded data (for now in memory)
%% This streams all parts to the linked process, accumulating them in gzip buffer
content_encoding_gzip(timeout, State) ->
    case is_compressable(State#state.mime) andalso State#state.size < ?MAX_GZIP_SIZE of
        true ->
            % Start compression in separate process
            State1 = State#state{gzipper = start_link_gzip(State#state.parts)},
            {next_state, serving, State1, ?SERVING_TIMEOUT};   
        false ->
            {next_state, serving, State, ?SERVING_TIMEOUT}
    end.

serving(timeout, State) ->
    unreg(State),
    {next_state, stopping, State, ?STOP_TIMEOUT}.

stopping(timeout, State) ->
    lager:debug("Stop file entry ~p:~p", [State#state.site, State#state.request_path]),
    {stop, normal, State}.

%%% ------------------------------------------------------------------------------------
%%% gen_fsm events
%%% ------------------------------------------------------------------------------------

handle_sync_event(lookup, From, locate, State) ->
    {next_state, locate, State#state{waiting=[From|State#state.waiting]}, 0};
handle_sync_event(lookup, _From, stopping, State) ->
    {next_state, stopping, lookup_file_info(State), ?STOP_TIMEOUT};
handle_sync_event(lookup, From, StateName, State) ->
    case check_current(State) of
        {ok, State1} ->
            Reply = lookup_file_info(State1),
            case timeout(StateName, State#state.is_found) of
                undefined ->
                    {reply, Reply, StateName, State1};
                Timeout ->
                    {reply, Reply, StateName, State1, Timeout}
            end;
        stale ->
            lager:debug("Stale file entry ~p:~p (relocating)", [State#state.site, State#state.request_path]),
            State1 = State#state{waiting=[From|State#state.waiting]},
            {next_state, locate, State1, 0}
    end;
handle_sync_event(force_stale, _From, _StateName, State) ->
    {reply, ok, locate, State, 0};
handle_sync_event(Msg, _From, StateName, State) ->
    lager:error("Unexpected sync-event ~p in state ~p", [Msg, StateName]),
    {next_state, StateName, State, ?SERVING_TIMEOUT}.


handle_event(stop, _StateName, State) ->
    unreg(State),
    {next_state, stopping, State, ?STOP_TIMEOUT};
handle_event(Msg, StateName, State) ->
    lager:error("Unexpected event ~p in state ~p", [Msg, StateName]),
    {next_state, StateName, State}.

handle_info({gzip, Ref, Data}, StateName, #state{gzipper=Ref} = State) ->
    State1 = State#state{
        gzip_part=#part_data{data=Data},
        gzip_size=size(Data),
        gzipper=undefined
    },
    {next_state, StateName, State1};
handle_info({'DOWN', MRef, process, _Pid, _Info}, StateName, State) ->
    case is_mref_part(MRef, State#state.parts) of
        true ->
            {stop, normal, State};
        false ->
            {next_state, StateName, State}
    end;
handle_info(Info, StateName, State) ->
    lager:info("Unexpected info ~p in state ~p", [Info, StateName]),
    {next_state, StateName, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

%%% ------------------------------------------------------------------------------------
%%% support routines
%%% ------------------------------------------------------------------------------------

timeout(locate, _IsFound) ->
    0;
timeout(content_encoding_gzip, true) ->
    0;
timeout(serving, true) ->
    ?SERVING_TIMEOUT;
timeout(serving, false) ->
    ?SERVING_ENOENT_TIMEOUT;
timeout(stopping, _IsFound) ->
    ?STOP_TIMEOUT;
timeout(_State, _IsFound) ->
    undefined.

unreg(State) ->
    gproc:unreg({n,l,reg_name(State#state.request_path, State#state.site)}).

is_mref_part(MRef, Parts) ->
    lists:any(fun(#part_cache{cache_monitor=Ref}) -> Ref =:= MRef ; (_) -> false end, Parts).

check_current(#state{index_ref=IndexRef} = State) ->
    case z_module_indexer:index_ref(z_context:new(State#state.site)) of   
        IndexRef ->
            Now = os:timestamp(),
            check_current_1(timer:now_diff(Now, State#state.last_stale_check), Now, State);
        _Reindexed ->
            stale
    end.

check_current_1(Diff, Now, State) when Diff > ?STALE_TIMER_DIFF ->
    case lists:any(fun is_stale_part/1, State#state.parts) of
        true -> stale;
        false -> {ok, State#state{last_stale_check=Now}}
    end;
check_current_1(_Diff, _Now, State) ->
    {ok, State}.

is_stale_part(#part_file{filepath=Filename, size=Size, modified=MTime}) ->
    case file:read_file_info(Filename) of
        {ok, #file_info{size=Size, type=regular, mtime=MTime}} ->
            false;
        _ ->
            true
    end;
is_stale_part(_Part) ->
    false.

reply_waiting(State) ->
    Reply = lookup_file_info(State),
    lists:foreach(fun(From) ->
                        gen_fsm:reply(From, Reply)
                  end, State#state.waiting),
    State#state{waiting=[], last_stale_check=os:timestamp()}. 

lookup_file_info(#state{is_found = false}) ->
    {error, enoent};
lookup_file_info(State) ->
    {ok, #z_file_info{
        modifiedUTC = State#state.modifiedUTC,
        mime = State#state.mime,
        size = State#state.size,
        acls = lookup_acls(State#state.parts, []),
        encodings = lookup_encodings(State)
    }}.

lookup_encodings(#state{gzip_part=undefined} = State)  ->
    [{identity, State#state.parts}];
lookup_encodings(#state{gzip_part=GzipPart} = State) ->
    [{identity, State#state.parts}, {gzip, [GzipPart]}].

lookup_acls([], Acc) ->
    Acc;
lookup_acls([#part_file{acl=undefined}|Ps], Acc) ->
    lookup_acls(Ps, Acc);
lookup_acls([#part_data{acl=undefined}|Ps], Acc) ->
    lookup_acls(Ps, Acc);
lookup_acls([#part_cache{acl=undefined}|Ps], Acc) ->
    lookup_acls(Ps, Acc);
lookup_acls([#part_file{acl=Acl}|Ps], Acc) ->
    lookup_acls(Ps, [Acl|Acc]);
lookup_acls([#part_data{acl=Acl}|Ps], Acc) ->
    lookup_acls(Ps, [Acl|Acc]);
lookup_acls([#part_cache{acl=Acl}|Ps], Acc) ->
    lookup_acls(Ps, [Acl|Acc]).


extract_filters(Path, OptFilters, Context) ->
    case safe_path(Path) of
        undefined ->
            lager:warning("Unsafe path ~p", Path),
            throw(enoent);
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

locate_sources(Root, Files, Context) ->
    [ locate_source(Root, Path, OriginalFile, Filters, Context) || {Path, OriginalFile, Filters} <- Files ].

% Find all files, possibly starting a preview-request
locate_source(NoRoots, Path, "lib/"++OriginalFile, Filters, Context) when NoRoots =:= undefined; NoRoots =:= [] ->
    locate_source([lib], Path, OriginalFile, Filters, Context);
locate_source(NoRoots, Path, <<"lib/",OriginalFile/binary>>, Filters, Context) when NoRoots =:= undefined; NoRoots =:= [] ->
    locate_source([lib], Path, OriginalFile, Filters, Context);
locate_source(NoRoots, Path, OriginalFile, Filters, Context) when NoRoots =:= undefined; NoRoots =:= [] ->
    case locate_source_uploaded(Path, OriginalFile, Filters, Context) of
        {error, _} = Error->
            lager:debug("Could not find ~p, error ~p", [Path, Error]),
            throw(enoent);
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
    NamePath = filename:join([DirName,Path]),
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
            lager:debug("Unknown medium file ~p", [OriginalFile]),
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
                modified=proplists:get_value(modified, Opts),
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
            case z_media_preview:convert(z_convert:to_list(Filename), z_convert:to_list(PreviewFilePath), Filters, Context) of
                ok ->
                    FileStorePath = filename:join([filename:basename(PreviewDir), Path]),
                    z_notifier:first(#filestore{action=upload, path=FileStorePath}, Context),
                    case proplists:get_value(id, Medium) of
                        undefined ->
                            part_file(PreviewFilePath);
                        RscId ->
                            part_file(PreviewFilePath, [{acl,RscId}])
                    end;
                {error, _} = Error ->
                    lager:warning("Convert error: ~p for path ~p", [Error, Path]),
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
generate_preview(false, _Mime, _Path, _OriginalFile, _Filters, _Medium, _Context) ->
    {error, enoent}.

% TODO: create temp file for large #part_data{} returns
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


newest_part([], M) ->
    M;
newest_part([#part_file{modified=M1}|Ps], M) when M =/= undefined, M1 > M -> 
    newest_part(Ps, M1);
newest_part([#part_cache{modified=M1}|Ps], M) when M =/= undefined, M1 > M -> 
    newest_part(Ps, M1);
newest_part([#part_data{modified=M1}|Ps], M) when M =/= undefined, M1 > M -> 
    newest_part(Ps, M1);
newest_part([_|Ps], M) ->
    newest_part(Ps, M).


total_size(Parts) ->
    lists:sum([ part_size(P) || P <- Parts ]).

part_size(#part_file{size=Size}) when is_integer(Size) -> Size;
part_size(#part_cache{size=Size}) when is_integer(Size) -> Size;
part_size(#part_data{data=D}) when is_binary(D) -> size(D);
part_size(_) -> 0.

is_compressable(<<"text/", _/binary>>) -> true;
is_compressable(<<"application/x-javascript">>) -> true;
is_compressable(<<"application/xhtml+xml">>) -> true;
is_compressable(<<"application/xml">>) -> true;
is_compressable(_Mime) -> false.

start_link_gzip(Sources) ->
    Self = self(),
    Ref = erlang:make_ref(),
    Pid = erlang:spawn_link(fun() ->
                          Bin = gzip_compress(Sources),
                          Self ! {gzip, {Ref, self()}, Bin}
                      end),
    {Ref, Pid}.

% TODO: big files should be compressed to disk (tmpfile or filezcache should do)
-define(MAX_WBITS, 15).

gzip_compress(Sources) ->
    Z = zlib:open(),
    zlib:deflateInit(Z, default, deflated, 16+?MAX_WBITS, 8, default),
    Compressed = gzip_compress_1(Sources, Z, []),
    Last = zlib:deflate(Z, <<>>, finish),
    ok = zlib:deflateEnd(Z),
    zlib:close(Z),
    iolist_to_binary([Compressed,Last]).

gzip_compress_1([], _Z, Acc) ->
    lists:reverse(Acc);
gzip_compress_1([#part_data{data=B}|Ps], Z, Acc) ->
    Acc1 = [ zlib:deflate(Z, B) | Acc],
    gzip_compress_1(Ps, Z, Acc1);
gzip_compress_1([#part_file{filepath=Filename}|Ps], Z, Acc) ->
    Acc1 = compress_file(Filename, Z, Acc),
    gzip_compress_1(Ps, Z, Acc1);
gzip_compress_1([#part_cache{cache_pid=Pid}|Ps], Z, Acc) ->
    {ok, {filename, _, Filename}} = filezcache:lookup_file(Pid),
    Acc1 = compress_file(Filename, Z, Acc),
    gzip_compress_1(Ps, Z, Acc1).


% We read 512K at once for compressing files.
-define(BLOCK_SIZE, 512*1024).

compress_file(Filename, Z, Acc) ->
    {ok, IO} = file:open(Filename, [read,raw,binary]),
    Compressed = compress_file_1(Z, IO, Acc),
    ok = file:close(IO),
    Compressed.

compress_file_1(Z, IO, Acc) ->
    case file:read(IO, ?BLOCK_SIZE) of
        eof ->
            Acc;
        {ok, Data} ->
            compress_file_1(Z, IO, [zlib:deflate(Z, Data)|Acc])
    end.


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
