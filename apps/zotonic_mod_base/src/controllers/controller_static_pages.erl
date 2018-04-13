%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%%
%% @doc Serve static pages relative to a defined root.  This resource is useful to add a complete static html subsite to an existing site.

%% Copyright 2010 Marc Worrell
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

-module(controller_static_pages).
-export([
     allowed_methods/1,
     resource_exists/1,
     last_modified/1,
     expires/1,
     content_types_provided/1,
     charsets_provided/1,
     provide_content/1,
     finish_request/1,
     previously_existed/1,
     moved_temporarily/1
     ]).

-include_lib("template_compiler/include/template_compiler.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

-record(cache, {
    path=undefined,
    fullpath=undefined,
    mime=undefined,
    last_modified=undefined,
    body=undefined
    }).

-define(MAX_AGE, 86400).


% service_available(Context) ->
%     Root     = z_context:get(root, Context),
%     DirIndex = z_convert:to_bool(z_context:get(allow_directory_index, Context, false)),
%     UseCache = z_context:get(use_cache, Context, false),
%     State = #state{
%         root = Root,
%         use_cache = UseCache,
%         allow_directory_index=DirIndex
%     }}.

allowed_methods(Context) ->
    {[<<"HEAD">>, <<"GET">>], Context}.

resource_exists(Context) ->
    Context1 = check_resource(Context),
    case z_context:get(fullpath, Context1) of
        false -> {false, Context1};
        redir -> {false, Context1};
        _ -> {true, Context1}
    end.


previously_existed(Context) ->
    case z_context:get(fullpath, Context) of
        redir -> {true, Context};
        _ -> {false, Context}
    end.

% Moved if fullpath =:= redir
moved_temporarily(Context) ->
    RawPath = cowmachine_req:raw_path(Context),
    Location = case cow_qs:urldecode(RawPath) of
        <<>> ->
            <<"/">>;
        <<$/, Path/binary>> ->
            iolist_to_binary([
                    $/,
                    mochiweb_util:safe_relative_path(z_convert:to_list(Path)),
                    $/
                ])
    end,
    {{true, z_context:abs_url(Location, Context)}, Context}.

content_types_provided(Context) ->
    Context1 = check_resource(Context),
    case z_context:get(mime, Context1) of
        undefined ->
            FullPath = z_context:get(fullpath, Context1),
            CT = z_media_identify:guess_mime(filename:basename(FullPath, <<".tpl">>)),
            Context2 = z_context:set(mime, CT, Context1),
            {[{CT, provide_content}], Context2};
        Mime ->
            {[{Mime, provide_content}], Context1}
    end.

charsets_provided(Context) ->
    case is_text(z_context:get(mime, Context)) of
        true -> {[<<"utf-8">>], Context};
        _ -> {no_charset, Context}
    end.

last_modified(Context) ->
    Context1 = check_resource(Context),
    FullPath = z_context:get(fullpath, Context1),
    case filename:extension(FullPath) of
        <<".tpl">> ->
            NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            {calendar:gregorian_seconds_to_datetime(NowSecs - 86400), Context1};
        _ ->
            Context2 = z_context:set_resp_header(
                    <<"cache-control">>,
                    <<"public, max-age=", (z_convert:to_binary(?MAX_AGE))/binary>>,
                    Context1),
            case z_context:get(last_modified, Context) of
                undefined ->
                    LMod = filelib:last_modified(FullPath),
                    [LModUTC|_] = calendar:local_time_to_universal_time_dst(LMod),
                    Context3 = z_context:set(last_modified, LModUTC),
                    {LModUTC, Context3};
                LModUTC ->
                    {LModUTC, Context2}
            end
    end.

expires(Context) ->
    Context1 = check_resource(Context),
    FullPath = z_context:get(fullpath, Context1),
    MaxAge = case filename:extension(FullPath) of
        <<".tpl">> -> -86400;
        _ -> ?MAX_AGE
    end,
    NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    {calendar:gregorian_seconds_to_datetime(NowSecs + MaxAge), Context1}.

provide_content(Context) ->
    case z_context:get(body, Context) of
        undefined ->
            FullPath = z_context:get(fullpath, Context),
            case filelib:is_dir(FullPath) of
                true ->
                    %% Render directory index
                    Context1 = z_context:ensure_qs(Context),
                    Root = z_context:get(root, Context),
                    Vars = directory_index_vars(FullPath, Root, Context)
                            ++ z_context:get_all(Context),
                    Html = z_template:render(
                                    <<"directory_index.tpl">>,
                                    Vars,
                                    Context1),
                    {Html1, Context2} = z_context:output(Html, Context1),
                    Context3 = z_context:set([
                            {use_cache, false},
                            {multiple_encodings, false}
                        ],
                        Context2),
                    {iolist_to_binary(Html1), Context3};
                false ->
                    case filename:extension(FullPath) of
                        <<".tpl">> ->
                            %% Render template, prevent caching
                            Context1 = z_context:ensure_qs(Context),
                            Vars = z_context:get_all(Context1),
                            Template = #template_file{
                                filename = FullPath,
                                template = z_context:get(path, Context)
                            },
                            Html = z_template:render(Template, Vars, Context1),
                            {Html1, Context2} = z_context:output(Html, Context1),
                            Context3 = z_context:set([
                                    {use_cache, false},
                                    {multiple_encodings, false}
                                ],
                                Context2),
                            {iolist_to_binary(Html1), Context3};
                        _ ->
                            case z_convert:to_bool(z_context:get(multiple_encodings, Context)) of
                                true ->
                                    {ok, Data} = file:read_file(z_context:get(fullpath, Context)),
                                    Body = encode_data(Data),
                                    Context1 = z_context:set(body, Body, Context),
                                    {select_encoding(Body, Context1), Context1};
                                false ->
                                    {{file, FullPath}, Context}
                            end
                    end
            end;
        Body ->
            {select_encoding(Body, Context), Context}
    end.


finish_request(Context) ->
    case z_context:get(is_cached, Context, false) of
        false ->
            case z_context:get(body, Context) of
                undefined ->
                    {ok, Context};
                _ ->
                    UseCache = z_convert:to_bool(z_context:get(use_cache, Context)),
                    MultipleEncodings = z_convert:to_bool(z_context:get(multiple_encodings, Context)),
                    case UseCache andalso MultipleEncodings of
                        true ->
                            % Cache the served file in the depcache.  Cache it for 3600 secs.
                            Path = z_context:get(path, Context),
                            Cache = #cache{
                                        path = Path,
                                        fullpath = z_context:get(fullpath, Context),
                                        mime = z_context:get(mime, Context),
                                        last_modified = z_context:get(last_modified, Context),
                                        body = z_context:get(body, Context)
                                    },
                            z_depcache:set(cache_key(Path), Cache, Context),
                            {ok, Context};
                        _ ->
                            % No cache or no gzip'ed version (file system cache is fast enough for static file serving)
                            {ok, Context}
                    end
            end;
        true ->
            {ok, Context}
    end.


%%%%%%%%%%%%%% Helper functions %%%%%%%%%%%%%%

cache_key(Path) ->
    {?MODULE, Path}.

check_resource(Context) ->
    case z_context:get(fullpath, Context) of
        undefined ->
            check_resource_1(Context);
        _FullPath ->
            Context
    end.

check_resource_1(Context) ->
    Context1 = z_context:set_noindex_header(Context),
    DispPath = cow_qs:urldecode(cowmachine_req:disp_path(Context1)),
    SafePath = z_convert:to_binary(
                    mochiweb_util:safe_relative_path(
                        z_convert:to_list(DispPath))),
    Cached = case z_context:get(use_cache, Context, false) of
        true -> z_depcache:get(cache_key(SafePath), Context);
        false -> undefined
    end,
    case Cached of
        undefined ->
            Root = abs_root(z_context:get(root, Context), Context),
            case find_file(Root, SafePath, Context) of
                {ok, FullPath} ->
                    case filename:basename(FullPath, <<".tpl">>) of
                        <<"index.html">> ->
                            case last(DispPath) /= $/
                                andalso filename:basename(SafePath) /= <<"index.html">>
                            of
                                true ->
                                    z_context:set([
                                            {path, SafePath},
                                            {fullpath, redir}
                                        ], Context);
                                false ->
                                    z_context:set([
                                            {path, SafePath},
                                            {fullpath, FullPath}
                                        ], Context)
                            end;
                        _ ->
                            z_context:set([
                                    {path, SafePath},
                                    {fullpath, FullPath}
                                ], Context)
                    end;
                {error, eacces} ->
                    z_context:set([
                            {path, SafePath},
                            {fullpath, false},
                            {mime, <<"text/html">>}
                        ], Context);
                {error, _Reason} ->
                    Dir = filename:join(Root, SafePath),
                    AllowDirIndex = z_context:get(allow_directory_index, Context, false),
                    case filelib:is_dir(Dir) andalso AllowDirIndex of
                        true ->
                            case last(DispPath) /= $/ of
                                true ->
                                    z_context:set([
                                            {path, SafePath},
                                            {fullpath, redir}
                                        ], Context);
                                false ->
                                    z_context:set([
                                            {path, SafePath},
                                            {fullpath, false},
                                            {mime, <<"text/html">>}
                                        ], Context)
                            end;
                        false ->
                            z_context:set([
                                    {path, SafePath},
                                    {fullpath, false},
                                    {mime, <<"text/html">>}
                                ], Context)
                    end
            end;
        {ok, Cache} ->
            z_context:set([
                    {is_cached, true},
                    {path, Cache#cache.path},
                    {fullpath, Cache#cache.fullpath},
                    {mime, Cache#cache.mime},
                    {last_modified, Cache#cache.last_modified},
                    {body, Cache#cache.body}
                ], Context)
    end.

last(undefined) -> undefind;
last(<<>>) -> undefined;
last(B) when is_binary(B) -> binary:last(B).

find_file(Root, File, Context) ->
    RelName = case File of
        <<>> -> <<>>;
        <<"/", RelFile/binary>> -> RelFile;
        _ -> File
    end,
    case is_protected(RelName) of
        true ->
            {error, eacces};
        false ->
            T = [ RelName,
                  <<RelName/binary, ".tpl">>,
                  filename:join(RelName, "index.html.tpl"),
                  filename:join(RelName, "index.html")
                ],
            case find_template(T, Context) of
                {error, enoent} ->
                    RelName1 = filename:join(Root, RelName),
                    T1 = [  RelName1,
                            <<RelName/binary, ".tpl">>,
                            filename:join(RelName1, "index.html.tpl"),
                            filename:join(RelName1, "index.html")
                         ],
                    find_file1(T1);
                {ok, File} = Found ->
                    Found
            end
    end.

is_protected(<<".", _/binary>>) -> true;
is_protected(<<"/.", _/binary>>) -> true;
is_protected(Filename) -> is_protected_1(Filename).

is_protected_1(<<>>) -> false;
is_protected_1(<<"/.", _/binary>>) -> true;
is_protected_1(<<"\\.", _/binary>>) -> true;
is_protected_1(<<_, Rest/binary>>) -> is_protected_1(Rest).

find_file1([]) ->
    {error, enoent};
find_file1([F|R]) ->
    case filelib:is_regular(F) of
        true -> {ok, F};
        false -> find_file1(R)
    end.

find_template([], _Context) ->
    {error, enoent};
find_template([F|R], Context) ->
    case z_module_indexer:find(template, F, Context) of
        {error, enoent} -> find_template(R, Context);
        {ok, #module_index{filepath=File}} -> {ok, File}
    end.



%% @doc Check if a mime type is textual
-spec is_text(binary()) -> boolean().
is_text(<<"text/", _/binary>>) -> true;
is_text(<<"application/x-javascript">>) -> true;
is_text(<<"application/xhtml+xml">>) -> true;
is_text(<<"application/xml">>) -> true;
is_text(_Mime) -> false.


select_encoding(Data, Context) ->
    decode_data(cowmachine_req:resp_content_encoding(Context), Data).

%% Encode the data so that the identity variant comes first and then the gzip'ed variant
encode_data(Data) when is_binary(Data) ->
    {encoded, Data, zlib:gzip(Data)}.

decode_data(<<"identity">>, Data) when is_binary(Data) ->
    Data;
decode_data(<<"gzip">>, Data) when is_binary(Data) ->
    zlib:gzip(Data);
decode_data(<<"identity">>, {encoded, Data, _Gzip}) ->
    Data;
decode_data(<<"gzip">>, {encoded, _Data, Gzip}) ->
    Gzip.

abs_root(Root, Context) ->
    case Root of
        <<"/", _/binary>> -> Root;
        _ -> filename:join(z_path:site_dir(Context), Root)
    end.

directory_index_vars(FullPath, RelRoot, Context) ->
    Root = abs_root(RelRoot, Context),
    UpFileEntry = case Root =:= FullPath of
                      true -> [];
                      false -> [fileinfo(filename:dirname(FullPath), <<"..">>)]
                  end,

    Files = lists:sort(
                fun fileinfo_cmp/2,
                    lists:map(
                        fun fileinfo/1,
                        lists:sort(z_utils:wildcard(filename:join(FullPath, <<"*">>))))),
    [{basename, filename:basename(FullPath)},
     {files, UpFileEntry ++ Files}
    ].


fileinfo(F) ->
    fileinfo(F, filename:basename(F)).

fileinfo(F, N) ->
    [{name, N},
     {is_dir, filelib:is_dir(F)},
     {last_modified, filelib:last_modified(F)},
     {mime, z_media_identify:guess_mime(F)},
     {size, filelib:file_size(F)}].


fileinfo_cmp(A, B) ->
    case proplists:get_value(is_dir, A) =:= proplists:get_value(is_dir, B) of
        true ->
            proplists:get_value(name, A) < proplists:get_value(name, B);
        false ->
            proplists:get_value(is_dir, A) > proplists:get_value(is_dir, B)
    end.
