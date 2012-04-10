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

-module(resource_static_pages).
-export([init/1]).
-export([allowed_methods/2,
	 resource_exists/2,
	 last_modified/2,
	 expires/2,
	 content_types_provided/2,
	 charsets_provided/2,
	 encodings_provided/2,
	 provide_content/2,
	 finish_request/2,
	 previously_existed/2,
	 moved_temporarily/2
	 ]).

-include_lib("webmachine_resource.hrl").
-include_lib("zotonic.hrl").

%% These are used for file serving (move to metadata)
-record(state, {
        root=undefined,                 % Preconfigured media preview directory
        config=[],
        use_cache=false,
        encode_data=false,
        path=undefined,
        fullpath=undefined,
        is_cached=false,
        mime=undefined,
        last_modified=undefined,
        body=undefined,
        context=undefined
    }).

-record(cache, {
    path=undefined,
    fullpath=undefined,
    mime=undefined,
    last_modified=undefined,
    body=undefined
    }).

-define(MAX_AGE, 86400).


init(ConfigProps) ->
    Root     = proplists:get_value(root, ConfigProps),
    UseCache = proplists:get_value(use_cache, ConfigProps, false),
    {ok, #state{root=Root, use_cache=UseCache, config=ConfigProps}}.
    
allowed_methods(ReqData, State) ->
    {['HEAD', 'GET'], ReqData, State}.

resource_exists(ReqData, State) ->
    {ReqData1, State1} = check_resource(ReqData, State),
    case State1#state.fullpath of
        false -> {false, ReqData1, State1};
        redir -> {false, ReqData1, State1};
        _ -> {true, ReqData1, State1}
    end.


previously_existed(ReqData, #state{fullpath=redir} = State) ->
	{true, ReqData, State};
previously_existed(ReqData, State) ->
	{false, ReqData, State}.

moved_temporarily(ReqData, #state{fullpath=redir} = State) ->
    Location = case mochiweb_util:unquote(wrq:path(ReqData)) of 
        [] -> "/";
        [$/ | Path] -> [$/ | mochiweb_util:safe_relative_path(Path)] ++ "/"
    end,
	{{true, z_context:abs_url(Location, State#state.context)}, ReqData, State}.

content_types_provided(ReqData, State) ->
    {ReqData1, State1} = check_resource(ReqData, State),
    case State1#state.mime of
        undefined ->
            CT = z_media_identify:guess_mime(filename:basename(State1#state.fullpath, ".tpl")),
            {[{CT, provide_content}], ReqData1, State1#state{mime=CT}};
        Mime -> 
            {[{Mime, provide_content}], ReqData1, State1}
    end.

encodings_provided(ReqData, State) ->
    {ReqData1, State1} = check_resource(ReqData, State),
    Encodings = case filename:extension(State1#state.fullpath) of
        ".tpl" ->
            [{"identity", fun(Data) -> decode_data(identity, Data) end}];
        _ ->
            case State#state.mime of
                "image/jpeg" ->
                    [{"identity", fun(Data) -> decode_data(identity, Data) end}];
                _ -> 
                    [{"identity", fun(Data) -> decode_data(identity, Data) end},
                     {"gzip",     fun(Data) -> decode_data(gzip, Data) end}]
            end
    end,
    EncodeData = length(Encodings) > 1,
    {Encodings, ReqData1, State1#state{encode_data=EncodeData}}.


charsets_provided(ReqData, State) ->
    case is_text(State#state.mime) of
        true -> {[{"utf-8", fun(X) -> X end}], ReqData, State};
        _ -> {no_charset, ReqData, State}
    end.
    
last_modified(ReqData, State) ->
    {ReqData1, State1} = check_resource(ReqData, State),
    case filename:extension(State1#state.fullpath) of
        ".tpl" -> 
            NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            {calendar:gregorian_seconds_to_datetime(NowSecs - 86400), ReqData1, State1};
        _ ->
            RD1 = wrq:set_resp_header("Cache-Control", "public, max-age="++integer_to_list(?MAX_AGE), ReqData1),
            case State#state.last_modified of
                undefined -> 
                    LMod = filelib:last_modified(State#state.fullpath),
        			[LModUTC|_] = calendar:local_time_to_universal_time_dst(LMod),
                    {LModUTC, RD1, State1#state{last_modified=LModUTC}};
                LModUTC ->
                    {LModUTC, RD1, State1}
            end
    end.

expires(ReqData, State) ->
    {ReqData1, State1} = check_resource(ReqData, State),
    MaxAge = case filename:extension(State1#state.fullpath) of
        ".tpl" -> -86400;
        _ -> ?MAX_AGE
    end,
    NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    {calendar:gregorian_seconds_to_datetime(NowSecs + MaxAge), ReqData1, State1}.

provide_content(ReqData, State) ->
    case State#state.body of
        undefined ->
            FullPath = State#state.fullpath,
            case filename:extension(FullPath) of
                ".tpl" -> 
                    %% Render template, prevent caching
                    Context = z_context:set_reqdata(ReqData, State#state.context),
                    Context1 = z_context:ensure_all(Context),
                    Html = z_template:render(FullPath, State#state.config, Context1),
                    {Html1, Context2} = z_context:output(Html, Context1),
                    ReqData1 = z_context:get_reqdata(Context2),
                    State1 = State#state{context=Context2, use_cache=false, encode_data=false},
                    {iolist_to_binary(Html1), ReqData1, State1};
                _ -> 
                    %% Fetch file, allow caching
                    file:read_file(FullPath),
                    {ok, Data} = file:read_file(State#state.fullpath),
                    Body = case State#state.encode_data of 
                        true -> encode_data(Data);
                        false -> Data
                    end,
                    {Body, ReqData, State#state{body=Body}}
            end;
        Body ->
            {Body, ReqData, State}
    end.

    
finish_request(ReqData, State) ->
    case State#state.is_cached of
        false ->
            case State#state.body of
                undefined ->  
                    {ok, ReqData, State};
                _ ->
                    case State#state.use_cache andalso State#state.encode_data of
                        true ->
                            % Cache the served file in the depcache.  Cache it for 3600 secs.
                            Cache = #cache{
                                        path=State#state.path,
                                        fullpath=State#state.fullpath,
                                        mime=State#state.mime,
                                        last_modified=State#state.last_modified,
                                        body=State#state.body
                                    },
                            Context = z_context:new(ReqData, ?MODULE),
                            z_depcache:set(cache_key(State#state.path), Cache, Context),
                            {ok, ReqData, State};
                        _ ->
                            % No cache or no gzip'ed version (file system cache is fast enough for static file serving)
                            {ok, ReqData, State}
                    end
            end;
        true ->
            {ok, ReqData, State}
    end.


%%%%%%%%%%%%%% Helper functions %%%%%%%%%%%%%%
    
cache_key(Path) ->
    {resource_static_pages, Path}.

check_resource(ReqData, #state{fullpath=undefined} = State) ->
    Context = z_context:new(ReqData, ?MODULE),
    case mochiweb_util:safe_relative_path(mochiweb_util:unquote(wrq:disp_path(ReqData))) of
        undefined ->
            {error, enoent};
        SafePath ->
            Cached = case State#state.use_cache of
                true -> z_depcache:get(cache_key(SafePath), Context);
                _    -> undefined
            end,
            case Cached of
                undefined ->
                    case find_file(State#state.root, SafePath, Context) of 
                    	{ok, FullPath} -> 
            	            case filename:basename(FullPath, ".tpl") of
            	                "index.html" ->
            	                    case last(wrq:path(ReqData)) /= $/ andalso filename:basename(SafePath) /= "index.html" of
            	                        true ->
                            	            {ReqData, State#state{path=SafePath, fullpath=redir, context=Context}};
            	                        false ->
                            	            {ReqData, State#state{path=SafePath, fullpath=FullPath, context=Context}}
            	                    end;
            	                _ ->
                    	            {ReqData, State#state{path=SafePath, fullpath=FullPath, context=Context}}
                    	    end;
                    	{error, _Reason} -> 
                    	    {ReqData, State#state{path=SafePath, fullpath=false, context=Context, mime="text/html"}}
                    end;
                {ok, Cache} ->
                    {ReqData, State#state{
                                    is_cached=true,
                                    path=Cache#cache.path,
                                    fullpath=Cache#cache.fullpath,
                                    mime=Cache#cache.mime,
                                    last_modified=Cache#cache.last_modified,
                                    body=Cache#cache.body,
                                    context=Context
                                }}
            end
    end;
check_resource(ReqData, State) ->
    {ReqData, State}.
    
    last([]) -> undefined;
    last(L) -> lists:last(L).

    find_file(Root, File, Context) ->
        RelName = case File of
            "" -> "";
            "/" ++ RelFile -> RelFile;
            _ -> File
        end,
        T = [ RelName, 
              RelName ++ ".tpl", 
              filename:join(RelName, "index.html.tpl"), 
              filename:join(RelName, "index.html")
            ],
        case find_template(T, Context) of
            {error, enoent} ->
                Root1 = case Root of
                    "/" ++ _ -> Root;
                    _ -> filename:join(z_path:site_dir(Context),Root)
                end,
                RelName1 = filename:join(Root1, RelName),
                T1 = [  RelName1,
                        RelName1 ++ ".tpl", 
                        filename:join(RelName1, "index.html.tpl"),
                        filename:join(RelName1, "index.html") 
                     ],
                find_file1(T1);
            {ok, File} = Found -> 
                Found
        end.

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



%% @spec is_text(Mime) -> bool()
%% @doc Check if a mime type is textual
is_text("text/" ++ _) -> true;
is_text("application/x-javascript") -> true;
is_text("application/xhtml+xml") -> true;
is_text("application/xml") -> true;
is_text(_Mime) -> false.


%% Encode the data so that the identity variant comes first and then the gzip'ed variant
encode_data(Data) when is_binary(Data) ->
    {encoded, Data, zlib:gzip(Data)}.

decode_data(identity, Data) when is_binary(Data) ->
    Data;
decode_data(gzip, Data) when is_binary(Data) ->
    zlib:gzip(Data);
decode_data(identity, {encoded, Data, _Gzip}) ->
    Data;
decode_data(gzip, {encoded, _Data, Gzip}) ->
    Gzip.

