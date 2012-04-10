%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Serve static library files (css and js).  Library files can be combined in one path, using z_lib_include:tag/2
%%
%% Serves files like: /lib/some/path

%% Copyright 2009-2011 Marc Worrell, Konstantin Nikiforov
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


-module(resource_lib).
-export([init/1]).
-export([allowed_methods/2,
	 resource_exists/2,
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

%% These are used for file serving (move to metadata)
-record(state, {
        root=undefined,
        content_disposition=undefined,
        use_cache=false,
        encode_data=false,
        fullpaths=undefined,
        is_cached=false,
        path=undefined,
        mime=undefined,
        last_modified=undefined,
        body=undefined
    }).

-record(cache, {
    path=undefined,
    fullpaths=undefined,
    mime=undefined,
    last_modified=undefined,
    body=undefined
    }).

-define(MAX_AGE, 315360000).

init(ConfigProps) ->
    UseCache = proplists:get_value(use_cache, ConfigProps, false),
    Root = proplists:get_value(root, ConfigProps, [lib]),
    ContentDisposition = proplists:get_value(content_disposition, ConfigProps),
    {ok, #state{root=Root, use_cache=UseCache, content_disposition=ContentDisposition}}.
    
allowed_methods(ReqData, State) ->
    {['HEAD', 'GET'], ReqData, State}.

content_types_provided(ReqData, State) ->
	State1 = lookup_path(ReqData, State),
    case State1#state.mime of
        undefined ->
            Path = mochiweb_util:unquote(wrq:disp_path(ReqData)),
            CT = z_media_identify:guess_mime(Path),
            {[{CT, provide_content}], ReqData, State1#state{mime=CT}};
        Mime -> 
            {[{Mime, provide_content}], ReqData, State1}
    end.

encodings_provided(ReqData, State) ->
    State1 = lookup_path(ReqData, State),
    Encodings = case z_media_identify:is_mime_compressed(State1#state.mime) of
        true -> 
            [{"identity", fun(Data) -> Data end}];
        false -> 
            [{"identity", fun(Data) -> decode_data(identity, Data) end},
             {"gzip",     fun(Data) -> decode_data(gzip, Data) end}]
    end,
    EncodeData = length(Encodings) > 1,
    {Encodings, ReqData, State1#state{encode_data=EncodeData}}.


resource_exists(ReqData, State) ->
	State1 = lookup_path(ReqData, State),
	case State1#state.path of
		none -> {false, ReqData, State1};
		_  -> {true, ReqData, State1}
	end.

charsets_provided(ReqData, State) ->
	State1 = lookup_path(ReqData, State),
    case is_text(State1#state.mime) of
        true -> {[{"utf-8", fun(X) -> X end}], ReqData, State1};
        _ -> {no_charset, ReqData, State1}
    end.
    
last_modified(ReqData, State) ->
	State1 = lookup_path(ReqData, State),
    RD1 = wrq:set_resp_header("Cache-Control", "public, max-age="++integer_to_list(?MAX_AGE), ReqData),
    case State1#state.last_modified of
        undefined -> 
            LMod = max_last_modified(State1#state.fullpaths, {{1970,1,1},{12,0,0}}),
			[LModUTC|_] = calendar:local_time_to_universal_time_dst(LMod),
            {LModUTC, RD1, State1#state{last_modified=LModUTC}};
        LModUTC ->
            {LModUTC, RD1, State1}
    end.

    %% @doc Find the latest modification time of a list of files.
    max_last_modified([], Mod) ->
        Mod;
    max_last_modified([F|Rest], Mod) ->
        LMod = filelib:last_modified(F),
        case LMod > Mod of
            true -> max_last_modified(Rest, LMod);
            false -> max_last_modified(Rest, Mod)
        end.

        
expires(ReqData, State) ->
    NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    {calendar:gregorian_seconds_to_datetime(NowSecs + ?MAX_AGE), ReqData, State}.

provide_content(ReqData, State) ->
	State1 = lookup_path(ReqData, State),
    RD1 = case State1#state.content_disposition of
        inline ->     wrq:set_resp_header("Content-Disposition", "inline", ReqData);
        attachment -> wrq:set_resp_header("Content-Disposition", "attachment", ReqData);
        undefined ->  ReqData
    end,
    {Content, State2} = case State1#state.body of
        undefined ->
            Data = [ read_data(F) || F <- State1#state.fullpaths ],
            Data1 = case State1#state.mime of
                "text/javascript" -> z_utils:combine([$;, $\n], Data);
                "application/x-javascript" -> z_utils:combine([$;, $\n], Data);
                _ -> z_utils:combine($\n, Data)
            end,
            Body = case State1#state.encode_data of 
                true -> encode_data(Data1);
                false -> Data1
            end,
            {Body, State1#state{body=Body}};
        Body -> 
            {Body, State1}
    end,
    {Content, RD1, State2}.
    
    read_data(F) ->
        {ok, Data} = file:read_file(F),
        Data.
    

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
                                        fullpaths=State#state.fullpaths,
                                        mime=State#state.mime,
                                        last_modified=State#state.last_modified,
                                        body=State#state.body
                                    },
                            Context = z_context:new(ReqData, ?MODULE),
                            z_depcache:set(cache_key(State#state.path), Cache, Context),
                            {ok, ReqData, State};
                        _ ->
                            % No cache or no gzip'ed version (file system cache is fast enough for image serving)
                            {ok, ReqData, State}
                    end
            end;
        true ->
            {ok, ReqData, State}
    end.


%%%%%%%%%%%%%% Helper functions %%%%%%%%%%%%%%
    
cache_key(Path) ->
    {resource_file, Path}.

file_exists(_State, [], _Context) ->
    false;
file_exists(State, Name, Context) ->
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
            file_exists1(State#state.root, RelName, Context)
    end.

file_exists1([], _RelName, _Context) ->
    false;
file_exists1([ModuleIndex|T], RelName, Context) when is_atom(ModuleIndex) ->
    case z_module_indexer:find(ModuleIndex, RelName, Context) of
        {ok, #module_index{filepath=File}} -> {true, File};
        {error, _} -> file_exists1(T, RelName, Context)
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



lookup_path(ReqData, State = #state{path=undefined}) ->
    Context = z_context:new(ReqData, ?MODULE),
    Path   = mochiweb_util:unquote(wrq:disp_path(ReqData)),
    Cached = case State#state.use_cache of
        true -> z_depcache:get(cache_key(Path), Context);
        _    -> undefined
    end,
    case Cached of
        undefined ->
            Paths = z_lib_include:uncollapse(Path),
            FullPaths = [ file_exists(State, P, Context) || P <- Paths ],
            FullPaths1 = [ P || {true, P} <- FullPaths ], 
            case FullPaths1 of
                [] -> State#state{path=none};
                _ -> State#state{path=Path, fullpaths=FullPaths1}
            end;
        {ok, Cache} ->
            State#state{
                 is_cached=true,
                 path=Cache#cache.path,
                 fullpaths=Cache#cache.fullpaths,
                 mime=Cache#cache.mime,
                 last_modified=Cache#cache.last_modified,
                 body=Cache#cache.body
             }
    end;
lookup_path(_ReqData, State) ->
	State.



%% Encode the data so that the identity variant comes first and then the gzip'ed variant
encode_data(Data) when is_list(Data) ->
    encode_data(iolist_to_binary(Data));
encode_data(Data) when is_binary(Data) ->
    {Data, zlib:gzip(Data)}.

decode_data(identity, {Data, _Gzip}) ->
	Data;
decode_data(gzip, {_Data, Gzip}) ->
	Gzip.
