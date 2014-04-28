%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013-2014 Marc Worrell
%%
%% @doc Interface to static files and media storage system.

%% Copyright 2013-2014 Marc Worrell
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

-module(z_file_request).

-export([
    lookup_lib/2,
    lookup_file/2,
    lookup_file/4,
    stop/2,
    force_stale/2,
    content_encodings/1,
    content_data/2,
    content_stream/2,
    content_file/2,
    is_visible/2,

    %% Exported for webzmachine streaming
    stream_many_parts/1,
    stream_sub_stream/2
    ]).

-include_lib("zotonic.hrl").
-include_lib("zotonic_file.hrl").

% Path: request path including optional url resize filters (binary)
% Filters: optional filters to be applied to the original file

lookup_lib(Path, Context) ->
    lookup_file(Path, [lib], undefined, Context).

lookup_file(Path, Context) ->
    lookup_file(Path, [], undefined, Context).

lookup_file(Path, Root, OptFilters, Context) ->
    z_file_sup:ensure_file(z_convert:to_binary(Path), Root, OptFilters, Context).

stop(Path, Context) ->
    z_file_entry:stop(Path, Context).

force_stale(Path, Context) ->
    z_file_entry:force_stale(Path, Context).

content_encodings(#z_file_info{encodings=Encs}) ->
    [Enc || {Enc,_Parts} <- Encs].


content_file(#z_file_info{encodings=Encs}, Context) ->
    {identity, Parts} = proplists:lookup(identity, Encs),
    content_file_part(Parts, Context).

content_file_part([#part_missing{}], _Context) ->
    {error, enoent};
content_file_part([#part_file{filepath=File}], _Context) ->
    {ok, File};
content_file_part([#part_cache{cache_pid=Pid}], _Context) ->
    case filezcache:lookup_file(Pid) of
        {ok, {file, _Size, File}} ->
            {ok, File};
        {error, _} = Error ->
            Error
    end.

is_visible(#z_file_info{acls=Acls}, Context) ->
    lists:all(fun(Id) when is_integer(Id) ->
                    z_acl:rsc_visible(Id, Context);
                 ({module, Module}) ->
                    not Module:file_forbidden(z_context:get(path, Context), Context)
              end, 
              Acls).

content_data(Info, Enc) ->
    concatenate_stream(content_stream(Info, Enc), <<>>).

content_stream(Info, undefined) ->
    content_stream(Info, identity);
content_stream(#z_file_info{encodings=Encs}, ContentEncoding) ->
    CE = z_convert:to_atom(ContentEncoding),
    {CE, Parts} = proplists:lookup(CE, Encs),
    content_stream_parts(Parts).

content_stream_parts([]) ->
    <<>>;
content_stream_parts([Part]) ->
    stream_single_part(Part);
content_stream_parts(Parts) ->
    {stream, stream_many_parts(Parts)}.

stream_single_part(#part_data{data=Data}) ->
    Data;
stream_single_part(#part_file{filepath=File, size=Size}) ->
    {file, Size, File};
stream_single_part(#part_cache{cache_pid=Pid}) ->
    case filezcache:lookup(Pid) of
        {ok, Data} ->
            Data;
        Other ->
            lager:warning("Unexpected result from the filezcache: ~p", [Other]),
            <<>>
    end;
stream_single_part(#part_missing{}) ->
    <<>>.

stream_many_parts([]) ->
    {<<>>, done};
stream_many_parts([#part_data{data=Data}|Parts]) ->
    {Data, fun() -> ?MODULE:stream_many_parts(Parts) end};
stream_many_parts([#part_file{filepath=File, size=Size}|Parts]) ->
    {{file, Size, File}, fun() -> ?MODULE:stream_many_parts(Parts) end};
stream_many_parts([#part_cache{cache_pid=Pid}|Parts]) ->
    case filezcache:lookup(Pid) of
        {ok, {file, _Size, _File} = FileRef} ->
            {FileRef, fun() -> ?MODULE:stream_many_parts(Parts) end};
        {ok, {stream, Fun}} ->
            stream_sub_stream(Fun, Parts);
        Other ->
            lager:warning("Unexpected result from the filezcache: ~p", [Other]),
            stream_many_parts(Parts)
    end;
stream_many_parts([#part_missing{}|Parts]) ->
    stream_many_parts(Parts).

stream_sub_stream(Fun, Parts) ->
    case Fun() of
        {DataOrFile, done} ->
            {DataOrFile, fun() -> ?MODULE:stream_many_parts(Parts) end};
        {DataOrFile, Next} ->
            {DataOrFile, fun() -> ?MODULE:stream_sub_stream(Next, Parts) end}
    end.


concatenate_stream(B, Acc) when is_binary(B) ->
    <<Acc/binary, B/binary>>;
concatenate_stream({file, Size, Filename}, Acc) ->
    {ok, Fh} = file:open(Filename, [read,raw,binary]),
    {ok, Data} = file:read(Fh, Size),
    ok = file:close(Fh),
    <<Acc/binary, Data/binary>>;
concatenate_stream({B, done}, Acc) ->
    <<Acc/binary, B/binary>>;
concatenate_stream({B, Fun}, Acc) ->
    concatenate_stream(Fun(), <<Acc/binary, B/binary>>).

