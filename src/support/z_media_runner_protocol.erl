%% @copyright 2026 Marc Worrell
%% @doc Endpoint and HTTP policy for the media runner client.

%% Copyright 2026 Marc Worrell
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

-module(z_media_runner_protocol).

-export([endpoint/1, endpoint/2, http_options/1, callback_limit/0, https_url/1, pack/3, unpack/2, unpack/3, validate/1, rewrite/2,
    hash_file/1, hash_file/2, input_limit/0, output_limit/0, profile/1,
    control_url/2, http_url/1, request/3, request/4, upload/5, hex/1]).

-include_lib("kernel/include/file.hrl").
-include("z_media_limits.hrl").

%% Match the protocol's maximum encoded result envelope; reject invalid settings.
callback_limit() ->
    case z_config:get(media_runner_max_callback_bytes, 135266304) of
        Limit when is_integer(Limit), Limit > 0, Limit =< 135266304 -> Limit;
        _ -> error(media_runner_configuration)
    end.

https_url(<<"https://", _/binary>> = Url) when byte_size(Url) =< 2048 ->
    case uri_string:parse(Url) of
        #{host := Host} = Parts ->
            byte_size(Host) > 0 andalso not maps:is_key(userinfo, Parts)
                andalso not maps:is_key(fragment, Parts);
        _ -> false
    end;
https_url(_) -> false.

%% @doc A hostname with optional port, using HTTPS by default.
-spec endpoint(Hostname) -> Result when
    Hostname :: binary() | string(),
    Result :: {ok, binary()} | {error, media_runner_configuration}.
endpoint(Hostname) ->
    endpoint(Hostname, <<"https">>).

%% @doc Accept only an authority, not a URL, path, query or embedded credentials.
-spec endpoint(Hostname, Protocol) -> Result when
    Hostname :: binary() | string(),
    Protocol :: binary() | string(),
    Result :: {ok, binary()} | {error, media_runner_configuration}.
endpoint(Hostname, Protocol) ->
    try
        Scheme = text(Protocol),
        true = Scheme =:= <<"https">> orelse Scheme =:= <<"http">>,
        Host = text(Hostname),
        true = byte_size(Host) > 0 andalso byte_size(Host) =< 253,
        Base = <<Scheme/binary, "://", Host/binary>>,
        #{scheme := Scheme, host := ParsedHost, path := <<>>} = Parts = uri_string:parse(Base),
        true = byte_size(ParsedHost) > 0,
        true = maps:without([scheme, host, path, port], Parts) =:= #{},
        Port = maps:get(port, Parts, default_port(Scheme)),
        true = is_integer(Port) andalso Port > 0 andalso Port =< 65535,
        {ok, <<Base/binary, "/media-runner/jobs">>}
    catch
        _:_ -> {error, media_runner_configuration}
    end.

text(Value) when is_binary(Value) -> Value;
text(Value) when is_list(Value) -> unicode:characters_to_binary(Value).

default_port(<<"https">>) -> 443;
default_port(<<"http">>) -> 80.

%% @doc Runner TLS deliberately does not verify certificates in any environment.
%% Redirects remain disabled so bearer credentials stay with the configured runner.
-spec http_options(Timeout) -> list() when Timeout :: pos_integer().
http_options(Timeout) when is_integer(Timeout), Timeout > 0 ->
    [
        {autoredirect, false},
        {ssl, [{verify, verify_none}]},
        {connect_timeout, 5000},
        {timeout, Timeout}
    ].

-spec pack(atom(), iodata(), map()) -> {ok, map()} | {error, term()}.
pack(Profile, Command, Options) ->
    try
        Reads = lists:usort(maps:get(read, Options, [])),
        Writes = lists:usort(maps:get(write, Options, [])),
        Paths = lists:usort(Reads ++ Writes),
        true = length(Paths) =< ?MAX_JOB_FILECOUNT,
        Bindings = lists:zip(Paths, lists:seq(1, length(Paths))),
        Files = [
            pack_file(P, N, lists:member(P, Reads), lists:member(P, Writes))
         || {P, N} <- Bindings
        ],
        Replacements = [{escaped(P), marker(N)} || {P, N} <- Bindings],
        Cd =
            case maps:find(cd, Options) of
                {ok, Dir} ->
                    [{escaped(Dir), <<"__ZMR_CWD__">>}];
                error ->
                    []
            end,
        Cmd = rewrite(unicode:characters_to_binary(Command), Replacements ++ Cd),
        Job = #{
            <<"version">> => 3,
            <<"profile">> => atom_to_binary(Profile, utf8),
            <<"command">> => Cmd,
            <<"files">> => Files,
            <<"timeout">> => maps:get(timeout, Options, maps:get(timeout, z_exec:profile(Profile)))
        },
        ok = validate(Job),
        {ok, Job}
    catch
        _:_ ->
            {error, media_runner_invalid_input}
    end.

pack_file(Path, N, Read, Write) ->
    Extension = z_convert:to_binary(filename:extension(Path)),
    F = #{
        <<"id">> => N,
        <<"write">> => Write,
        <<"extension">> => Extension
    },
    case Read of
        false ->
            F;
        true ->
            {ok, Size, Hash} = hash_file(Path),
            F#{<<"size">> => Size, <<"sha256">> => Hash}
    end.

%% Reuse the incremental file hash; the protocol adds its size and type checks.
-spec hash_file(file:filename_all()) -> {ok, non_neg_integer(), binary()} | {error, term()}.
hash_file(Path) ->
    hash_file(Path, input_limit()).

-spec hash_file(file:filename_all(), pos_integer()) -> {ok, non_neg_integer(), binary()} | {error, term()}.
hash_file(Path, Limit) ->
    case file:read_file_info(Path) of
        {ok, #file_info{type = regular, size = Size}} when Size =< Limit ->
            case sha256_file(Path) of
                {ok, Hash} ->
                    %% Reject files that changed size while being hashed.
                    case file:read_file_info(Path) of
                        {ok, #file_info{type = regular, size = Size}} ->
                            {ok, Size, Hash};
                        _ ->
                            {error, invalid_file}
                    end;
                {error, _} = Error ->
                    Error
            end;
        {ok, _} ->
            {error, invalid_file};
        {error, _} = Error ->
            Error
    end.

%% Replace longest paths first, in one pass: replacements cannot rewrite each other.

-spec rewrite(binary(), [{binary(), binary()}]) -> binary().
rewrite(Text, []) ->
    Text;
rewrite(Text, Replacements) ->
    Patterns = lists:sort(
        fun(A, B) -> byte_size(A) > byte_size(B) end,
        lists:usort([P || {P, _} <- Replacements])
    ),
    rewrite_matches(Text, binary:matches(Text, Patterns), maps:from_list(Replacements), 0, []).

rewrite_matches(Text, [], _Map, Pos, Acc) ->
    iolist_to_binary(lists:reverse([binary:part(Text, Pos, byte_size(Text) - Pos) | Acc]));
rewrite_matches(Text, [{Start, Len} | Rest], Map, Pos, Acc) ->
    Prefix = binary:part(Text, Pos, Start - Pos),
    Key = binary:part(Text, Start, Len),
    rewrite_matches(Text, Rest, Map, Start + Len, [maps:get(Key, Map), Prefix | Acc]).

escaped(Path) ->
    Quoted = unicode:characters_to_binary(z_utils:os_filename(unicode:characters_to_list(Path))),
    binary:part(Quoted, 1, byte_size(Quoted) - 2).

marker(N) ->
    iolist_to_binary(["__ZMR_FILE_", integer_to_binary(N), "__"]).

-spec profile(binary()) -> atom().
profile(<<"file">>) -> file;
profile(<<"imagemagick">>) -> imagemagick;
profile(<<"imagemagick_pdf">>) -> imagemagick_pdf;
profile(<<"ffmpeg">>) -> ffmpeg;
profile(<<"ffmpeg_preview">>) -> ffmpeg_preview;
profile(<<"ffprobe">>) -> ffprobe.

%% Remote callers may request longer than a default, within the profile ceiling.
max_timeout(ffmpeg) -> ?MAX_JOB_TIMEOUT;
max_timeout(ffmpeg_preview) -> ?MAX_PREVIEW_TIMEOUT;
max_timeout(imagemagick) -> ?MAX_IMAGE_TIMEOUT;
max_timeout(imagemagick_pdf) -> ?MAX_IMAGE_TIMEOUT;
max_timeout(ffprobe) -> ?MAX_PROBE_TIMEOUT;
max_timeout(file) -> ?MAX_FILE_TIMEOUT.

output_limit(Profile) ->
    min(output_limit(), maps:get(file_size, z_exec:profile(Profile))).

-spec validate(term()) -> ok | {error, invalid_job}.
validate(#{
    <<"version">> := 3,
    <<"profile">> := P,
    <<"command">> := Cmd,
    <<"files">> := Files,
    <<"timeout">> := Timeout
}) ->
    try
        Profile = profile(P),
        true = is_binary(Cmd) andalso byte_size(Cmd) > 0 andalso byte_size(Cmd) =< ?MAX_JOB_CMDSIZE,
        true = is_integer(Timeout) andalso Timeout > 0 andalso Timeout =< max_timeout(Profile),
        true = is_list(Files) andalso length(Files) =< ?MAX_JOB_FILECOUNT,
        Ids = [
            begin
                #{<<"id">> := Id, <<"write">> := Write} = F,
                true = is_integer(Id) andalso Id > 0 andalso Id =< ?MAX_JOB_FILECOUNT,
                true = is_boolean(Write),
                Extension = maps:get(<<"extension">>, F, <<>>),
                true = is_binary(Extension) andalso byte_size(Extension) =< 17,
                match = re:run(Extension, <<"^(\\.[a-zA-Z0-9]{1,16})?$">>, [{capture, none}]),
                false = maps:is_key(<<"data">>, F),
                case maps:find(<<"sha256">>, F) of
                    {ok, Hash} when is_binary(Hash), byte_size(Hash) =:= 64 ->
                        match = re:run(Hash, <<"^[0-9a-f]{64}$">>, [{capture, none}]),
                        Size = maps:get(<<"size">>, F),
                        true = is_integer(Size) andalso Size >= 0 andalso Size =< input_limit();
                    error ->
                        false = maps:is_key(<<"data">>, F)
                end,
                Id
            end
         || F <- Files
        ],
        true = length(Ids) =:= length(lists:usort(Ids)),
        ok
    catch
        _:_ ->
            {error, invalid_job}
    end;
validate(_) ->
    {error, invalid_job}.

-spec unpack(map(), map()) -> {ok, binary()} | {error, term()}.
unpack(Result, Options) ->
    unpack(Result, Options, fun z_media_runner_download:fetch/3).

-spec unpack(map(), map(), function()) -> {ok, binary()} | {error, term()}.
unpack(#{<<"status">> := <<"error">>, <<"error">> := Reason}, _Options, _Fetch) when is_binary(Reason) ->
    {error, {media_runner_processing, Reason}};
unpack(#{<<"status">> := <<"ok">>, <<"stdout">> := Stdout, <<"files">> := Files}, Options, Fetch) ->
    try
        Paths = lists:usort(maps:get(read, Options, []) ++ maps:get(write, Options, [])),
        Bindings = lists:zip(lists:seq(1, length(Paths)), Paths),
        Expected = [{N, P} || {N, P} <- Bindings, lists:member(P, maps:get(write, Options, []))],
        true = lists:sort([maps:get(<<"id">>, F) || F <- Files]) =:= [N || {N, _} <- Expected],
        Profile = profile(maps:get(media_runner_profile, Options, <<"ffmpeg">>)),
        Defaults = z_exec:profile(Profile),
        true = lists:sum([maps:get(<<"size">>, F) || F <- Files]) =< output_limit(Profile),
        Out = base64:decode(Stdout),
        true = byte_size(Out) =< min(maps:get(max_size, Defaults), maps:get(max_size, Options, maps:get(max_size, Defaults))),
        ok = z_media_runner_download:install(Files, Expected, Options, Fetch),
        RestoredOut = rewrite(Out, [{marker(N), stdout_path(P, Options)} || {N, P} <- Bindings]),
        {ok, RestoredOut}
    catch
        throw:{media_runner_download, Reason} ->
            {error, {media_runner_unavailable, {download, Reason}}};
        _:_ ->
            {error, media_runner_invalid_result}
    end;
unpack(_, _, _) ->
    {error, media_runner_invalid_result}.

stdout_path(Path, #{media_runner_profile := <<"ffprobe">>}) ->
    Quoted = jsx:encode(unicode:characters_to_binary(Path)),
    binary:part(Quoted, 1, byte_size(Quoted) - 2);
stdout_path(Path, _) ->
    unicode:characters_to_binary(Path).

%% @doc Locate a control operation on the standard model API, alongside streaming routes.
-spec control_url(binary(), binary()) -> binary().
control_url(Url, Operation) ->
    Parts = uri_string:parse(Url),
    Result = uri_string:recompose(maps:without([query, fragment], Parts#{
        path => case Operation of
            <<"capabilities">> -> <<"/api/model/mediarunner_job/get/capabilities">>;
            _ ->
                <<"/api/model/mediarunner_job/post/", Operation/binary>>
        end
    })),
    true = is_binary(Result),
    Result.

%% @doc Validate HTTP(S) URLs for administrator-configured runner traffic.
-spec http_url(term()) -> boolean().
http_url(Url) when is_binary(Url), byte_size(Url) =< ?MAX_URL_SIZE ->
    try uri_string:parse(Url) of
        #{scheme := Scheme, host := Host} = Parts when Scheme =:= <<"https">>; Scheme =:= <<"http">> ->
            byte_size(Host) > 0 andalso not maps:is_key(userinfo, Parts) andalso
                not maps:is_key(fragment, Parts);
        _ ->
            false
    catch
        _:_ ->
            false
    end;
http_url(_) ->
    false.

%% The file descriptor is a file server, since the HTTPS worker invokes the body
%% generator from its own process. Bounded reads and socket sends apply backpressure.
-spec upload(binary(), binary(), binary(), file:filename_all(), non_neg_integer()) ->
    {ok, integer()} | {error, term()}.
upload(Url, Token, Lease, Path, Size) ->
    case http_url(Url) of
        false ->
            {error, invalid_url};
        true ->
            case file:open(Path, [read, binary]) of
                {ok, Fd} ->
                    try
                        Request = {
                            binary_to_list(Url),
                            [{"authorization", "Bearer " ++ binary_to_list(Token)},
                             {"x-upload-token", binary_to_list(Lease)},
                             {"connection", "close"},
                             {"content-length", integer_to_list(Size)}],
                            "application/octet-stream",
                            {fun upload_chunk/1, {Fd, Size}}
                        },
                        %% A dedicated connection keeps long uploads independent
                        %% of job requests and callbacks to the same host.
                        case z_media_runner_http:request(put, Request, ?UPLOAD_TIMEOUT) of
                            {ok, Status, _} ->
                                {ok, Status};
                            {error, _} = Error ->
                                Error
                        end
                    after
                        file:close(Fd)
                    end;
                {error, _} = Error ->
                    Error
            end
    end.

upload_chunk({_Fd, 0}) -> eof;
upload_chunk({Fd, Left}) ->
    {ok, Data} = file:read(Fd, min(1048576, Left)),
    {ok, Data, {Fd, Left - byte_size(Data)}}.


input_limit() -> byte_limit(media_runner_max_input_bytes).
output_limit() -> byte_limit(media_runner_max_output_bytes).
byte_limit(Key) ->
    case z_config:get(Key, ?DEFAULT_MEDIA_LIMIT) of
        N when is_integer(N), N > 0 -> N;
        _ -> error(media_runner_configuration)
    end.

sha256_file(Path) ->
    case file:open(Path, [read, raw, binary]) of
        {ok, Fd} ->
            try hash_chunks(Fd, crypto:hash_init(sha256)) after file:close(Fd) end;
        Error -> Error
    end.
hash_chunks(Fd, Hash) ->
    case file:read(Fd, 1048576) of
        eof -> {ok, hex(crypto:hash_final(Hash))};
        {ok, Bin} -> hash_chunks(Fd, crypto:hash_update(Hash, Bin));
        Error -> Error
    end.
hex(Bin) ->
    << <<(hex_digit(B bsr 4)), (hex_digit(B band 15))>> || <<B>> <= Bin >>.
hex_digit(N) when N < 10 -> $0 + N;
hex_digit(N) -> $a + N - 10.

request(Url, Token, Payload) -> request(Url, Token, Payload, 30000).
request(Url, Token, Payload, Timeout) ->
    case http_url(Url) of
        false -> {error, invalid_url};
        true ->
            Request = {binary_to_list(Url),
                [{"authorization", "Bearer " ++ binary_to_list(Token)}],
                "application/json", jsx:encode(Payload)},
            case z_media_runner_http:request(post, Request, Timeout, callback_limit()) of
                {ok, Code, Body} when Code >= 200, Code < 300 ->
                    try jsx:decode(Body, [return_maps]) of
                        #{<<"status">> := <<"ok">>, <<"result">> := Map} when is_map(Map) -> {ok, Map};
                        Map when is_map(Map), map_size(Map) =:= 0 -> {ok, Map};
                        _ -> {error, invalid_response}
                    catch _:_ -> {error, invalid_response}
                    end;
                {ok, Code, _} -> {error, {http_status, Code}};
                Error -> Error
            end
    end.
