%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Validate media job envelopes and transfer explicit files for sandboxed execution.
%% @end

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

-moduledoc("
Versioned media job envelopes. Only explicit input/output files cross the boundary. No
remote path is ever used as a local output path. Cache paths are used only by the
trusted Erlang staging step; sandbox grants contain private job copies, never the cache.
").
-export([
    pack/3,
    hash_file/1,
    hash_file/2,
    output_limit/0,
    http_options/1,
    control_url/2,
    input_limit/0,
    upload/5,
    validate/1,
    execute/1,
    execute/2,
    execute/3,
    execute/4,
    unpack/2,
    unpack/3,
    callback_limit/0,
    https_url/1,
    endpoint/1,
    post/3,
    request/3,
    request/4,
    profile/1,
    rewrite/2
]).
-include_lib("kernel/include/file.hrl").

-include("z_media_limits.hrl").

-spec input_limit() -> pos_integer().
input_limit() ->
    z_config:get(media_runner_max_input_bytes, ?DEFAULT_MEDIA_LIMIT).

-spec output_limit() -> pos_integer().
output_limit() ->
    z_config:get(media_runner_max_output_bytes, ?DEFAULT_MEDIA_LIMIT).

%% @doc Maximum encoded callback JSON bytes, reserved per starting/running job.
%% File transfers have independent input/output limits and do not consume this budget.
-spec callback_limit() -> pos_integer().
callback_limit() ->
    z_config:get(media_runner_max_callback_bytes, ?DEFAULT_JSON_LIMIT).

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
            case z_crypto:hex_sha2_file(Path) of
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
    Quoted = unicode:characters_to_binary(z_filelib:os_filename(unicode:characters_to_list(Path))),
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

-spec execute(map()) -> map().
execute(Job) -> execute(Job, fun(_) -> {error, missing} end).

-spec execute(map(), fun((map()) -> {ok, {file, file:filename_all()}} | {error, term()})) -> map().
execute(Job, Resolve) ->
    execute(Job, Resolve, fun(_, _) -> error(result_storage_required) end).

%% Persist outputs before the private staging directory is removed.
-spec execute(map(), function(), function()) -> map().
execute(Job, Resolve, Store) ->
    Dir = z_convert:to_list(z_tempfile:new()) ++ "-mediarunner",
    execute(Job, Resolve, Store, Dir).

%% @doc Execute in a caller-owned staging path, removed on normal completion or failure.
-spec execute(map(), function(), function(), file:filename_all()) -> map().
execute(Job, Resolve, Store, Dir) ->
    try
        ok = validate(Job),
        ok = file:make_dir(Dir),
        ok = file:change_mode(Dir, 8#700),
        execute_staged(Job, Dir, Resolve, Store)
    catch
        _:_ ->
            #{<<"status">> => <<"error">>, <<"error">> => <<"processing_failed">>}
    after
        file:del_dir_r(Dir)
    end.

execute_staged(
    #{
        <<"profile">> := Profile,
        <<"command">> := Command,
        <<"files">> := Files,
        <<"timeout">> := Timeout
    },
    Dir,
    Resolve,
    Store
) ->
    Paths = [
        {
            maps:get(<<"id">>, F),
            filename:join(
                Dir,
                integer_to_list(maps:get(<<"id">>, F)) ++
                    binary_to_list(maps:get(<<"extension">>, F, <<>>))
            )
        }
     || F <- Files
    ],
    lists:foreach(
        fun(F) ->
            case maps:find(<<"sha256">>, F) of
                {ok, _Hash} ->
                    {ok, {file, Cached}} = Resolve(F),
                    %% Cache paths must never become sandbox grants. Copy only this
                    %% job's inputs; never hard-link, as commands may modify read/write inputs.
                    {ok, Size} = file:copy(Cached, proplists:get_value(maps:get(<<"id">>, F), Paths)),
                    Size = maps:get(<<"size">>, F);
                error ->
                    ok
            end
        end,
        Files
    ),
    %% Grant individual staged files, not their directory or the source cache.
    Read = [
        proplists:get_value(maps:get(<<"id">>, F), Paths)
     || F <- Files, maps:is_key(<<"sha256">>, F)
    ],
    Write = [
        proplists:get_value(maps:get(<<"id">>, F), Paths)
     || F <- Files, maps:get(<<"write">>, F)
    ],
    Cmd = rewrite(
        Command,
        [{marker(N), escaped(Path)} || {N, Path} <- Paths] ++ [{<<"__ZMR_CWD__">>, escaped(Dir)}]
    ),
    ProfileName = profile(Profile),
    Defaults = z_exec:profile(ProfileName),
    OutputLimit = output_limit(ProfileName),
    case
        z_exec:run_sandbox(ProfileName, Cmd, #{
            read => Read,
            write => Write,
            cd => Dir,
            timeout => Timeout,
            max_size => maps:get(max_size, Defaults),
            file_size => OutputLimit
        })
    of
        {ok, Stdout} ->
            true = lists:sum([output_size(Path) || Path <- Write]) =< OutputLimit,
            Output = [store_output(N, Path, Store) || {N, Path} <- Paths, lists:member(Path, Write)],
            PortableStdout = rewrite(Stdout, [
                {unicode:characters_to_binary(Path), marker(N)}
             || {N, Path} <- Paths
            ]),
            #{
                <<"status">> => <<"ok">>,
                <<"stdout">> => base64:encode(PortableStdout),
                <<"files">> => Output
            };
        {error, Reason} ->
            %% Do not return sandbox stderr or host paths to clients or the dashboard.
            #{<<"status">> => <<"error">>, <<"error">> => error_code(Reason)}
    end.

output_size(Path) ->
    {ok, #file_info{type = regular, size = Size}} = file:read_link_info(Path),
    Size.

store_output(Id, Path, Store) ->
    {ok, Size, Hash} = hash_file(Path, output_limit()),
    Store(Path, #{
        <<"id">> => Id,
        <<"size">> => Size,
        <<"sha256">> => Hash
    }).

error_code(timeout) -> <<"command_timeout">>;
error_code(output_limit) -> <<"output_limit">>;
error_code(_) -> <<"command_failed">>.

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
    Quoted = z_json:encode(unicode:characters_to_binary(Path)),
    binary:part(Quoted, 1, byte_size(Quoted) - 2);
stdout_path(Path, _) ->
    unicode:characters_to_binary(Path).

%% @doc Build the fixed runner endpoint from a hostname, optionally with an HTTPS port.
-spec endpoint(binary() | string()) -> {ok, binary()} | {error, media_runner_configuration}.
endpoint(Hostname) ->
    try
        Host = z_convert:to_binary(Hostname),
        true = byte_size(Host) > 0 andalso byte_size(Host) =< ?MAX_HOSTNAME_SIZE,
        Base = <<"https://", Host/binary>>,
        #{scheme := <<"https">>, host := ParsedHost, path := <<>>} = Parts = uri_string:parse(Base),
        true = byte_size(ParsedHost) > 0,
        true = maps:without([scheme, host, path, port], Parts) =:= #{},
        Port = maps:get(port, Parts, 443),
        true = is_integer(Port) andalso Port > 0 andalso Port =< 65535,
        {ok, <<Base/binary, "/media-runner/jobs">>}
    catch
        _:_ ->
            {error, media_runner_configuration}
    end.

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

-spec https_url(term()) -> boolean().
https_url(Url) when is_binary(Url), byte_size(Url) =< ?MAX_URL_SIZE ->
    try uri_string:parse(Url) of
        #{scheme := <<"https">>, host := Host} = Parts ->
            byte_size(Host) > 0 andalso not maps:is_key(userinfo, Parts) andalso
                not maps:is_key(fragment, Parts);
        _ ->
            false
    catch
        _:_ ->
            false
    end;
https_url(_) ->
    false.

%% Only administrator-approved HTTPS destinations. Redirects MUST NOT receive credentials.

%% @doc Send a JSON callback and report whether the receiver accepted it.
-spec post(binary(), binary(), map()) -> ok | {error, term()}.
post(Url, Token, Payload) ->
    case request(Url, Token, Payload) of
        {ok, _} -> ok;
        {error, _} = Error ->
            Error
    end.

-spec request(binary(), binary(), map()) -> {ok, map()} | {error, term()}.
request(Url, Token, Payload) ->
    request(Url, Token, Payload, 30000).

%% @doc Exchange small JSON control messages; source and result files use streaming HTTP.
-spec request(binary(), binary(), map(), pos_integer()) -> {ok, map()} | {error, term()}.
request(Url, Token, Payload, Timeout) ->
    case https_url(Url) of
        false ->
            {error, invalid_url};
        true ->
            Options = [
                {autoredirect, false},
                {content_type, <<"application/json">>},
                {timeout, Timeout},
                {max_length, ?DEFAULT_JSON_LIMIT},
                {insecure, z_config:get(environment) =:= development}
            ] ++ case Token of
                <<>> ->
                    [];
                _ ->
                    [{authorization, <<"Bearer ", Token/binary>>}]
            end,
            case z_fetch:fetch_json(post, Url, Payload, Options, undefined) of
                {ok, #{<<"status">> := <<"ok">>, <<"result">> := Map}} when is_map(Map) ->
                    {ok, Map};
                {ok, Map} when map_size(Map) =:= 0 ->
                    {ok, Map};
                {ok, _} ->
                    {error, invalid_response};
                {error, {Code, _, _, _, _}} ->
                    {error, {http_status, Code}};
                {error, _} = Error ->
                    Error
            end
    end.

%% The file descriptor is a file server, since the HTTPS worker invokes the body
%% generator from its own process. Bounded reads and socket sends apply backpressure.
-spec upload(binary(), binary(), binary(), file:filename_all(), non_neg_integer()) ->
    {ok, integer()} | {error, term()}.
upload(Url, Token, Lease, Path, Size) ->
    case https_url(Url) of
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

%% @doc Use the trusted system environment for all media-runner HTTPS traffic.
%% Development permits self-signed peers; every other environment verifies TLS.
-spec http_options(pos_integer()) -> list().
http_options(Timeout) ->
    Ssl = case z_config:get(environment) of
        development ->
            [{verify, verify_none}];
        _ ->
            verified_ssl_options()
    end,
    [{autoredirect, false}, {ssl, Ssl}, {connect_timeout, ?CONNECT_TIMEOUT}, {timeout, Timeout}].

verified_ssl_options() ->
    [
        {verify, verify_peer},
        {cacerts, tls_certificate_check:trusted_authorities()},
        {customize_hostname_check, [
            {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
        ]}
    ].
