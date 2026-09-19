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
remote path is ever used as a local output path.
").
-export([
    pack/3,
    validate/1,
    execute/1, execute/2,
    unpack/2,
    limit/0,
    body_limit/0,
    https_url/1,
    post/3,
    request/3,
    profile/1,
    rewrite/2
]).
-include_lib("kernel/include/file.hrl").

-spec limit() -> pos_integer().
limit() -> z_config:get(media_runner_max_bytes, 67108864).

-spec body_limit() -> pos_integer().
body_limit() -> 2 * limit() + 1048576.

-spec pack(atom(), iodata(), map()) -> {ok, map()} | {error, term()}.
pack(Profile, Command, Options) ->
    try
        Reads = lists:usort(maps:get(read, Options, [])),
        Writes = lists:usort(maps:get(write, Options, [])),
        Paths = lists:usort(Reads ++ Writes),
        true = length(Paths) =< 32,
        Bindings = lists:zip(Paths, lists:seq(1, length(Paths))),
        Files = [
            pack_file(P, N, lists:member(P, Reads), lists:member(P, Writes))
         || {P, N} <- Bindings
        ],
        true =
            lists:sum([byte_size(maps:get(<<"data">>, F, <<>>)) || F <- Files]) =<
                (limit() * 4 div 3 + 128),
        Replacements = [{escaped(P), marker(N)} || {P, N} <- Bindings],
        Cd =
            case maps:find(cd, Options) of
                {ok, Dir} -> [{escaped(Dir), <<"__ZMR_CWD__">>}];
                error -> []
            end,
        Cmd = rewrite(iolist_to_binary(Command), Replacements ++ Cd),
        Job = #{
            <<"version">> => 1,
            <<"profile">> => atom_to_binary(Profile, utf8),
            <<"command">> => Cmd,
            <<"files">> => Files,
            <<"timeout">> => maps:get(timeout, Options, 3600000)
        },
        ok = validate(Job),
        {ok, Job}
    catch
        _:_ -> {error, media_runner_invalid_input}
    end.

pack_file(Path, N, Read, Write) ->
    Extension = z_convert:to_binary(filename:extension(Path)),
    F = #{<<"id">> => N, <<"write">> => Write, <<"extension">> => Extension},
    case Read of
        false ->
            F;
        true ->
            {ok, #file_info{type = regular, size = Size}} = file:read_link_info(Path),
            true = Size =< limit(),
            {ok, Data} = file:read_file(Path),
            true = byte_size(Data) =< limit(),
            F#{
                <<"data">> => base64:encode(Data),
                <<"sha256">> => binary:encode_hex(crypto:hash(sha256, Data), lowercase)
            }
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
    Quoted = iolist_to_binary(z_filelib:os_filename(Path)),
    binary:part(Quoted, 1, byte_size(Quoted) - 2).
marker(N) -> iolist_to_binary(["__ZMR_FILE_", integer_to_binary(N), "__"]).

-spec profile(binary()) -> atom().
profile(<<"file">>) -> file;
profile(<<"imagemagick">>) -> imagemagick;
profile(<<"imagemagick_pdf">>) -> imagemagick_pdf;
profile(<<"ffmpeg">>) -> ffmpeg;
profile(<<"ffprobe">>) -> ffprobe.

-spec validate(term()) -> ok | {error, invalid_job}.
validate(#{
    <<"version">> := 1,
    <<"profile">> := P,
    <<"command">> := Cmd,
    <<"files">> := Files,
    <<"timeout">> := Timeout
}) ->
    try
        _ = profile(P),
        true = is_binary(Cmd) andalso byte_size(Cmd) > 0 andalso byte_size(Cmd) =< 65536,
        true = is_integer(Timeout) andalso Timeout > 0 andalso Timeout =< 3600000,
        true = is_list(Files) andalso length(Files) =< 32,
        Ids = [
            begin
                #{<<"id">> := Id, <<"write">> := Write} = F,
                true = is_integer(Id) andalso Id > 0 andalso Id =< 32,
                true = is_boolean(Write),
                Extension = maps:get(<<"extension">>, F, <<>>),
                true = is_binary(Extension) andalso byte_size(Extension) =< 17,
                match = re:run(Extension, <<"^(\\.[a-zA-Z0-9]{1,16})?$">>, [{capture, none}]),
                Data = maps:get(<<"data">>, F, <<>>),
                true = is_binary(Data),
                case maps:find(<<"sha256">>, F) of
                    {ok, Hash} when is_binary(Hash), byte_size(Hash) =:= 64 ->
                        match = re:run(Hash, <<"^[0-9a-f]{64}$">>, [{capture, none}]);
                    error ->
                        false = maps:is_key(<<"data">>, F)
                end,
                Id
            end
         || F <- Files
        ],
        true = length(Ids) =:= length(lists:usort(Ids)),
        true =
            lists:sum([byte_size(maps:get(<<"data">>, F, <<>>)) || F <- Files]) =<
                (limit() * 4 div 3 + 128),
        ok
    catch
        _:_ -> {error, invalid_job}
    end;
validate(_) ->
    {error, invalid_job}.

-spec execute(map()) -> map().
execute(Job) -> execute(Job, fun(_) -> {error, missing} end).

-spec execute(map(), fun((map()) -> {ok, binary()} | {error, term()})) -> map().
execute(Job, Resolve) ->
    Dir = z_convert:to_list(z_tempfile:new()) ++ "-mediarunner",
    try
        ok = validate(Job),
        ok = file:make_dir(Dir),
        ok = file:change_mode(Dir, 8#700),
        execute(Job, Dir, Resolve)
    catch
        _:_ -> #{<<"status">> => <<"error">>, <<"error">> => <<"processing_failed">>}
    after
        file:del_dir_r(Dir)
    end.

execute(
    #{
        <<"profile">> := Profile,
        <<"command">> := Command,
        <<"files">> := Files,
        <<"timeout">> := Timeout
    },
    Dir,
    Resolve
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
    lists:foldl(
        fun(F, Bytes) ->
            case maps:find(<<"sha256">>, F) of
                {ok, Hash} ->
                    Data =
                        case maps:find(<<"data">>, F) of
                            {ok, Encoded} ->
                                base64:decode(Encoded);
                            error ->
                                {ok, Cached} = Resolve(F),
                                Cached
                        end,
                    Hash = binary:encode_hex(crypto:hash(sha256, Data), lowercase),
                    Total = Bytes + byte_size(Data),
                    true = Total =< limit(),
                    ok = file:write_file(proplists:get_value(maps:get(<<"id">>, F), Paths), Data, [
                        exclusive
                    ]),
                    Total;
                error ->
                    Bytes
            end
        end,
        0,
        Files
    ),
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
    case
        z_exec:run_sandbox(profile(Profile), Cmd, #{
            read => Read,
            write => Write,
            cd => Dir,
            timeout => Timeout,
            max_size => 16777216,
            file_size => limit()
        })
    of
        {ok, Stdout} ->
            true = lists:sum([output_size(Path) || Path <- Write]) =< limit(),
            Output = [
                #{<<"id">> => N, <<"data">> => base64:encode(read_output(Path))}
             || {N, Path} <- Paths, lists:member(Path, Write)
            ],
            true =
                lists:sum([byte_size(maps:get(<<"data">>, F)) || F <- Output]) =<
                    (limit() * 4 div 3 + 128),
            PortableStdout = rewrite(Stdout, [
                {z_convert:to_binary(Path), marker(N)}
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
read_output(Path) ->
    true = output_size(Path) =< limit(),
    {ok, Data} = file:read_file(Path),
    Data.
error_code(timeout) -> <<"command_timeout">>;
error_code(output_limit) -> <<"output_limit">>;
error_code(_) -> <<"command_failed">>.

-spec unpack(map(), map()) -> {ok, binary()} | {error, term()}.
unpack(#{<<"status">> := <<"error">>, <<"error">> := Reason}, _Options) when is_binary(Reason) ->
    {error, {media_runner_processing, Reason}};
unpack(#{<<"status">> := <<"ok">>, <<"stdout">> := Stdout, <<"files">> := Files}, Options) ->
    try
        Paths = lists:usort(maps:get(read, Options, []) ++ maps:get(write, Options, [])),
        Bindings = lists:zip(lists:seq(1, length(Paths)), Paths),
        Expected = [{N, P} || {N, P} <- Bindings, lists:member(P, maps:get(write, Options, []))],
        true = lists:sort([maps:get(<<"id">>, F) || F <- Files]) =:= [N || {N, _} <- Expected],
        Decoded = [
            {
                proplists:get_value(maps:get(<<"id">>, F), Expected),
                base64:decode(maps:get(<<"data">>, F))
            }
         || F <- Files
        ],
        true = lists:sum([byte_size(D) || {_, D} <- Decoded]) =< limit(),
        Out = base64:decode(Stdout),
        true = byte_size(Out) =< maps:get(max_size, Options, 16777216),
        lists:foreach(fun({Path, Data}) -> ok = file:write_file(Path, Data) end, Decoded),
        RestoredOut = rewrite(Out, [{marker(N), stdout_path(P, Options)} || {N, P} <- Bindings]),
        {ok, RestoredOut}
    catch
        _:_ -> {error, media_runner_invalid_result}
    end;
unpack(_, _) ->
    {error, media_runner_invalid_result}.

stdout_path(Path, #{media_runner_profile := <<"ffprobe">>}) ->
    Quoted = z_json:encode(z_convert:to_binary(Path)),
    binary:part(Quoted, 1, byte_size(Quoted) - 2);
stdout_path(Path, _) ->
    z_convert:to_binary(Path).

-spec https_url(term()) -> boolean().
https_url(Url) when is_binary(Url), byte_size(Url) =< 2048 ->
    try uri_string:parse(Url) of
        #{scheme := <<"https">>, host := Host} = Parts ->
            byte_size(Host) > 0 andalso not maps:is_key(userinfo, Parts) andalso
                not maps:is_key(fragment, Parts);
        _ ->
            false
    catch
        _:_ -> false
    end;
https_url(_) ->
    false.

%% Only administrator-approved HTTPS destinations. Redirects MUST NOT receive credentials.

-spec post(binary(), binary(), map()) -> {ok, integer()} | {error, term()}.
post(Url, Token, Payload) ->
    case request(Url, Token, Payload) of
        {ok, Code, _Body} -> {ok, Code};
        {error, _} = Error -> Error
    end.

-spec request(binary(), binary(), map()) -> {ok, integer(), binary()} | {error, term()}.
request(Url, Token, Payload) ->
    case https_url(Url) of
        false ->
            {error, invalid_url};
        true ->
            Request = {
                binary_to_list(Url),
                [{"authorization", "Bearer " ++ binary_to_list(Token)}],
                "application/json",
                z_json:encode(Payload)
            },
            Trust =
                case z_config:get(media_runner_cacertfile) of
                    undefined -> {cacerts, certifi:cacerts()};
                    File -> {cacertfile, z_convert:to_list(File)}
                end,
            Ssl = [
                {verify, verify_peer},
                Trust,
                {customize_hostname_check, [
                    {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
                ]}
            ],
            case
                httpc:request(
                    post,
                    Request,
                    [
                        {autoredirect, false},
                        {ssl, Ssl},
                        {connect_timeout, 5000},
                        {timeout, 30000}
                    ],
                    [{body_format, binary}],
                    zotonic
                )
            of
                {ok, {{_, Status, _}, _, Body}} -> {ok, Status, Body};
                {error, _} = Error -> Error
            end
    end.
