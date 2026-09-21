%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Route media commands to a remote runner and await authenticated result callbacks.
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

-module(z_media_runner).

-moduledoc("
Remote media execution and bounded, process-monitored callback rendezvous. Availability
fallback preserves the caller's local sandbox policy.
").
-behaviour(gen_server).
-export([start_link/0, run/3, callback/3, authorized/2, enabled/0, find_executable/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, format_status/1]).

-spec start_link() -> gen_server:start_ret().
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec enabled() -> boolean().
enabled() -> not lists:member(z_config:get(media_runner_hostname), [undefined, <<>>, ""]).

%% Do not require processing tools to be installed on a remote-only client.

-spec find_executable(string()) -> string() | false.
find_executable(Name) ->
    case enabled() of
        false ->
            os:find_executable(Name);
        true ->
            case Name of
                "magick" ->
                    case z_media_imagemagick:selected() of
                        #{available := true, tool := <<"magick">>} -> "magick";
                        _ -> false
                    end;
                "convert" ->
                    case z_media_imagemagick:selected() of
                        #{available := true, tool := <<"convert">>} -> "convert";
                        _ -> false
                    end;
                _ -> Name
            end
    end.

-spec run(atom(), iodata(), map()) -> {ok, binary()} | {error, term()}.
run(Profile, Command, Options) ->
    case z_config:get(media_runner_hostname) of
        undefined ->
            z_exec:run_local(Profile, Command, Options);
        <<>> ->
            z_exec:run_local(Profile, Command, Options);
        "" ->
            z_exec:run_local(Profile, Command, Options);
        Hostname ->
            case remote(Hostname, Profile, Command, Options) of
                {error, {media_runner_unavailable, _}} = Error ->
                    case z_config:get(media_runner_local_fallback, false) of
                        true -> z_exec:run_local(Profile, Command, Options);
                        _ -> Error
                    end;
                Result ->
                    Result
            end
    end.

remote(Hostname, Profile, Command, Options) ->
    case z_media_runner_protocol:endpoint(Hostname) of
        {ok, Url} -> remote_url(Url, Profile, Command, Options);
        {error, _} = Error -> Error
    end.

remote_url(Url, Profile, Command, Options) ->
    Token = z_convert:to_binary(z_config:get(media_runner_oauth2_key, <<>>)),
    Callback = callback_url(maps:get(context, Options, undefined)),
    case
        Token =/= <<>> andalso z_media_runner_protocol:https_url(Url) andalso
            z_media_runner_protocol:https_url(Callback)
    of
        false ->
            {error, media_runner_configuration};
        true ->
            case z_media_runner_protocol:pack(Profile, Command, Options) of
                {ok, Job} -> submit(Url, Token, Callback, Job, Options);
                {error, _} = Error -> Error
            end
    end.

%% Use the site's canonical dispatcher context, also for background media jobs.
%% Do not derive the callback host from the incoming request's Host header.
callback_url(undefined) ->
    undefined;
callback_url(Context) ->
    SiteContext = z_context:new(z_context:site(Context)),
    z_dispatcher:url_for(media_runner_callback, [{absolute_url, true}], SiteContext).

submit(Url, Token, Callback, Job, Options) ->
    Id = z_ids:id(32),
    Secret = z_ids:id(44),
    Wait = z_config:get(media_runner_wait_timeout, 3900000),
    case gen_server:call(?MODULE, {register, Id, Secret, self()}) of
        ok ->
            try
                Request = Job#{
                    <<"id">> => Id,
                    <<"callback_url">> => Callback,
                    <<"callback_token">> => Secret,
                    <<"expires">> => erlang:system_time(second) + Wait div 1000
                },
                case submit_request(Url, Token, Request, Options, 2) of
                    ok ->
                        receive
                            {media_runner_result, Id, Result} ->
                                Received = z_media_runner_protocol:unpack(Result, Options#{
                                    media_runner_profile => maps:get(<<"profile">>, Job),
                                    media_runner_endpoint => Url,
                                    media_runner_token => Token
                                }),
                                case Received of
                                    {ok, _} ->
                                        %% A lost receipt merely retains the cache pin until expiry.
                                        z_media_runner_protocol:request(
                                            z_media_runner_protocol:control_url(Url, <<"received">>),
                                            Token, #{<<"id">> => Id});
                                    _ -> ok
                                end,
                                Received
                        after Wait -> {error, {media_runner_unavailable, callback_timeout}}
                        end;
                    {ok, Code} when Code =:= 429; Code =:= 502; Code =:= 503; Code =:= 504 ->
                        {error, {media_runner_unavailable, Code}};
                    {ok, Code} ->
                        {error, {media_runner_http, Code}};
                    {error, {protocol, Reason}} ->
                        {error, {media_runner_protocol, Reason}};
                    {error, Reason} ->
                        {error, {media_runner_unavailable, Reason}}
                end
            after
                gen_server:call(?MODULE, {remove, Id})
            end;
        {error, _} = Error ->
            Error
    end.

%% Optimistic hash-only submission avoids even a preflight round trip on cache hits.
%% The missing outcome lists precisely which inputs need uploading.
submit_request(Url, Token, Request, Options, Retries) ->
    ControlUrl = z_media_runner_protocol:control_url(Url, <<"submit">>),
    case z_media_runner_protocol:request(ControlUrl, Token, Request) of
        {ok, #{<<"outcome">> := <<"missing">>, <<"missing">> := Missing}}
                when Retries > 0, is_list(Missing), Missing =/= [] ->
            try
                Files = maps:get(<<"files">>, Request),
                Known = [H || #{<<"sha256">> := H} <- Files],
                true = lists:all(fun(H) -> lists:member(H, Known) end, Missing),
                Paths = lists:usort(maps:get(read, Options, []) ++ maps:get(write, Options, [])),
                case upload_missing(lists:usort(Missing), Files, Paths, Url, Token) of
                    ok -> submit_request(Url, Token, Request, Options, Retries - 1);
                    {ok, Code} -> {ok, Code};
                    {error, _} = Error -> Error
                end
            catch
                _:_ -> {error, {protocol, invalid_cache_response}}
            end;
        {ok, #{<<"outcome">> := <<"accepted">>}} -> ok;
        Reply -> control_result(Reply)
    end.

control_result({ok, #{<<"outcome">> := <<"full">>}}) -> {ok, 429};
control_result({ok, #{<<"outcome">> := <<"unavailable">>}}) -> {ok, 503};
control_result({ok, #{<<"outcome">> := <<"conflict">>}}) -> {ok, 409};
control_result({ok, _}) -> {error, {protocol, invalid_response}};
control_result({error, {http_status, Code}}) -> {ok, Code};
control_result({error, _} = Error) -> Error.

upload_missing([], _Files, _Paths, _Url, _Token) -> ok;
upload_missing([Hash | Rest], Files, Paths, Url, Token) ->
    [F | _] = [F || #{<<"sha256">> := H} = F <- Files, H =:= Hash],
    Path = lists:nth(maps:get(<<"id">>, F), Paths),
    FileUrl = <<Url/binary, "/files/", Hash/binary>>,
    Deadline = erlang:monotonic_time(second) + 3600,
    case ensure_uploaded(FileUrl, Token, Path, maps:get(<<"size">>, F), Deadline) of
        ok -> upload_missing(Rest, Files, Paths, Url, Token);
        Error -> Error
    end.

%% Reserving the hash serializes concurrent clients before any file bytes are sent.
ensure_uploaded(Url, Token, Path, Size, Deadline) ->
    Hash = lists:last(binary:split(Url, <<"/">>, [global])),
    ControlUrl = z_media_runner_protocol:control_url(Url, <<"reserve">>),
    case z_media_runner_protocol:request(ControlUrl, Token, #{<<"hash">> => Hash, <<"size">> => Size}) of
        {ok, #{<<"outcome">> := <<"present">>}} -> ok;
        {ok, #{<<"outcome">> := <<"upload">>, <<"upload_token">> := Lease}} ->
            case z_media_runner_protocol:upload(Url, Token, Lease, Path, Size) of
                {ok, 204} -> ok;
                {ok, 409} -> wait_for_upload(Url, Token, Path, Size, Deadline);
                Error -> Error
            end;
        {ok, #{<<"outcome">> := <<"busy">>}} -> wait_for_upload(Url, Token, Path, Size, Deadline);
        Reply -> control_result(Reply)
    end.

%% A busy hash or an expired claim must return to reservation, never resend bytes
%% using the old token. The next POST either finds the file or elects one uploader.
wait_for_upload(Url, Token, Path, Size, Deadline) ->
    case erlang:monotonic_time(second) < Deadline of
        true ->
            timer:sleep(1000),
            ensure_uploaded(Url, Token, Path, Size, Deadline);
        false -> {error, upload_wait_timeout}
    end.

-spec authorized(binary(), binary()) -> boolean().
authorized(Id, Secret) -> gen_server:call(?MODULE, {authorized, Id, digest(Secret)}).

-spec callback(binary(), binary(), map()) -> ok | {error, gone}.
callback(Id, Secret, Result) -> gen_server:call(?MODULE, {callback, Id, digest(Secret), Result}).
digest(Secret) -> crypto:hash(sha256, Secret).

init([]) -> {ok, #{}}.
handle_call({register, Id, Secret, Pid}, _From, State) when map_size(State) < 1000 ->
    Ref = monitor(process, Pid),
    {reply, ok, State#{
        Id => #{pid => Pid, monitor => Ref, secret => digest(Secret), delivered => false}
    }};
handle_call({register, _, _, _}, _From, State) ->
    {reply, {error, media_runner_busy}, State};
handle_call({authorized, Id, Hash}, _From, State) ->
    {reply, matches(Id, Hash, State), State};
handle_call({callback, Id, Hash, Result}, _From, State) ->
    case matches(Id, Hash, State) of
        true ->
            Entry = maps:get(Id, State),
            case maps:get(delivered, Entry) of
                false -> maps:get(pid, Entry) ! {media_runner_result, Id, Result};
                true -> ok
            end,
            {reply, ok, State#{Id => Entry#{delivered => true}}};
        false ->
            {reply, {error, gone}, State}
    end;
handle_call({remove, Id}, _From, State) ->
    case maps:find(Id, State) of
        {ok, #{monitor := Ref}} -> demonitor(Ref, [flush]);
        error -> ok
    end,
    {reply, ok, maps:remove(Id, State)}.
handle_cast(_, State) -> {noreply, State}.
handle_info({'DOWN', Ref, process, _, _}, State) ->
    {noreply, maps:filter(fun(_, #{monitor := R}) -> R =/= Ref end, State)};
handle_info(_, State) ->
    {noreply, State}.
matches(Id, Hash, State) ->
    case maps:find(Id, State) of
        {ok, #{secret := Hash}} -> true;
        _ -> false
    end.

%% Callback results and credentials must not enter crash reports.
format_status(Status) -> maps:without([message, reason], Status#{state => redacted}).
