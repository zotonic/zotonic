%% @copyright 2026 Marc Worrell
%% @doc Bounded client-side rendezvous for authenticated media runner callbacks.

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

-module(z_media_runner_job).
-export([run/4, run/5]).

%% @doc Resolve callbacks from canonical site configuration, including background jobs.
run(Profile, Command, Options, Runners) ->
    case z_media_runner:callback_url(maps:get(context, Options, undefined)) of
        {ok, Callback} -> run(Profile, Command, Options, Runners, Callback);
        Error ->
            lager:warning("Media runner callback configuration failed: ~p", [error_reason(Error)]),
            Error
    end.

%% @doc Execute with an already resolved, trusted callback URL.
run(Profile, Command, Options, Runners, Callback) ->
    Result = run_job(Profile, Command, Options, Runners, Callback),
    case Result of
        {ok, _} -> ok;
        {error, _} ->
            lager:warning("Media runner job failed (profile ~p): ~p", [Profile, error_reason(Result)])
    end,
    Result.

run_job(Profile, Command, Options, Runners, Callback) ->
    Wait = z_config:get(media_runner_wait_timeout, 43500),
    case z_media_runner_protocol:https_url(Callback) andalso is_integer(Wait)
            andalso Wait > 0 andalso Wait =< 4294967 of
        false -> {error, media_runner_configuration};
        true ->
            case z_media_runner_protocol:pack(Profile, Command, Options) of
                {ok, Job} ->
                    Eligible = compatible(Profile, Runners, Options),
                    Ranked = z_media_runner_pool:rank(Eligible, Job, #{}, #{}),
                    Deadline = erlang:monotonic_time(second) + Wait,
                    run_pool(Ranked, Job, Options, Callback, Deadline,
                        {error, {media_runner_unavailable, no_runner}});
                Error -> Error
            end
    end.

compatible(Profile, Runners, Options) when Profile =:= imagemagick; Profile =:= imagemagick_pdf ->
    Selected = maps:get(media_runner_imagemagick, Options, z_media_imagemagick:selected()),
    case z_media_imagemagick:installations() of
        {ok, Installed} -> [R || {R, {ok, Info}} <- Installed, lists:member(R, Runners),
            maps:get(major, Info, undefined) =:= maps:get(major, Selected, missing),
            maps:get(tool, Info, undefined) =:= maps:get(tool, Selected, missing)];
        _ -> []
    end;
compatible(_, Runners, _) -> Runners.

run_pool([], _, _, _, _, Error) -> Error;
run_pool([#{url := Url, token := Token} | Rest], Job, Options, Callback, Deadline, _) ->
    Left = Deadline - erlang:monotonic_time(second),
    case Left > 0 of
        false -> {error, {media_runner_unavailable, callback_timeout}};
        true ->
            case z_media_runner:register(Left * 1000) of
                {ok, Id, Secret} ->
                    Started = erlang:monotonic_time(millisecond),
                    lager:debug("Media runner submitting job ~s (profile ~s) to ~s",
                        [Id, maps:get(<<"profile">>, Job), Url]),
                    Result = try
                        submit(Url, Token, Callback, Job,
                            Options#{media_runner_deadline => Deadline}, Id, Secret)
                    after
                        z_media_runner:unregister(Id),
                        receive {media_runner_result, Id, _} -> ok after 0 -> ok end
                    end,
                    Elapsed = erlang:monotonic_time(millisecond) - Started,
                    lager:debug("Media runner job ~s on ~s finished in ~p ms: ~p",
                        [Id, Url, Elapsed, error_reason(Result)]),
                    case Result of
                        {error, {media_runner_unavailable, _}} ->
                            case Rest of
                                [] -> ok;
                                _ -> lager:warning("Media runner job ~s on ~s unavailable (~p); trying next runner",
                                    [Id, Url, error_reason(Result)])
                            end,
                            run_pool(Rest, Job, Options, Callback, Deadline, Result);
                        _ -> Result
                    end;
                Error -> Error
            end
    end.

submit(Url, Token, Callback, Job, Options, Id, Secret) ->
    WaitSeconds = max(1, maps:get(media_runner_deadline, Options) - erlang:monotonic_time(second)),
    Request = Job#{
        <<"id">> => Id,
        <<"callback_url">> => Callback,
        <<"callback_token">> => Secret,
        <<"expires">> => erlang:system_time(second) + WaitSeconds
    },
    case submit_request(Url, Token, Request, Options, 2) of
        ok ->
            case await_result(Id, Url, Token, maps:get(media_runner_deadline, Options), 0) of
                {ok, Result} ->
                    Received = z_media_runner_protocol:unpack(Result, Options#{
                        media_runner_profile => maps:get(<<"profile">>, Job),
                        media_runner_endpoint => Url,
                        media_runner_token => Token
                    }),
                    case Received of
                        {ok, _} ->
                            Ack = z_media_runner_protocol:request(
                                z_media_runner_protocol:control_url(Url, <<"received">>),
                                Token, #{<<"id">> => Id}),
                            case Ack of
                                {ok, _} -> ok;
                                _ -> lager:warning("Media runner receipt acknowledgement failed for job ~s on ~s: ~p",
                                    [Id, Url, error_reason(Ack)])
                            end;
                        _ -> ok
                    end,
                    Received;
                Error -> Error
            end;
        {ok, Code} when Code =:= 429; Code =:= 502; Code =:= 503; Code =:= 504 ->
            {error, {media_runner_unavailable, Code}};
        {ok, Code} ->
            {error, {media_runner_http, Code}};
        {error, {protocol, Reason}} ->
            {error, {media_runner_protocol, Reason}};
        {error, Reason} ->
            {error, {media_runner_unavailable, Reason}}
    end.

%% A callback is the fast path. Polling recovers lost callbacks and detects a
%% failed runner during long renders. Three failed probes allow another attempt;
%% the old attempt's registration is removed before accepting replacement results.
await_result(Id, Url, Token, Deadline, Failures) ->
    Left = Deadline - erlang:monotonic_time(second),
    case Left =< 0 of
        true -> {error, {media_runner_unavailable, callback_timeout}};
        false ->
            receive
                {media_runner_result, Id, Result} -> {ok, Result}
            after min(15, Left) * 1000 ->
                Reply = z_media_runner_protocol:request(
                    z_media_runner_protocol:control_url(Url, <<"status">>), Token, #{<<"id">> => Id}, 5000),
                case Reply of
                    {ok, #{<<"result">> := Result}} when is_map(Result) ->
                        lager:debug("Media runner recovered result by polling for job ~s on ~s", [Id, Url]),
                        {ok, Result};
                    {ok, #{<<"outcome">> := Status}} when
                        Status =:= <<"queued">>; Status =:= <<"starting">>; Status =:= <<"running">> ->
                        await_result(Id, Url, Token, Deadline, 0);
                    _ when Failures >= 2 -> {error, {media_runner_unavailable, status_unavailable}};
                    _ -> await_result(Id, Url, Token, Deadline, Failures + 1)
                end
            end
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
                lager:debug("Media runner job ~s on ~s needs ~p input uploads",
                    [maps:get(<<"id">>, Request), Url, length(Missing)]),
                Paths = lists:usort(maps:get(read, Options, []) ++ maps:get(write, Options, [])),
                case upload_missing(lists:usort(Missing), Files, Paths, Url, Token) of
                    ok ->
                        submit_request(Url, Token, Request, Options, Retries - 1);
                    {ok, Code} ->
                        {ok, Code};
                    {error, _} = Error ->
                        Error
                end
            catch
                _:_ ->
                    {error, {protocol, invalid_cache_response}}
            end;
        {ok, #{<<"outcome">> := <<"accepted">>}} -> ok;
        {error, _} = Error ->
            %% A failed response does not prove the submission failed. Recover
            %% admission on this runner before creating an independent attempt.
            case z_media_runner_protocol:request(
                z_media_runner_protocol:control_url(Url, <<"status">>), Token,
                #{<<"id">> => maps:get(<<"id">>, Request)}, 5000)
            of
                {ok, #{<<"outcome">> := Status}} when
                    Status =:= <<"queued">>; Status =:= <<"starting">>; Status =:= <<"running">>;
                    Status =:= <<"completed">>; Status =:= <<"failed">> ->
                        lager:debug("Media runner recovered submission by polling for job ~s on ~s",
                            [maps:get(<<"id">>, Request), Url]),
                        ok;
                _ -> control_result(Error)
            end;
        Reply ->
            control_result(Reply)
    end.

control_result({ok, #{<<"outcome">> := <<"full">>}}) ->
    {ok, 429};
control_result({ok, #{<<"outcome">> := <<"unavailable">>}}) ->
    {ok, 503};
control_result({ok, #{<<"outcome">> := <<"conflict">>}}) ->
    {ok, 409};
control_result({ok, _}) ->
    {error, {protocol, invalid_response}};
control_result({error, {http_status, Code}}) ->
    {ok, Code};
control_result({error, _} = Error) ->
    Error.

upload_missing([], _Files, _Paths, _Url, _Token) -> ok;
upload_missing([Hash | Rest], Files, Paths, Url, Token) ->
    [F | _] = [F || #{<<"sha256">> := H} = F <- Files, H =:= Hash],
    Path = lists:nth(maps:get(<<"id">>, F), Paths),
    FileUrl = <<Url/binary, "/files/", Hash/binary>>,
    Deadline = erlang:monotonic_time(second) + 3600,
    case ensure_uploaded(FileUrl, Token, Path, maps:get(<<"size">>, F), Deadline) of
        ok ->
            upload_missing(Rest, Files, Paths, Url, Token);
        Error ->
            Error
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
                {ok, 409} ->
                    wait_for_upload(Url, Token, Path, Size, Deadline);
                Error ->
                    Error
            end;
        {ok, #{<<"outcome">> := <<"busy">>}} ->
            wait_for_upload(Url, Token, Path, Size, Deadline);
        Reply ->
            control_result(Reply)
    end.

%% A busy hash or an expired claim must return to reservation, never resend bytes
%% using the old token. The next POST either finds the file or elects one uploader.
wait_for_upload(Url, Token, Path, Size, Deadline) ->
    case erlang:monotonic_time(second) < Deadline of
        true ->
            timer:sleep(1000),
            ensure_uploaded(Url, Token, Path, Size, Deadline);
        false ->
            {error, upload_wait_timeout}
    end.

%% Log only error categories, never remote error text, command output, request
%% tuples (which can contain credentials), or file paths.
error_reason({ok, _}) -> ok;
error_reason({error, Reason}) -> error_reason(Reason);
error_reason({Category, Reason}) when
        Category =:= media_runner_unavailable; Category =:= media_runner_http;
        Category =:= media_runner_protocol; Category =:= download ->
    {Category, error_reason(Reason)};
error_reason(Reason) when is_atom(Reason); is_integer(Reason) -> Reason;
error_reason(Reason) when is_tuple(Reason), tuple_size(Reason) > 0, is_atom(element(1, Reason)) ->
    element(1, Reason);
error_reason(_) -> unknown.
