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
-include("z_media_limits.hrl").
-export([
    start_link/0,
    run/3,
    callback/3,
    authorized/2,
    enabled/0,
    find_executable/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    format_status/1
]).

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec enabled() -> boolean().
enabled() ->
    z_media_runner_pool:configured().

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
                        #{available := true, tool := <<"magick">>} ->
                            "magick";
                        _ ->
                            false
                    end;
                "convert" ->
                    case z_media_imagemagick:selected() of
                        #{available := true, tool := <<"convert">>} ->
                            "convert";
                        _ ->
                            false
                    end;
                _ ->
                    Name
            end
    end.

-spec run(atom(), iodata(), map()) -> {ok, binary()} | {error, term()}.
run(Profile, Command, Options) ->
    case z_media_runner_pool:runners() of
        {ok, []} -> z_exec:run_local(Profile, Command, Options);
        {ok, Runners} ->
            case z_media_runner_protocol:pack(Profile, Command, Options) of
                {ok, Job} ->
                    Eligible = compatible(Profile, Runners, Options),
                    Wait = z_config:get(media_runner_wait_timeout, ?DEFAULT_RUNNER_WAIT_SECONDS),
                    Deadline = erlang:monotonic_time(second) + Wait,
                    Result = run_pool(Eligible, Job, Options, Deadline,
                        {error, {media_runner_unavailable, no_runner}}),
                    case {Result, z_config:get(media_runner_local_fallback, false)} of
                        {{error, {media_runner_unavailable, _}}, true} ->
                            z_exec:run_local(Profile, Command, Options);
                        _ -> Result
                    end;
                Error -> Error
            end;
        Error -> Error
    end.

%% Commands are constructed for the selected ImageMagick installation. Do not
%% send those arguments to a runner with a different major version or executable.
compatible(Profile, Runners, Options) when Profile =:= imagemagick; Profile =:= imagemagick_pdf ->
    Selected = case maps:find(media_runner_imagemagick, Options) of
        {ok, Installation} -> Installation;
        error -> z_media_imagemagick:selected()
    end,
    case z_media_imagemagick:installations() of
        {ok, Installed} -> [R || {R, Info} <- Installed, lists:member(R, Runners),
            compatible_imagemagick(Selected, Info)];
        {error, _} -> []
    end;
compatible(_, Runners, _) -> Runners.

compatible_imagemagick(#{major := Major, tool := Tool}, {ok, #{major := Major, tool := Tool}}) -> true;
compatible_imagemagick(_, _) -> false.

run_pool([], _Job, _Options, _Deadline, Error) -> Error;
run_pool(Runners, Job, Options, Deadline, PreviousError) ->
    Callback = callback_url(maps:get(context, Options, undefined)),
    case z_media_runner_protocol:https_url(Callback) of
        false -> {error, media_runner_configuration};
        true ->
            case erlang:monotonic_time(second) < Deadline of
                false -> {error, {media_runner_unavailable, callback_timeout}};
                true ->
                    Id = z_ids:id(32),
                    Secret = z_ids:id(44),
                    case gen_server:call(?MODULE, {select, Runners, Job, Id, Secret, self()}) of
                        {ok, #{url := Url, token := Token} = Runner} ->
                            Result = try
                                submit(Url, Token, Callback, Job,
                                    Options#{media_runner_deadline => Deadline}, Id, Secret)
                            after
                                gen_server:call(?MODULE, {remove, Id})
                            end,
                            case Result of
                                {error, {media_runner_unavailable, _}} ->
                                    gen_server:call(?MODULE, {unavailable, runner_id(Url, Token)}),
                                    run_pool(lists:delete(Runner, Runners), Job, Options, Deadline, Result);
                                _ -> Result
                            end;
                        {error, no_runner} -> PreviousError;
                        Error -> Error
                    end
            end
    end.

runner_id(Url, Token) ->
    z_media_runner_pool:identity(#{url => Url, token => Token}).

%% Use the site's canonical dispatcher context, also for background media jobs.
%% Do not derive the callback host from the incoming request's Host header.
callback_url(undefined) ->
    undefined;
callback_url(Context) ->
    SiteContext = z_context:new(z_context:site(Context)),
    z_dispatcher:url_for(media_runner_callback, [{absolute_url, true}], SiteContext).

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
            remember(Url, Token, maps:get(<<"files">>, Job)),
            case await_result(Id, Url, Token, maps:get(media_runner_deadline, Options), 0) of
                {ok, Result} ->
                    Received = z_media_runner_protocol:unpack(Result, Options#{
                        media_runner_profile => maps:get(<<"profile">>, Job),
                        media_runner_endpoint => Url,
                        media_runner_token => Token
                    }),
                    case Received of
                        {ok, _} ->
                            remember(Url, Token, maps:get(<<"files">>, Result, [])),
                            z_media_runner_protocol:request(
                                z_media_runner_protocol:control_url(Url, <<"received">>),
                                Token, #{<<"id">> => Id});
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
                    {ok, #{<<"result">> := Result}} when is_map(Result) -> {ok, Result};
                    {ok, #{<<"outcome">> := Status}} when
                        Status =:= <<"queued">>; Status =:= <<"starting">>; Status =:= <<"running">> ->
                        await_result(Id, Url, Token, Deadline, 0);
                    _ when Failures >= 2 -> {error, {media_runner_unavailable, status_unavailable}};
                    _ -> await_result(Id, Url, Token, Deadline, Failures + 1)
                end
            end
    end.

remember(Url, Token, Files) ->
    gen_server:call(?MODULE, {remember, runner_id(Url, Token), Files}).

%% Optimistic hash-only submission avoids even a preflight round trip on cache hits.
%% The missing outcome lists precisely which inputs need uploading.
submit_request(Url, Token, Request, Options, Retries) ->
    ControlUrl = z_media_runner_protocol:control_url(Url, <<"submit">>),
    case capacity_request(ControlUrl, Token, Request, maps:get(media_runner_deadline, Options)) of
        {ok, #{<<"outcome">> := <<"missing">>, <<"missing">> := Missing}}
                when Retries > 0, is_list(Missing), Missing =/= [] ->
            try
                Files = maps:get(<<"files">>, Request),
                Known = [H || #{<<"sha256">> := H} <- Files],
                true = lists:all(fun(H) -> lists:member(H, Known) end, Missing),
                gen_server:call(?MODULE, {forget, runner_id(Url, Token), Missing}),
                Paths = lists:usort(maps:get(read, Options, []) ++ maps:get(write, Options, [])),
                case upload_missing(lists:usort(Missing), Files, Paths, Url, Token,
                        maps:get(media_runner_deadline, Options)) of
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
            case deadline_request(
                z_media_runner_protocol:control_url(Url, <<"status">>), Token,
                #{<<"id">> => maps:get(<<"id">>, Request)},
                maps:get(media_runner_deadline, Options) * 1000, 5000)
            of
                {ok, #{<<"outcome">> := Status}} when
                    Status =:= <<"queued">>; Status =:= <<"starting">>; Status =:= <<"running">>;
                    Status =:= <<"completed">>; Status =:= <<"failed">> -> ok;
                _ -> control_result(Error)
            end;
        Reply ->
            control_result(Reply)
    end.

%% Upload bursts are capacity pressure, not evidence of a failed runner.
%% Retry the same reservation/job ID before pool failover, with jitter to avoid
%% synchronized callers. Allow up to a minute within the overall job deadline.
%% Waiting happens in the caller, never in the registry.
capacity_request(Url, Token, Request, JobDeadline) ->
    Deadline = min(JobDeadline * 1000, erlang:monotonic_time(millisecond) + 60000),
    capacity_request(Url, Token, Request, Deadline, 100).

capacity_request(Url, Token, Request, Deadline, Delay) ->
    Reply = deadline_request(Url, Token, Request, Deadline, 30000),
    case control_result(Reply) of
        {ok, 429} ->
            Left = Deadline - erlang:monotonic_time(millisecond),
            Wait = Delay + rand:uniform(Delay),
            case Left > Wait of
                true ->
                    timer:sleep(Wait),
                    capacity_request(Url, Token, Request, Deadline, min(500, Delay * 2));
                false -> Reply
            end;
        _ -> Reply
    end.

%% Check again after sleeping and bound the HTTP call itself, not just retries.
deadline_request(Url, Token, Request, Deadline, MaxTimeout) ->
    case Deadline - erlang:monotonic_time(millisecond) of
        Left when Left > 0 ->
            z_media_runner_protocol:request(Url, Token, Request, min(Left, MaxTimeout));
        _ ->
            {error, timeout}
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

upload_missing([], _Files, _Paths, _Url, _Token, _Deadline) -> ok;
upload_missing([Hash | Rest], Files, Paths, Url, Token, Deadline) ->
    [F | _] = [F || #{<<"sha256">> := H} = F <- Files, H =:= Hash],
    Path = lists:nth(maps:get(<<"id">>, F), Paths),
    FileUrl = <<Url/binary, "/files/", Hash/binary>>,
    case ensure_uploaded(FileUrl, Token, Path, maps:get(<<"size">>, F), Deadline) of
        ok ->
            remember(Url, Token, [F]),
            upload_missing(Rest, Files, Paths, Url, Token, Deadline);
        Error ->
            Error
    end.

%% Reserving the hash serializes concurrent clients before any file bytes are sent.
ensure_uploaded(Url, Token, Path, Size, Deadline) ->
    Hash = lists:last(binary:split(Url, <<"/">>, [global])),
    ControlUrl = z_media_runner_protocol:control_url(Url, <<"reserve">>),
    case capacity_request(ControlUrl, Token, #{<<"hash">> => Hash, <<"size">> => Size}, Deadline) of
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
            timer:sleep(100 + rand:uniform(200)),
            case erlang:monotonic_time(second) < Deadline of
                true -> ensure_uploaded(Url, Token, Path, Size, Deadline);
                false -> {error, upload_wait_timeout}
            end;
        false ->
            {error, upload_wait_timeout}
    end.

-spec authorized(binary(), binary()) -> boolean().
authorized(Id, Secret) ->
    gen_server:call(?MODULE, {authorized, Id, digest(Secret)}).

-spec callback(binary(), binary(), map()) -> ok | {error, gone}.
callback(Id, Secret, Result) ->
    gen_server:call(?MODULE, {callback, Id, digest(Secret), Result}).

digest(Secret) ->
    crypto:hash(sha256, Secret).

init([]) ->
    {ok, #{jobs => #{}, hints => #{}, cooldown => #{}}}.

%% Selection and recording expected inputs are one atomic operation. Another
%% caller cannot observe changed load without also seeing this upload reservation.
handle_call({select, Runners, Job, Id, Secret, Pid}, From, #{jobs := Jobs} = State) ->
    case handle_call({rank, Runners, Job}, From, State) of
        {reply, [], _} -> {reply, {error, no_runner}, State};
        {reply, [Runner | _], _} ->
            case handle_job_call({register, Id, Secret, Pid}, From, Jobs) of
                {reply, ok, NewJobs} ->
                    {reply, ok, Assigned} = handle_call(
                        {assign, Id, z_media_runner_pool:identity(Runner), Job}, From, State#{jobs => NewJobs}),
                    {reply, {ok, Runner}, Assigned};
                {reply, Error, _} -> {reply, Error, State}
            end
    end;
handle_call({rank, Runners, Job}, _From, #{jobs := Jobs, hints := Hints, cooldown := Cooldown} = State) ->
    Now = erlang:monotonic_time(second),
    Class = work_class(maps:get(<<"profile">>, Job)),
    Active = maps:fold(fun
        (_, #{runner := Id, profile := Profile}, Acc) ->
            case work_class(Profile) =:= Class of
                true -> Acc#{Id => maps:get(Id, Acc, 0) + 1};
                false -> Acc
            end;
        (_, _, Acc) -> Acc
    end, #{}, Jobs),
    %% In-flight inputs are expected here even before an upload finishes. This
    %% keeps concurrent commands together on the runner's existing upload lease.
    Expected = maps:fold(fun
        (_, #{runner := Id, files := Files}, Acc) ->
            lists:foldl(fun
                (#{<<"sha256">> := Hash}, A) -> A#{{Id, Hash} => Now + 1};
                (_, A) -> A
            end, Acc, Files);
        (_, _, Acc) -> Acc
    end, Hints, Jobs),
    Ready = [R || R <- Runners, maps:get(z_media_runner_pool:identity(R), Cooldown, Now) =< Now],
    {reply, z_media_runner_pool:rank(Ready, Job, Expected, Active), State};
handle_call({assign, Id, Runner, Job}, _From, #{jobs := Jobs} = State) ->
    Entry = maps:get(Id, Jobs),
    Assigned = Entry#{runner => Runner, profile => maps:get(<<"profile">>, Job),
        files => maps:get(<<"files">>, Job)},
    {reply, ok, State#{jobs => Jobs#{Id => Assigned}}};
handle_call({remember, Runner, Files}, _From, #{hints := Hints, cooldown := Cooldown} = State) ->
    {reply, ok, State#{hints => z_media_runner_pool:remember(Runner, Files, Hints),
        cooldown => maps:remove(Runner, Cooldown)}};
handle_call({forget, Runner, Hashes}, _From, #{hints := Hints} = State) ->
    {reply, ok, State#{hints => z_media_runner_pool:forget(Runner, Hashes, Hints)}};
handle_call({unavailable, Runner}, _From, #{cooldown := Cooldown} = State) ->
    Now = erlang:monotonic_time(second),
    Fresh = maps:filter(fun(_, Until) -> Until > Now end, Cooldown),
    {reply, ok, State#{cooldown => Fresh#{Runner => Now + 5}}};
handle_call(Message, From, #{jobs := Jobs} = State) ->
    {reply, Reply, NewJobs} = handle_job_call(Message, From, Jobs),
    {reply, Reply, State#{jobs => NewJobs}}.

work_class(<<"ffmpeg">>) -> ffmpeg;
work_class(_) -> general.

handle_job_call({register, Id, Secret, Pid}, _From, State) when map_size(State) < 1000 ->
    Ref = monitor(process, Pid),
    {reply, ok, State#{
        Id => #{
            pid => Pid,
            monitor => Ref,
            secret => digest(Secret),
            delivered => false
        }
    }};
handle_job_call({register, _, _, _}, _From, State) ->
    {reply, {error, media_runner_busy}, State};
handle_job_call({authorized, Id, Hash}, _From, State) ->
    {reply, matches(Id, Hash, State), State};
handle_job_call({callback, Id, Hash, Result}, _From, State) ->
    case matches(Id, Hash, State) of
        true ->
            Entry = maps:get(Id, State),
            case maps:get(delivered, Entry) of
                false ->
                    maps:get(pid, Entry) ! {media_runner_result, Id, Result};
                true ->
                    ok
            end,
            {reply, ok, State#{Id => Entry#{delivered => true}}};
        false ->
            {reply, {error, gone}, State}
    end;
handle_job_call({remove, Id}, _From, State) ->
    case maps:find(Id, State) of
        {ok, #{monitor := Ref}} ->
            demonitor(Ref, [flush]);
        error ->
            ok
    end,
    {reply, ok, maps:remove(Id, State)}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, _, _}, #{jobs := Jobs} = State) ->
    {noreply, State#{jobs => maps:filter(fun(_, #{monitor := R}) -> R =/= Ref end, Jobs)}};
handle_info(_, State) ->
    {noreply, State}.

matches(Id, Hash, State) ->
    case maps:find(Id, State) of
        {ok, #{secret := Hash}} -> true;
        _ ->
            false
    end.

%% Callback results and credentials must not enter crash reports.
format_status(Status) ->
    maps:without([message, reason], Status#{state => redacted}).
