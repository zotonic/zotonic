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
enabled() -> not lists:member(z_config:get(media_runner_url), [undefined, <<>>, ""]).

%% Do not require processing tools to be installed on a remote-only client.

-spec find_executable(string()) -> string() | false.
find_executable(Name) ->
    case enabled() of
        false ->
            os:find_executable(Name);
        true ->
            case {Name, z_config:get(media_runner_imagemagick_legacy, false)} of
                {"magick", true} -> false;
                _ -> Name
            end
    end.

-spec run(atom(), iodata(), map()) -> {ok, binary()} | {error, term()}.
run(Profile, Command, Options) ->
    case z_config:get(media_runner_url) of
        undefined ->
            z_exec:run_local(Profile, Command, Options);
        <<>> ->
            z_exec:run_local(Profile, Command, Options);
        "" ->
            z_exec:run_local(Profile, Command, Options);
        Url ->
            case remote(z_convert:to_binary(Url), Profile, Command, Options) of
                {error, {media_runner_unavailable, _}} = Error ->
                    case z_config:get(media_runner_local_fallback, false) of
                        true -> z_exec:run_local(Profile, Command, Options);
                        _ -> Error
                    end;
                Result ->
                    Result
            end
    end.

remote(Url, Profile, Command, Options) ->
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
    Secret = base64:encode(crypto:strong_rand_bytes(32)),
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
                case submit_request(Url, Token, Request) of
                    {ok, Code} when Code =:= 200; Code =:= 202 ->
                        receive
                            {media_runner_result, Id, Result} ->
                                z_media_runner_protocol:unpack(Result, Options#{
                                    media_runner_profile => maps:get(<<"profile">>, Job)
                                })
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
%% A 412 lists precisely which inputs were evicted or have never been uploaded.
submit_request(Url, Token, Request) ->
    Files = maps:get(<<"files">>, Request),
    Thin = Request#{<<"files">> => [maps:remove(<<"data">>, F) || F <- Files]},
    submit_request(Url, Token, Thin, Files, 2).
submit_request(Url, Token, Request, Files, Retries) ->
    case z_media_runner_protocol:request(Url, Token, Request) of
        {ok, 412, Body} when Retries > 0, byte_size(Body) < 8192 ->
            try z_json:decode(Body) of
                #{<<"missing">> := Missing} when is_list(Missing) ->
                    Upload = [
                        case lists:member(maps:get(<<"sha256">>, F, undefined), Missing) of
                            true -> F;
                            false -> maps:remove(<<"data">>, F)
                        end
                     || F <- Files
                    ],
                    submit_request(Url, Token, Request#{<<"files">> => Upload}, Files, Retries - 1);
                _ ->
                    {error, {protocol, invalid_cache_response}}
            catch
                _:_ -> {error, {protocol, invalid_cache_response}}
            end;
        {ok, Code, _} ->
            {ok, Code};
        {error, _} = Error ->
            Error
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
