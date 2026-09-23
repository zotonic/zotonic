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

-module(z_media_runner).
-behaviour(gen_server).

-export([start_link/0, enabled/0, register/0, register/1, unregister/1,
    authorized/2, callback/3, callback_url/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3, format_status/2]).

-define(MAX_JOBS, 1000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

enabled() ->
    z_media_runner_pool:configured().

%% @doc Register the calling process for the configured wait time (seconds).
-spec register() -> {ok, binary(), binary()} | {error, term()}.
register() ->
    case z_config:get(media_runner_wait_timeout, 43500) of
        Seconds when is_integer(Seconds), Seconds > 0, Seconds =< 4294967 ->
            ?MODULE:register(Seconds * 1000);
        _ -> {error, media_runner_configuration}
    end.

%% @doc Register a single attempt with a finite lifetime in milliseconds.
%% Call unregister/1 in an after clause; the monitor and timer are backstops.
-spec register(Timeout) -> {ok, binary(), binary()} | {error, term()} when
    Timeout :: pos_integer().
register(Timeout) when is_integer(Timeout), Timeout > 0, Timeout =< 4294967295 ->
    Id = random_id(),
    Secret = random_id(),
    case gen_server:call(?MODULE, {register, Id, digest(Secret), Timeout}) of
        ok -> {ok, Id, Secret};
        Error -> Error
    end;
register(_) -> {error, media_runner_configuration}.

random_id() ->
    Encoded = base64:encode(crypto:strong_rand_bytes(32)),
    UrlSafe = binary:replace(binary:replace(Encoded, <<"+">>, <<"-">>, [global]), <<"/">>, <<"_">>, [global]),
    binary:replace(UrlSafe, <<"=">>, <<>>, [global]).

-spec unregister(binary()) -> ok | {error, not_owner}.
unregister(Id) -> gen_server:call(?MODULE, {remove, Id}).

-spec authorized(binary(), binary()) -> boolean().
authorized(Id, Secret) when is_binary(Id), is_binary(Secret) ->
    gen_server:call(?MODULE, {authorized, Id, digest(Secret)});
authorized(_, _) -> false.

-spec callback(binary(), binary(), map()) -> ok | {error, gone}.
callback(Id, Secret, Result) when is_binary(Id), is_binary(Secret), is_map(Result) ->
    gen_server:call(?MODULE, {callback, Id, digest(Secret), Result});
callback(_, _, _) -> {error, gone}.

digest(Secret) -> crypto:hash(sha256, Secret).

%% @doc Use site configuration, never the incoming request's Host header.
callback_url(undefined) -> {error, media_runner_configuration};
callback_url(Context) ->
    SiteContext = z_context:new(z_context:site(Context)),
    Url = z_dispatcher:url_for(media_runner_callback, [{use_absolute_url, true}], SiteContext),
    case z_media_runner_protocol:https_url(Url) of
        true -> {ok, Url};
        false -> {error, media_runner_configuration}
    end.

init([]) -> {ok, #{}}.

handle_call({register, Id, Hash, Timeout}, {Pid, _}, Jobs) when map_size(Jobs) < ?MAX_JOBS ->
    Monitor = monitor(process, Pid),
    Timer = erlang:send_after(Timeout, self(), {expire, Id}),
    Job = #{pid => Pid, monitor => Monitor, timer => Timer, secret => Hash,
        deadline => erlang:monotonic_time(millisecond) + Timeout, delivered => false},
    {reply, ok, Jobs#{Id => Job}};
handle_call({register, _, _, _}, _, Jobs) ->
    {reply, {error, media_runner_busy}, Jobs};
handle_call({authorized, Id, Hash}, _, Jobs) ->
    {reply, matches(Id, Hash, Jobs), Jobs};
handle_call({callback, Id, Hash, Result}, _, Jobs) ->
    case matches(Id, Hash, Jobs) of
        true ->
            Job = maps:get(Id, Jobs),
            case maps:get(delivered, Job) of
                false -> maps:get(pid, Job) ! {media_runner_result, Id, Result};
                true -> ok
            end,
            {reply, ok, Jobs#{Id => Job#{delivered => true}}};
        false -> {reply, {error, gone}, Jobs}
    end;
handle_call({remove, Id}, {Pid, _}, Jobs) ->
    case maps:find(Id, Jobs) of
        {ok, #{pid := Pid}} -> {reply, ok, remove(Id, Jobs)};
        {ok, _} -> {reply, {error, not_owner}, Jobs};
        error -> {reply, ok, Jobs}
    end.

matches(Id, Hash, Jobs) ->
    case maps:find(Id, Jobs) of
        {ok, #{secret := Hash, deadline := Deadline, pid := Pid}} ->
            erlang:monotonic_time(millisecond) < Deadline andalso is_process_alive(Pid);
        _ -> false
    end.

remove(Id, Jobs) ->
    case maps:take(Id, Jobs) of
        {#{monitor := Monitor, timer := Timer}, Rest} ->
            demonitor(Monitor, [flush]),
            erlang:cancel_timer(Timer),
            Rest;
        error -> Jobs
    end.

handle_cast(_, Jobs) -> {noreply, Jobs}.

handle_info({expire, Id}, Jobs) -> {noreply, remove(Id, Jobs)};
handle_info({'DOWN', Monitor, process, _, _}, Jobs) ->
    Ids = [Id || {Id, #{monitor := Ref}} <- maps:to_list(Jobs), Ref =:= Monitor],
    {noreply, lists:foldl(fun remove/2, Jobs, Ids)};
handle_info(_, Jobs) -> {noreply, Jobs}.

terminate(_, Jobs) ->
    lists:foreach(fun(Id) -> remove(Id, Jobs) end, maps:keys(Jobs)),
    ok.

code_change(_, Jobs, _) -> {ok, Jobs}.

%% OTP 22 callback: never expose hashes, caller data or result messages in reports.
format_status(_, _) -> [{data, [{"State", redacted}]}].
