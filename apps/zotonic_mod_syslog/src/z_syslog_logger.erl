%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2015-2021 Maas-Maarten Zeeman
%% @doc An access logger which gets its log entries from an ets buffer and writes them to syslog

%% Copyright 2015-2021 Maas-Maarten Zeeman
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

-module(z_syslog_logger).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

%% Api
-behaviour(gen_server).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    log/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-record(state, {
    priority,
    log
}).

%% ------------------------------------------------------------------------------------
%% Api
%% ------------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% @doc Format the reguest. We use a format similar to apache's vhost_commont
% The site and dispatch rule is also included in the log line.
log(#http_log_access{
        timestamp = StartTime,
        status = Status,
        method = Method,
        metrics = #{
            site := Site,
            http_version := Version,
            user_agent := UserAgent,
            referer := Referer,
            path := Path,
            resp_bytes := Size,
            metrics := UserMetrics
        }
    }) ->
    Dispatch = maps:get(dispatch_rule, UserMetrics, '-'),
    UserId = maps:get(user_id, UserMetrics, <<$->>),
    PeerIP = maps:get(peer_ip, UserMetrics, undefined),
    Msg = fmt(z_convert:to_binary(Site),
        z_convert:to_binary(Dispatch),
        fmt_time(StartTime),
        fmt_ip(PeerIP),
        UserId,
        Method,
        sanitize(Path),
        z_convert:to_binary(Version),
        z_convert:to_binary(Status),
        z_convert:to_binary(Size),
        sanitize(Referer),
        sanitize(UserAgent)),
    gen_server:call(?MODULE, {log, Msg});
log(#http_log_access{ metrics = Metrics }) ->
    lager:debug("z_syslog_logger: could not log ~p", [ Metrics ]),
    ok.

%% ------------------------------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------------------------------

% @doc Initialize the logger.
init([]) ->
    Ident = z_config:get(syslog_ident),
    Opts = z_config:get(syslog_opts),
    Facility = z_config:get(syslog_facility),
    Level = z_config:get(syslog_level),
    {ok, Log} = syslog:open(Ident, Opts, Facility),
    {ok, #state{
        priority = Level,
        log = Log
    }}.

handle_call({log, Message}, _From, #state{ log = Log, priority = Priority } = State) ->
    syslog:log(Log, Priority, Message),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


%% ------------------------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------------------------

% @doc Format the information in a similar format as the apache log format.
fmt(Site, DispatchRule, Time, Ip, User, Method, Path, Version, Status,  Length, Referrer, UserAgent) ->
    iolist_to_binary([
        Site, $:, DispatchRule, $\s,
        Ip, $\s,
        z_convert:to_binary(User), $\s,
        Time, $\s,
        $", Method, $\s, Path, $\s, Version, $", $\s,
        Status, $\s,
        Length, $\s,
        $", Referrer, $", $\s,
        $", UserAgent, $"
    ]).

% @doc Format the ip address
fmt_ip(IP) when is_tuple(IP) ->
    inet_parse:ntoa(IP);
fmt_ip(undefined) ->
    <<"0.0.0.0">>;
fmt_ip(HostName) ->
    HostName.

% @doc Format the time
fmt_time({_MegaSecs, _Secs, _MicroSecs}=Ts) ->
    fmt_time(calendar:now_to_universal_time(Ts));
fmt_time({{Year, Month, Date}, {Hour, Min, Sec}}) ->
    io_lib:format("[~2..0w/~s/~4..0w:~2..0w:~2..0w:~2..0w ~s]",
        [Date, month(Month), Year, Hour, Min, Sec, <<"+0000">>]).

% @doc Prevent escape characters to be shown in the log file.
% Seealso http://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2009-4487
sanitize(undefined) -> <<$->>;
sanitize(S) -> sanitize(S, <<>>).

sanitize(<<>>, Acc) ->
    Acc;
sanitize(<<C, Rest/binary>>, Acc) when C < 32 ->
    sanitize(Rest, <<Acc/binary, (C+$A), $^>>);
sanitize(<<C, Rest/binary>>, Acc) ->
    sanitize(Rest, <<Acc/binary, C>>).

% @doc Get a short name for the month
month(1) -> <<"Jan">>;
month(2) -> <<"Feb">>;
month(3) -> <<"Mar">>;
month(4) -> <<"Apr">>;
month(5) -> <<"May">>;
month(6) -> <<"Jun">>;
month(7) -> <<"Jul">>;
month(8) -> <<"Aug">>;
month(9) -> <<"Sep">>;
month(10) -> <<"Oct">>;
month(11) -> <<"Nov">>;
month(12) -> <<"Dec">>.
