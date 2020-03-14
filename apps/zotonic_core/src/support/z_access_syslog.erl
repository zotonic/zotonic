%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2015 Maas-Maarten Zeeman
%% @doc An access logger which gets its log entries from an ets buffer and writes them to syslog

%% Copyright 2015 Maas-Maarten Zeeman
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

-module(z_access_syslog).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

%% Api

-export([start_link/0, start_link/4, log_access/1]).

-include_lib("zotonic.hrl").

%% gen_server exports

-export([init/2, handle_value/4, handle_flush_done/2]).

-record(state, {
    priority,
    log
}).

-define(FLUSH_INTERVAL, 1000).

%%
%% Api
%%

start_link() ->
    Ident = z_config:get(syslog_ident),
    Opts = z_config:get(syslog_opts),
    Facility = z_config:get(syslog_facility),
    Level = z_config:get(syslog_level),

    start_link(Ident, Opts, Facility, Level).

start_link(Ident, Opts, Facility, Level) ->
    z_buffered_worker:start_link(?MODULE, ?MODULE, [[Ident, Opts, Facility], Level]).

log_access(#{}=MetricData) ->
    ReqInfo = maps:get(req, MetricData, #{}),
    Headers = maps:get(headers, ReqInfo, #{}),
    {IP, _} = maps:get(peer, ReqInfo, {undefined, undefined}),
    UserData = maps:get(user_data, MetricData, #{}),

    MD = #{
      site => maps:get(site, UserData, '-'),
      dispatch_rule => maps:get(dispatch_rule, UserData, '-'),
      user => maps:get(user, UserData, <<$->>),

      size => maps:get(resp_body_length, MetricData, 0),
      req_start => maps:get(req_start, MetricData, 0),

      status => maps:get(resp_status, MetricData, '-'),
      remote_ip => IP,

      path => maps:get(path, ReqInfo, <<$->>),
      method => maps:get(method, ReqInfo, <<$->>),
      version => maps:get(version, ReqInfo, '-'),

      user_agent => maps:get(<<"user-agent">>, Headers, <<$->>),
      referer => maps:get(<<"referer">>, Headers, <<$->>)
     },
    z_buffered_worker:push(?MODULE, MD).

%%
%% Buffered worker callbacks
%%

% @doc Initialize the logger.
init(_Pid, [[Name, Opts, Facility], Priority]) ->
    {ok, Log} = syslog:open(Name, Opts, Facility),
    {ok, ?FLUSH_INTERVAL, #state{priority=Priority, log=Log}}.

% @doc Log one entry
handle_value(_Pid, _Count, MD, #state{log=Log, priority=Priority}) ->
    Msg = format(MD),
    syslog:log(Log, Priority, Msg).

% @doc Flush operation done.
handle_flush_done(_Pid, _State) ->
    ok.


%%
%% Helpers
%%

% @doc Get a second resolution timestamp from a monotonic time sample.
monotonic_time_to_timestamp(MonotonicTime) ->
    Time = erlang:convert_time_unit(MonotonicTime, native, second) + erlang:time_offset(second),
    MegaSecs = Time div 1000000,
    Secs = Time rem 1000000,
    {MegaSecs, Secs, 0}.

% @doc Format the reguest. We use a format similar to apache's vhost_commont
% The site and dispatch rule is also included in the log line.
format(#{ site := Site,
          dispatch_rule := Dispatch, 
          req_start := ReqStart,
          path := Path,
          method := Method,
          status := Status,
          size := Size,
          user := User,
          remote_ip := RemoteIP,
          referer := Referer,
          user_agent := UserAgent,
          version := Version
        }) ->

    StartTime = monotonic_time_to_timestamp(ReqStart),

    fmt(z_convert:to_binary(Site),
        z_convert:to_binary(Dispatch),
        fmt_time(StartTime),
        fmt_ip(RemoteIP),
        User,
        Method,
        sanitize(Path),
        z_convert:to_binary(Version),
        z_convert:to_binary(Status),
        z_convert:to_binary(Size),
        sanitize(Referer),
        sanitize(UserAgent)).

% @doc Format the information in a similar format as the apache log format.
fmt(Site, DispatchRule, Time, Ip, User, Method, Path, Version, Status,  Length, Referrer, UserAgent) ->
    [Site, $:, DispatchRule, $\s,
     Ip, $\s,
     User, $\s,
     Time, $\s,
     $", Method, $\s, Path, $\s, Version, $", $\s,
     Status, $\s,
     Length, $\s,
     $", Referrer, $", $\s,
     $", UserAgent, $"].

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
