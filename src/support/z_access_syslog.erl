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

-export([start_link/0, start_link/4, log_access/2]).

-include_lib("zotonic.hrl").

%% gen_server exports

-export([init/2, handle_value/4, handle_flush_done/2]).

-record(state, {
    priority,
    format,
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

log_access(Request, {response, StatusCode, Headers, _Body}) ->
    LogData = Request#{status_code => StatusCode, response_headers => Headers},
    z_buffered_worker:push(?MODULE, LogData).

%%
%% Buffered worker callbacks
%%

%% @doc Initialize the logger.
init(_Pid, [[Name, Opts, Facility], Priority]) ->
    {ok, Log} = syslog:open(Name, Opts, Facility),
    {ok, ?FLUSH_INTERVAL, #state{priority=Priority, format=alog, log=Log}}.

%% @doc Log one entry
handle_value(_Pid, _Count, MD, #state{log=Log, priority=Priority, format=Format}) ->
    Msg = format(Format, MD),
    ok = syslog:log(Log, Priority, Msg).

%% @doc Flush operation done.
handle_flush_done(_Pid, _State) ->
    ok.

format(alog, #{
    streamid := StreamId, peer := {RemoteIP, _RemotePort}, method := Method, path := Path, version := Version,
    headers := Headers, status_code := StatusCode
}) ->
    fmt_alog(
        <<>>,   %%        fmt_time(StartTime),
        StreamId,
        RemoteIP,
        <<>>,
        Method,
        Path,
        Version,
        StatusCode,  %% z_convert:to_binary(Status),
        <<>>,  %% z_convert:to_binary(Size),
        maps:get(<<"referer">>, Headers, <<>>),
        maps:get(<<"user-agent">>, Headers, <<>>)
    ).


%%fmt_time({_MegaSecs, _Secs, _MicroSecs}=Ts) ->
%%    fmt_time(calendar:now_to_universal_time(Ts));
%%fmt_time({{Year, Month, Date}, {Hour, Min, Sec}}) ->
%%    io_lib:format("[~2..0w/~s/~4..0w:~2..0w:~2..0w:~2..0w ~s]",
%%        [Date, month(Month), Year, Hour, Min, Sec, "+0000"]).

fmt_alog(Time, StreamId, IP, User, Method, Path, Version, StatusCode, Length, Referrer, UserAgent) ->
    <<(fmt_ip(IP))/binary, " - ",
        User/binary, " ",
        Time/binary,  " ",
        (sanitize(Method))/binary, " ",
        (sanitize(Path))/binary, " ",
        (z_convert:to_binary(Version))/binary, " ",
        (z_convert:to_binary(StatusCode))/binary, " ",
        Length/binary, " ",
        (sanitize(Referrer))/binary, " ",
        (sanitize(UserAgent))/binary, " ",
        (z_convert:to_binary(StreamId))/binary>>.

fmt_ip(IP) when is_tuple(IP) ->
    z_convert:to_binary(inet_parse:ntoa(IP));
fmt_ip(undefined) ->
    <<"0.0.0.0">>;
fmt_ip(HostName) ->
    HostName.

% @doc Prevent escape characters to be shown in the log file.
% Seealso http://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2009-4487
sanitize(S) ->
    sanitize(S, <<>>).

sanitize(<<>>, Acc) ->
    Acc;
sanitize(<<C, Rest/binary>>, Acc) when C < 32 ->
    sanitize(Rest, <<Acc/binary, (C+$A), $^>>);
sanitize(<<C, Rest/binary>>, Acc) ->
    sanitize(Rest, <<Acc/binary, C>>).

%%month(1) -> <<"Jan">>;
%%month(2) -> <<"Feb">>;
%%month(3) -> <<"Mar">>;
%%month(4) -> <<"Apr">>;
%%month(5) -> <<"May">>;
%%month(6) -> <<"Jun">>;
%%month(7) -> <<"Jul">>;
%%month(8) -> <<"Aug">>;
%%month(9) -> <<"Sep">>;
%%month(10) -> <<"Oct">>;
%%month(11) -> <<"Nov">>;
%%month(12) -> <<"Dec">>.
