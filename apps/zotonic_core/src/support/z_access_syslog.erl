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

log_access(#{}=MetricData) ->
    ?DEBUG(maps:keys(MetricData)),

    Size = maps:get(resp_body_length, MetricData, 0),

    ReqInfo = maps:get(req, MetricData, #{}),

    ReqStart = maps:get(req_start, MetricData, 0),
    StartTime = monotonic_time_to_timestamp(ReqStart),

    Path = maps:get(path, ReqInfo, undefined),
    Method = maps:get(method, ReqInfo, undefined),
    Version = maps:get(version, ReqInfo, undefined),

    Headers = maps:get(headers, ReqInfo, #{}),

    UserAgent = maps:get(<<"user-agent">>, Headers, undefined),
    Referer = maps:get(<<"referer">>, Headers, undefined),

    UserData = maps:get(user_data, MetricData, #{}),
    Site = maps:get(site, UserData, undefined),
    Dispatch = maps:get(dispatch_rule, UserData, undefined),

    % ?DEBUG(MetricData),

    ?DEBUG({z_convert:to_binary(fmt_time(StartTime)), Version, Method, Site, Dispatch, Path, Size, Referer, UserAgent}),

    ok.

monotonic_time_to_timestamp(Time) ->
    T = erlang:convert_time_unit(Time, native, second) + erlang:time_offset(second),

    MegaSecs = TO div 1000000,
    Secs =     TO rem 1000000 ,
    MicroSecs = 0,

    {MegaSecs, Secs, MicroSecs}.

% log_access(#wm_log_data{start_time=StartTime, finish_time=FinishTime,
%         method=Method, response_code=Status,
%         path=Path, headers=Headers, response_length=Size}) ->
%         
%     MD = [{start_time, StartTime},
%         {finish_time, FinishTime},
%         {method, Method},
%         {status, Status},
%         {path, z_convert:to_binary(Path)},
%         {size, Size},
%         {user_agent, get_header_value("User-Agent", Headers)},
%         {referer, get_header_value("Referer", Headers)}
%         | lager:md()],
%     z_buffered_worker:push(?MODULE, MD).

%%
%% Buffered worker callbacks
%%

% @doc Initialize the logger.
init(_Pid, [[Name, Opts, Facility], Priority]) ->
    {ok, Log} = syslog:open(Name, Opts, Facility),
    {ok, ?FLUSH_INTERVAL, #state{priority=Priority, format=alog, log=Log}}.

% @doc Log one entry
handle_value(_Pid, _Count, MD, #state{log=Log, priority=Priority, format=Format}) ->
    Msg = format(Format, MD),
    syslog:log(Log, Priority, Msg).

% @doc Flush operation done.
handle_flush_done(_Pid, _State) ->
    ok.


%%
%% Helpers
%%

% get_header_value(Name, Headers) ->
%     case mochiweb_headers:get_value(Name, Headers) of
%         undefined -> <<>>;
% 	   Value -> z_convert:to_binary(Value)
%     end.

%%
%% Formats
%%

format(alog, MD) ->
    StartTime = proplists:get_value(start_time, MD),
    RequestId = proplists:get_value(req_id, MD),
    Path = proplists:get_value(path, MD),
    Method = proplists:get_value(method, MD),
    Status = proplists:get_value(status, MD),
    Size = proplists:get_value(size, MD),
    User = z_convert:to_binary(proplists:get_value(user, MD, <<"-">>)),
    RemoteIP = proplists:get_value(remote_ip, MD),
    Referer = proplists:get_value(referer, MD),
    UserAgent = proplists:get_value(user_agent, MD),

    fmt_alog(
        fmt_time(StartTime),
        RequestId,
        RemoteIP,
        z_convert:to_binary(User),
        z_convert:to_binary(Method),
        Path,
        {1,1},
        z_convert:to_binary(Status),
        z_convert:to_binary(Size),
        Referer,
        UserAgent).


fmt_time({_MegaSecs, _Secs, _MicroSecs}=Ts) ->
    fmt_time(calendar:now_to_universal_time(Ts));
fmt_time({{Year, Month, Date}, {Hour, Min, Sec}}) ->
    io_lib:format("[~2..0w/~s/~4..0w:~2..0w:~2..0w:~2..0w ~s]",
        [Date, month(Month), Year, Hour, Min, Sec, "+0000"]).

fmt_alog(Time, ReqId, Ip, User, Method, Path, {VM,Vm}, Status,  Length, Referrer, UserAgent) ->
    [fmt_ip(Ip), " - ", sanitize(User), $\s, Time, $\s, $", sanitize(Method), " ", sanitize(Path),
     " HTTP/", z_convert:to_binary(VM), $., z_convert:to_binary(Vm), $",$\s,
     Status, $\s, Length, $\s,$", sanitize(Referrer),
     $",$\s,$", sanitize(UserAgent), $",$\s, z_convert:to_binary(ReqId)].

fmt_ip(IP) when is_tuple(IP) ->
    inet_parse:ntoa(IP);
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
