%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021-2025 Marc Worrell
%% @doc Metrics for the http listener.

%% Copyright 2021-2025 Marc Worrell
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

-module(zotonic_listen_http_metrics).

-export([
    metrics_callback/1,
    req_filter/1,
    resp_headers_filter/1
]).

%% @doc Filter the headers and request to prevent excessive data copying.
-spec req_filter(cowboy_req:req()) -> map().
req_filter(#{ headers := Headers }=Req) ->
    Headers1 = maps:with([<<"referer">>, <<"user-agent">>], Headers),
    Req1 = maps:with([method, scheme, path, qs, version, peer], Req),
    Req1#{ headers => Headers1 }.


%% @doc We don't use any of the response headers for logging.
-spec resp_headers_filter(cowboy:http_headers()) -> cowboy:http_headers().
resp_headers_filter(_Headers) ->
    #{}.


%% @doc Cowboy metrics callback. For docs about the argument see:
%% https://ninenines.eu/docs/en/cowboy/2.9/manual/cowboy_metrics_h/
%%
%% The user_data metrics are added by z_sites_dispatcher, z_cowmachine_middleware
%% and via z_context:set_req_metrics/2
%%
%% The buffer is consumed by z_stats, which interfaces to the loggers.
%%
%% Basic stats is done on this routine, as the ringbuffer will drop events when
%% the loggers can't keep up with the http requests.
%%
-spec metrics_callback( map() ) -> ok.
metrics_callback(#{
        user_data := #{
            site := Site
        } = UserData,
        reason := Reason,
        req := Req = #{},
        req_start := ReqStart,
        req_end := ReqEnd,
        req_body_length := ReqBodyLength,
        req_body_end := ReqBodyEnd,
        resp_start := RespStart,
        resp_status := RespStatus,
        resp_body_length := RespBodyLength
    }) ->
    UnitsPerUsec = erlang:convert_time_unit(1, microsecond, native),
    ProcessStart = case ReqBodyEnd of
        undefined -> ReqStart;
        _ -> ReqBodyEnd
    end,
    ProcessEnd = case RespStart of
        undefined -> ReqEnd;
        _ -> RespStart
    end,
    DurationTotalUsec = (ReqEnd - ReqStart) div UnitsPerUsec,
    DurationProcessUsec = (ProcessEnd - ProcessStart) div UnitsPerUsec,
    StatusCategory = http_status_category(Reason, RespStatus),
    z_stats:record_event(http, request, Site),
    z_stats:record_event(http, StatusCategory, Site),
    z_stats:record_duration(http, duration, DurationProcessUsec div 1000, Site),
    case ReqBodyLength of
        undefined -> ok;
        _ -> z_stats:record_count(http, data_in, ReqBodyLength, Site)
    end,
    case RespBodyLength of
        undefined -> ok;
        _ -> z_stats:record_count(http, data_out, RespBodyLength, Site)
    end,
    PeerIP = maps:get(peer_ip, UserData, src(Req)),
    Log = maps:with([method, path, qs], Req),
    Log1 = Log#{
        site => Site,
        reason => Reason,
        req_start => ReqStart,
        duration_total_usec => DurationTotalUsec,
        duration_process_usec => DurationProcessUsec,
        resp_status => RespStatus,
        resp_status_category => StatusCategory,
        req_bytes => ReqBodyLength,
        resp_bytes => RespBodyLength,
        http_version => cowboy_req:version(Req),
        user_agent => cowboy_req:header(<<"user-agent">>, Req),
        referer => cowboy_req:header(<<"referer">>, Req),
        metrics => UserData#{ peer_ip => PeerIP }
    },
    ringbuffer:write(queue(StatusCategory), Log1);
metrics_callback(_Metrics) ->
    % Early failure.
    % TODO: Should also be logged.
    ok.

src(#{ peer := {IP, _Port} }) -> IP;
src(_) -> undefined.

queue('xxx') -> zotonic_http_metrics_normal;
queue('1xx') -> zotonic_http_metrics_normal;
queue('2xx') -> zotonic_http_metrics_normal;
queue('3xx') -> zotonic_http_metrics_normal;
queue('4xx') -> zotonic_http_metrics_prio;
queue('5xx') -> zotonic_http_metrics_prio.

http_status_category(switch_protocol, _) -> '1xx';
http_status_category(_, X) when X < 300 -> '2xx';
http_status_category(_, X) when X < 400 -> '3xx';
http_status_category(_, X) when X < 500 -> '4xx';
http_status_category(_, X) when X < 600 -> '5xx';
http_status_category(_, _X) -> 'xxx'.

