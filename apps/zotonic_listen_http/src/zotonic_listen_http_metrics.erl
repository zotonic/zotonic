%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc Metrics for the http listener.

%% Copyright 2021 Marc Worrell
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
    metrics_callback/1
]).

%% @doc Cowboy metrics callback. For docs about the argument see:
%% https://ninenines.eu/docs/en/cowboy/2.8/manual/cowboy_metrics_h/
%%
%% The user_data metrics are added by z_sites_dispatcher, z_cowmachine_middleware
%% and via z_context:set_req_metrics/2
-spec metrics_callback( map() ) -> ok.
metrics_callback(#{
        user_data := #{
            site := Site
        } = UserData,
        req := Req,
        req_start := ReqStart,
        req_end := ReqEnd,
        req_body_length := ReqBodyLength,
        req_body_end := ReqBodyEnd,
        resp_start := RespStart,
        resp_status := RespStatus,
        resp_body_length := RespBodyLength
    } = _Metrics) when is_map(Req) ->
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
    exometer:update([zotonic, Site, http, duration], DurationProcessUsec div 1000),
    case ReqBodyLength of
        undefined -> ok;
        _ -> exometer:update([zotonic, Site, http, data_in], ReqBodyLength)
    end,
    case RespBodyLength of
        undefined -> ok;
        _ -> exometer:update([zotonic, Site, http, data_out], RespBodyLength)
    end,
    PeerIP = case maps:get(peer_ip, UserData, undefined) of
        undefined -> cowmachine_req:peer_ip(Req);
        Peer -> Peer
    end,
    Log = #{
        site => Site,
        duration_total_usec => DurationTotalUsec,
        duration_process_usec => DurationProcessUsec,
        resp_status => RespStatus,
        req_bytes => ReqBodyLength,
        resp_bytes => RespBodyLength,
        method => cowboy_req:method(Req),
        path => cowboy_req:path(Req),
        user_agent => cowboy_req:header(<<"user-agent">>, Req),
        referer => cowboy_req:header(<<"referer">>, Req),
        metrics => UserData#{ peer_ip => PeerIP }
    },
    ringbuffer:write(zotonic_http_metrics, Log);
metrics_callback(_Metrics) ->
    % Early failure.
    % TODO: Should also be logged.
    ok.
