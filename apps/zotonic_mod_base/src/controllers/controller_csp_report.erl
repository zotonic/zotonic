%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Controller handling CSP reports. Every report received is checked for
%% the referrer and reported URL. If a report is accepted then it is forwarded
%% to the notifier system as a `content_security_report` notification, which modules
%% can subscribe to for further processing.
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

-module(controller_csp_report).
-moduledoc("
Controller handling Content-Security-Policy reports.

Set in the `report-to` header of your Content-Security-Policy the
URL to this controller to receive reports about policy violations.
").
-author("Marc Worrell <marc@worrell.nl>").

-export([
    allowed_methods/1,
    content_types_accepted/1,
    process/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-spec allowed_methods( z:context() ) -> {[ binary() ], z:context()}.
allowed_methods(Context) ->
    {[ <<"POST">> ], Context}.

-spec content_types_accepted( z:context() ) -> {list( cowmachine_req:media_type() ), z:context()}.
content_types_accepted(Context) ->
    {[
        {<<"application">>, <<"reports+json">>, []},
        {<<"application">>, <<"csp-report">>, []}
    ], Context}.

process(_Method, AcceptedCT, _ProvidedCT, Context) ->
    Origin = origin(Context),
    case is_site_url(Origin, Context) of
        true ->
            {Payload, Context1} = z_controller_helper:decode_request_noz(AcceptedCT, Context),
            case Payload of
                #{} ->
                    handle_report(Origin, Payload, Context1);
                Rs when is_list(Rs) ->
                    lists:foreach(fun(R) -> handle_report(Origin, R, Context1) end, Rs);
                _ ->
                    ok
            end,
            {<<>>, Context1};
        false ->
            ?LOG_INFO(#{
                in => zotonic_core,
                message => <<"Received CSP report from non-site origin URL, ignoring">>,
                result => error,
                reason => origin,
                origin => Origin
            }),
            {<<>>, Context}
    end.

handle_report(Origin, #{
        <<"type">> := <<"csp-violation">> = Type,
        <<"url">> := ReportUrl,
        <<"body">> := ReportBody
    }, Context) ->
    case is_site_url(ReportUrl, Context) of
        true ->
            UserAgent = maps:get(<<"user_agent">>, ReportBody,
                z_context:get_req_header(<<"user-agent">>, Context)),
            UserAgent1 = z_convert:to_binary(UserAgent),

            % Let modules handle the report.
            z_notifier:notify_sync(
                #content_security_report{
                    type = Type,
                    url = ReportUrl,
                    body = ReportBody,
                    user_agent = UserAgent1
                }, Context);
        false ->
            ?LOG_INFO(#{
                in => zotonic_core,
                message => <<"URL in CSP report does not match site">>,
                result => error,
                reason => referrer_mismatch,
                origin => Origin,
                report_url => ReportUrl
            })
    end;
handle_report(_Origin, _Report, _Context) ->
    % Ignore other reports
    ok.

is_site_url(<<"https://", _/binary>> = Url, Context) -> z_context:is_site_url(Url, Context);
is_site_url(_Url, _Context) -> false.

origin(Context) ->
    case z_context:get_req_header(<<"origin">>, Context) of
        <<"null">> -> z_context:get_req_header(<<"referer">>, Context);  % Safari
        Origin -> Origin
    end.
