%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013 Marc Worrell
%% @doc Check if an e-mail is an auto-reply, bulk message or bounce.

%% Copyright 2013 Marc Worrell
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

-module(z_email_receive_check).
-author("Marc Worrell <marc@worrell.nl>").

-export([
         is_bulk/1,
         is_auto/1,
         is_bounce/2,
         is_nonfatal_bounce/3
        ]).

-include_lib("zotonic.hrl").


%% @doc Check if an e-mail message is a bulk message
-spec is_bulk(#email_received{}) -> boolean().
is_bulk(#email_received{headers=Headers}) ->
           is_bulk_precedence(lists:keyfind(<<"x-precedence">>, 1, Headers))
    orelse is_bulk_precedence(lists:keyfind(<<"precedence">>, 1, Headers)).

is_bulk_precedence(false) -> 
    false;
is_bulk_precedence({_, S}) -> 
    case z_convert:to_binary(z_string:to_lower(S)) of
        <<"bulk">> -> true;
        <<"junk">> -> true;
        <<"list">> -> true;
        _ -> false
    end.

%% @doc Check if an e-mail message is an automatic reply or another kind of message.
%%      This uses heuristics on the precedence and the subject of the e-mail.
-spec is_auto(#email_received{}) -> boolean().
is_auto(#email_received{headers=Headers, email=Email}) ->
           lists:keymember(<<"x-auto-response-suppress">>, 1, Headers)
    orelse lists:keymember(<<"x-autorespond">>, 1, Headers)
    orelse is_auto_precedence(lists:keyfind(<<"x-precedence">>, 1, Headers))
    orelse is_auto_precedence(lists:keyfind(<<"precedence">>, 1, Headers))
    orelse is_auto_submitted(lists:keyfind(<<"auto-submitted">>, 1, Headers))
    orelse is_auto_subject(Email#email.subject).

is_auto_precedence(false) -> 
    false;
is_auto_precedence({_, S}) -> 
    z_convert:to_binary(z_string:to_lower(S)) =:= <<"auto_reply">>.

is_auto_submitted(false) -> 
    false;
is_auto_submitted({_, S}) -> 
    z_convert:to_binary(z_string:to_lower(S)) =/= <<"no">>.

is_auto_subject(undefined) -> false;
is_auto_subject(Subject) -> is_auto_subject_1(z_convert:to_binary(z_string:to_lower(Subject))).

is_auto_subject_1(<<"auto:">>) -> true;
is_auto_subject_1(<<"automatic reply">>) -> true;
is_auto_subject_1(<<"autosvar">>) -> true;
is_auto_subject_1(<<"automatisk svar">>) -> true;
is_auto_subject_1(<<"automatisch antwoord">>) -> true;
is_auto_subject_1(<<"abwesenheitsnotiz">>) -> true;
is_auto_subject_1(<<"risposta non al computer">>) -> true;
is_auto_subject_1(<<"auto response">>) -> true;
is_auto_subject_1(<<"eespuesta automática">>) -> true;
is_auto_subject_1(<<"fuori sede">>) -> true;
is_auto_subject_1(<<"out of Office">>) -> true;
is_auto_subject_1(<<"frånvaro">>) -> true;
is_auto_subject_1(<<"réponse automatique">>) -> true;
is_auto_subject_1(_) -> false.


%% @doc Check if this might be a bounce e-mail by inspecting the headers of a received e-mail.
%% @todo Distinguish non-fatal bounces (like delivery delays, warnings etc)
-spec is_bounce({binary(),binary()}, [{binary(),binary()}]) -> boolean().
is_bounce(Type, Headers) ->
    case proplists:get_value(<<"Return-Path">>, Headers) of
        <<"<>">> -> 
            % Could still be an auto-away message, do some further checks
            %
            % See: http://tools.ietf.org/html/rfc6522
            % From = MAILER-DAEMON
            % Has header: "Diagnostic-Code"
            % Has header: "X-Failed-Recipients"
            % Content-Type: multipart/report
            % Subject: Delivery Status Notification (Failure)
                   Type =:= {<<"multipart">>, <<"report">>}
            orelse lists:keymember(<<"Diagnostic-Code">>, 1, Headers)
            orelse lists:keymember(<<"X-Failed-Recipients">>, 1, Headers)
            orelse is_from_daemon(Headers)
            orelse is_delivery_status(Headers);
        _ -> 
            false
    end.

is_from_daemon(Headers) ->
    contains(lists:keyfind(<<"From">>, 1, Headers), <<"mailer-daemon">>).

% is_multipart_report(Headers) ->
%     case lists:keyfind(<<"Content-Type">>, 1, Headers) of
%         false -> false;
%         {_, <<"multipart/report", _/binary>>} -> true
%     end.

is_delivery_status(Headers) ->
    case lists:keyfind(<<"Subject">>, 1, Headers) of
        false -> false;
        {_, Subject} ->
            Subject1 = z_convert:to_binary(z_string:to_lower(Subject)),
            case Subject1 of
                <<"delivery notification", _/binary>> -> true;
                <<"delivery status notification", _/binary>> -> true;
                <<"mail delivery failed", _/binary>> -> true;
                <<"undelivered mail", _/binary>> -> true;
                <<"undeliverable:", _/binary>> -> true;
                <<"returned mail", _/binary>> -> true;
                <<"mail system error", _/binary>> -> true;
                _ -> false
            end
    end.


contains(false, _P) ->
    false;
contains({_, V}, P) ->
    V1 = z_convert:to_binary(z_string:to_lower(V)),
    case binary:split(V1, P) of
        [_] -> false;
        _ -> true
    end.


% Non fatal bounces:
% - Subject contains: "(Delay)", "delayed", "warning"
% - Message part message/delivery-status with non fatal Status
is_nonfatal_bounce(Type, Headers, Parts) ->
    is_nonfatal_subject(Headers)
    orelse is_nonfatal_status(Type, Parts).

is_nonfatal_subject(Headers) ->
    case lists:keyfind(<<"Subject">>, 1, Headers) of
        false -> 
            false;
        {_, Subject} ->
            Subject1 = z_convert:to_binary(z_string:to_lower(Subject)),
                   contains_1(Subject1, <<"delay">>)
            orelse contains_1(Subject1, <<"warning">>)
    end.

contains_1(V, S) ->
    case binary:split(V, S) of
        [_] -> false;
        _ -> true
    end. 

%% @doc Find the "message/delivery-status" part
is_nonfatal_status({<<"multipart">>,<<"report">>}, Parts) ->
    Status = [
        Body || {<<"message">>, <<"delivery-status">>, _Hs, _Disp, Body} <- Parts
    ],
    case Status of
        [Body|_] ->
            Body1 = binary:replace(Body, <<"\r\n\r\n">>, <<"\r\n">>, [global]), 
            Hs = mochiweb_headers:to_list(mochiweb_headers:from_binary(<<Body1/binary, "\r\n\r\n">>)),
                   is_action_delayed(Hs)
            orelse is_nonfatal_diagnostic(Hs)
            orelse is_nonfatal_code(Hs);
        [] -> 
            false
    end;
is_nonfatal_status(_Type, _Parts) ->
    false.


is_action_delayed(Hs) ->
    case z_convert:to_binary(proplists:get_value("Action", Hs)) of
        <<"Delay", _/binary>> -> true;
        <<"delay", _/binary>> -> true;
        <<"DELAY", _/binary>> -> true;
        _ -> false
    end.

get_diagnostic(Hs) ->
    case z_convert:to_binary(proplists:get_value("Diagnostic-Code", Hs)) of
        <<"smtp;  ", C/binary>> -> C;
        <<"smtp; ", C/binary>> -> C;
        <<"smtp;", C/binary>> -> C;
        <<"SMTP;  ", C/binary>> -> C;
        <<"SMTP; ", C/binary>> -> C;
        <<"SMTP;", C/binary>> -> C;
        C -> C
    end.

is_nonfatal_diagnostic(Hs) ->
    case get_diagnostic(Hs) of
        undefined -> true;
        <<"2", _/binary>> -> true;
        <<"4", _/binary>> -> true;
        _ -> false
    end.

% http://www.iana.org/assignments/smtp-enhanced-status-codes/smtp-enhanced-status-codes.xml
is_nonfatal_code(Hs) ->
    case z_convert:to_binary(proplists:get_value(<<"Status">>, Hs)) of
        <<"2.", _/binary>> -> true;
        <<"4.", _/binary>> -> true;
        _ -> false
    end.

% Fatal bounces:
% Subject contains: "failure", "failed", "undelivered", "unavailable"
% Header "Auto-Submitted: auto-generated (failure)"

% In case of a multipart/report there must be a part like:
%
% --Boundary_(ID_+BSref4CJtAWKJ+rtAI7Vw)
% Content-type: message/delivery-status
%
% Original-envelope-id: 0MFH0016N46XRU00@st11p02mm-asmtp004.mac.com
% Reporting-MTA: dns;st11p02mm-asmtp004.mac.com (tcp-daemon)
% Arrival-date: Sun, 23 Dec 2012 07:44:58 +0000 (GMT)
%
% Original-recipient: rfc822;reply-19ug17mi80@www.example.org
% Final-recipient: rfc822;reply-19ug17mi80@www.example.org
% Action: delayed
% Status: 4.4.7 (unable to deliver this message after 2 days)

