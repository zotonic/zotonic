%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell <marc@worrell.nl>
%% @doc Human-readable diagnostics for current and legacy mailing-run details.
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

-module(z_mailinglist_delivery_detail).
-moduledoc(<<"
Format delivery diagnostics, including legacy serialized notification tuples.
Only string tokens and restricted identifiers are decoded from stored text;
arbitrary Erlang terms are never evaluated and no atoms are created.
The result is plain text and must be escaped when displayed in HTML.
">>).
-export([format/1]).

-spec format(Detail) -> binary() | undefined when Detail :: term().
format(Detail) when is_binary(Detail) ->
    Pattern = <<"^\\s*\\{(?:bounce|retry|smtphost|error|illegal_address|sender_disabled),\\s*"
                "<<\\s*(\"(?:[^\"\\\\]|\\\\.)*\")\\s*>>\\s*\\}\\s*$">>,
    case re:run(Detail, Pattern, [dotall, {capture, [1], list}]) of
        {match, [Quoted]} ->
            %% Quoted is constrained to one string token, so scanning cannot
            %% intern atoms from an untrusted SMTP response.
            case erl_scan:string(Quoted) of
                {ok, [{string, _, Text}], _} ->
                    try list_to_binary(Text)
                    catch error:badarg -> Detail
                    end;
                _ -> Detail
            end;
        nomatch -> legacy_error(Detail)
    end;
format({Reason, undefined}) when is_atom(Reason) ->
    reason_message(atom_to_binary(Reason));
format({Reason, Status}) when is_atom(Reason) ->
    format(Status);
format(undefined) ->
    undefined;
format(Reason) when is_atom(Reason) ->
    error_message(atom_to_binary(Reason));
format(Other) ->
    iolist_to_binary(io_lib:format("~p", [Other])).


legacy_error(Detail) ->
    Pattern = <<"^\\s*\\{(bounce|retry|smtphost|error|illegal_address|sender_disabled),\\s*"
                "(?:undefined|\\{error,\\s*([a-z_]+)\\s*\\})\\s*\\}\\s*$">>,
    case re:run(Detail, Pattern, [{capture, [1, 2], binary}]) of
        {match, [Reason, <<>>]} -> reason_message(Reason);
        {match, [_Reason, Error]} -> error_message(Error);
        nomatch -> Detail
    end.

reason_message(<<"bounce">>) -> <<"Delivery was rejected.">>;
reason_message(<<"retry">>) -> <<"Delivery will be retried.">>;
reason_message(<<"illegal_address">>) -> <<"The email address is invalid.">>;
reason_message(<<"sender_disabled">>) -> <<"Sending is disabled for this sender.">>;
reason_message(_) -> <<"Delivery failed without further details.">>.

error_message(<<"timeout">>) -> <<"The connection to the mail server timed out.">>;
error_message(<<"closed">>) -> <<"The mail server closed the connection.">>;
error_message(<<"nxdomain">>) -> <<"The mail server domain could not be found.">>;
error_message(<<"econnrefused">>) -> <<"The mail server refused the connection.">>;
error_message(<<"auth_failed">>) -> <<"Authentication with the mail server failed.">>;
error_message(Error) ->
    <<"Delivery failed: ", (binary:replace(Error, <<"_">>, <<" ">>, [global]))/binary>>.
