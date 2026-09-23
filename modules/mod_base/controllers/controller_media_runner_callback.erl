%% @copyright 2026 Marc Worrell
%% @doc Accept authenticated results for pending media runner client jobs.

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

-module(controller_media_runner_callback).

-export([init/1, service_available/2, allowed_methods/2, is_authorized/2,
    content_types_accepted/2, content_types_provided/2, to_json/2, process_post/2]).
-include_lib("controller_webmachine_helper.hrl").

%% This endpoint needs no site context, session or general POST argument parsing.
init(_) -> {ok, undefined}.

service_available(RD, State) ->
    RD1 = wrq:set_resp_header("Cache-Control", "no-store", RD),
    {true, RD1, State}.

allowed_methods(RD, State) -> {['POST'], RD, State}.
content_types_accepted(RD, State) -> {[{"application/json", process_post}], RD, State}.
content_types_provided(RD, State) -> {[{"application/json", to_json}], RD, State}.
to_json(RD, State) -> {<<>>, RD, State}.

is_authorized(RD, _) ->
    %% ensure_qs/1 would read the body before we have authenticated the sender.
    Id = proplists:get_value("id", wrq:req_qs(RD)),
    case credentials(Id, wrq:get_req_header("authorization", RD)) of
        {ok, JobId, Secret} ->
            try z_media_runner:authorized(JobId, Secret) of
                true -> {true, RD, {JobId, Secret}};
                false -> respond(410, RD)
            catch exit:_ -> respond(503, RD)
            end;
        error -> respond(401, RD)
    end.

credentials(Id, "Bearer " ++ Secret) when is_list(Id), is_list(Secret),
        length(Id) >= 16, length(Id) =< 64, length(Secret) > 0, length(Secret) =< 128 ->
    case {re:run(Id, "^[a-zA-Z0-9_=-]+\\z", [{capture, none}]),
            re:run(Secret, "^[\\x21-\\x7e]+\\z", [{capture, none}])} of
        {match, match} -> {ok, list_to_binary(Id), list_to_binary(Secret)};
        _ -> error
    end;
credentials(_, _) -> error.

process_post(RD, {Id, Secret}) ->
    %% POST does not use content_types_accepted in every Webmachine path.
    ContentType = wrq:get_req_header_lc("content-type", RD),
    case ContentType =/= undefined andalso
            string:trim(hd(string:split(ContentType, ";"))) =:= "application/json" of
        true -> receive_result(Id, Secret, RD);
        false -> respond(415, RD)
    end;
process_post(RD, _) -> respond(401, RD).

receive_result(Id, Secret, RD) ->
    %% Use the underlying API to distinguish an oversized body from malformed JSON.
    try
        Limit = z_media_runner_protocol:callback_limit(),
        receive_body(Id, Secret, Limit, RD)
    catch
        error:media_runner_configuration -> respond(503, RD);
        _:_ -> respond(400, RD)
    end.

receive_body(Id, Secret, Limit, RD) ->
    case webmachine_request:req_body(Limit, RD) of
        {{error, req_body_too_large}, RD1} -> respond(413, RD1);
        {Body, RD1} when is_binary(Body), byte_size(Body) =< Limit ->
            decode_result(Id, Secret, Body, RD1);
        {Body, RD1} when is_binary(Body) -> respond(413, RD1);
        {_, RD1} -> respond(400, RD1)
    end.

decode_result(Id, Secret, Body, RD) ->
    try jsx:decode(Body, [return_maps]) of
        Result when is_map(Result) ->
            try z_media_runner:callback(Id, Secret, Result) of
                ok -> respond(204, RD);
                {error, gone} -> respond(410, RD)
            catch exit:_ -> respond(503, RD)
            end;
        _ -> respond(400, RD)
    catch _:_ -> respond(400, RD)
    end.

%% Credentials and callback bodies are deliberately never logged.
respond(Status, RD) -> {{halt, Status}, RD, undefined}.
