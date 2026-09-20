%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Accept authenticated result callbacks for pending remote media jobs.
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

-module(controller_media_runner_callback).

-moduledoc("
Client-side receiver for results produced by the external
[media runner](https://github.com/zotonic/mediarunner) site.
This controller belongs to `mod_base`, so each client site has a callback endpoint
without installing the runner site itself.

## Place in the processing flow

1. A client calls `z_exec:run/4` with its site context. When remote processing is
   configured, `z_media_runner` registers the waiting process, creates a job ID and
   random callback secret, and builds an absolute URL using the client's
   `media_runner_callback` dispatch rule.
2. The client submits the job to the runner using OAuth2. The job includes that
   callback URL and secret. The runner queues the command, executes it in the
   sandbox, and POSTs its JSON result here with `?id=<job-id>` and the secret in
   the `Authorization: Bearer <secret>` header.
3. This controller authenticates the callback and forwards the decoded result to
   `z_media_runner`. The waiting client process then uses
   `z_media_runner_protocol:unpack/2` to validate the result and restore output
   files to the paths declared by the original caller. This controller does not
   execute commands or write the returned media files.

## Callback authentication

The dispatch rule is anonymous deliberately: this endpoint authenticates with the
per-job callback secret, not the OAuth2 token used to submit jobs to the runner,
nor a browser session. `z_media_runner` keeps only the secret's hash and associates
it with the waiting process. Both the job ID and secret must match.

The request query is already parsed before `is_authorized/1`. Reading the job ID
there lets us reject unauthorized requests before allocating the bounded result
body. Responses have cache prevention headers; media payloads and callback secrets
are not logged.

## Delivery and lifetime

HTTP 204 acknowledges delivery to the waiting process, including a result that
reports a processing failure. It does not mean that output validation succeeded.
Malformed JSON or a non-map result returns 400; missing or malformed credentials
return 401. Unknown jobs and non-matching secrets return 410. The runner treats
410 as final and stops retrying that callback.

While the job is registered, duplicate authenticated callbacks are acknowledged
without notifying the waiting process again. The registration is removed when the
call finishes, times out, or its process dies. Late callbacks then return 410.
Registrations are in memory on the submitting Zotonic node; cluster routing must
send callbacks back to that node, and a node restart loses pending registrations.

See the [media runner documentation](https://github.com/zotonic/mediarunner#readme)
for deployment and client configuration.
").

-export([
    allowed_methods/1,
    content_types_accepted/1,
    content_types_provided/1,
    is_authorized/1,
    process/4
]).

-include_lib("kernel/include/logger.hrl").

allowed_methods(Context) ->
    {[<<"POST">>], Context}.

content_types_accepted(Context) ->
    {[{<<"application">>, <<"json">>, []}], Context}.

content_types_provided(Context) ->
    {[{<<"application">>, <<"json">>, []}], Context}.

%% @doc Authenticate the job ID and callback secret before reading the result body.
is_authorized(Context) ->
    %% The query is already parsed; no additional request parsing is needed here.
    Context1 = z_context:set_nocache_headers(Context),
    Id = z_context:get_q(<<"id">>, Context1),
    case cowmachine_req:get_req_header(<<"authorization">>, Context1) of
        <<"Bearer ", Secret/binary>> when is_binary(Id), byte_size(Secret) =< 128 ->
            case z_media_runner:authorized(Id, Secret) of
                true -> {true, z_context:set(media_runner_secret, Secret, Context1)};
                false -> authentication_failure(unknown_job_or_invalid_secret, 410, Context1)
            end;
        _ ->
            authentication_failure(missing_or_malformed_credentials, 401, Context1)
    end.

%% @doc Decode a bounded result envelope and hand it to the waiting client's registry.
%% The registry rechecks credentials in case the job ended after authorization.
process(_, _, _, Context) ->
    {Body, Context1} = cowmachine_req:req_body(z_media_runner_protocol:callback_limit(), Context),
    try
        Result = z_json:decode(Body),
        true = is_map(Result),
        Id = z_context:get_q(<<"id">>, Context1),
        case z_media_runner:callback(Id, z_context:get(media_runner_secret, Context1), Result) of
            ok -> {{halt, 204}, Context1};
            {error, gone} -> authentication_failure(job_no_longer_available, 410, Context1)
        end
    catch
        _:_ -> {{halt, 400}, Context1}
    end.

%% Log only bounded job identifiers, never authorization headers, secrets or bodies.
authentication_failure(Reason, Status, Context) ->
    ?LOG_NOTICE(#{
        text => <<"Media runner callback authentication failed">>,
        in => zotonic_mod_base,
        result => error,
        reason => Reason,
        http_status => Status,
        site => z_context:site(Context),
        job_id => log_job_id(z_context:get_q(<<"id">>, Context))
    }),
    {{halt, Status}, Context}.

log_job_id(Id) when is_binary(Id), byte_size(Id) >= 16, byte_size(Id) =< 64 ->
    case re:run(Id, <<"^[a-zA-Z0-9_-]+$">>, [{capture, none}]) of
        match -> Id;
        nomatch -> undefined
    end;
log_job_id(_) ->
    undefined.
