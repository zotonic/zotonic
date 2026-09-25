%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell <marc@worrell.nl>
%% @doc Serve saved mailing content with access control and a browser sandbox.
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

-module(controller_admin_mailing_run_content).
-moduledoc("Read-only saved mailing HTML, protected by run ACLs and a browser sandbox.").
-export([service_available/1, is_authorized/1, resource_exists/1, process/4]).

service_available(Context) ->
    Ctx = z_context:set_nocache_headers(z_context:set_noindex_header(Context)),
    {true,
        z_context:set_resp_header(
            <<"content-security-policy">>,
            <<"sandbox; default-src 'none'; img-src https: http: data:; style-src 'unsafe-inline' https: http:; font-src https: http: data:; base-uri 'none'; form-action 'none'; frame-ancestors 'self'">>,
            Ctx
        )}.

is_authorized(Context) ->
    z_controller_helper:is_authorized([{use, mod_mailinglist}], Context).

resource_exists(Context) ->
    case
        m_mailinglist_run:content(
            z_context:get_q(<<"run_id">>, Context),
            z_context:get_q(<<"language">>, Context),
            Context
        )
    of
        {ok, Copy} -> {true, z_context:set(mailing_copy, Copy, Context)};
        {error, _} -> {false, Context}
    end.

process(_, _, _, Context) ->
    Copy = z_context:get(mailing_copy, Context),
    z_context:output(maps:get(<<"html">>, Copy), Context).
