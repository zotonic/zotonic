%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell <marc@worrell.nl>
%% @doc Display mailing history and the status of an individual mailing.
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

-module(controller_admin_mailing_run).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "backend_developer", "controller", "mailing_lists",
        "email_delivery", "monitor", "authorization_and_access_control"
    ]
}).
-moduledoc("
Display mailing history and the delivery status of an individual mailing run.

## Routes and templates

| Dispatch | Path | Template |
| --- | --- | --- |
| `admin_mailings` | `/admin/mailings` | `admin_mailings.tpl` |
| `admin_mailing_run` | `/admin/mailings/run/:run_id` | `admin_mailing_run.tpl` |

The presence of the `run_id` request argument selects the detail template.
The history page accepts `status`, `language`, `page_id`, `list_id`, and `offset`
filters. The detail page shows delivery progress and recipient results, with
`recipient_status` and `after` arguments for filtering and pagination.
The templates use `model#mailinglist_run` and live updates to display run data.

## Access and response handling

The controller requires the `use mod_mailinglist` permission. The model also
checks access to each run's page and mailing list before exposing its data.
An unavailable or inaccessible run displays an explanatory message in the detail
template. Responses carry cache prevention and no-index headers.

Saved mailing HTML is served separately by
`controller#controller_admin_mailing_run_content`.
").
-export([service_available/1, is_authorized/1, process/4]).
-include_lib("zotonic_core/include/zotonic.hrl").
service_available(Context) ->
    {true, z_context:set_nocache_headers(z_context:set_noindex_header(Context))}.
is_authorized(Context) ->
    z_controller_helper:is_authorized([{use, mod_mailinglist}], Context).
process(_, _, _, Context) ->
    Template =
        case z_context:get_q(<<"run_id">>, Context) of
            undefined -> "admin_mailings.tpl";
            _ -> "admin_mailing_run.tpl"
        end,
    z_context:output(z_template:render(Template, [], Context), Context).
