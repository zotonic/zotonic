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
-moduledoc("Mailing run history and delivery detail, authorized through m_mailinglist_run.").
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
