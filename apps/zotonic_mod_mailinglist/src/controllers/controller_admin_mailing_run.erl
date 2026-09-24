%% Copyright 2026 The Zotonic Contributors
%% SPDX-License-Identifier: Apache-2.0
-module(controller_admin_mailing_run).
-moduledoc("Mailing run history and delivery detail, authorized through m_mailinglist_run.").
-export([service_available/1,is_authorized/1,process/4]).
-include_lib("zotonic_core/include/zotonic.hrl").
service_available(Context) ->
    {true,z_context:set_nocache_headers(z_context:set_noindex_header(Context))}.
is_authorized(Context) ->
    z_controller_helper:is_authorized([{use,mod_mailinglist}],Context).
process(_,_,_,Context) ->
    Template = case z_context:get_q(<<"run_id">>,Context) of
        undefined -> "admin_mailings.tpl";
        _ -> "admin_mailing_run.tpl"
    end,
    z_context:output(z_template:render(Template,[],Context),Context).
