%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Authorized Site Analytics dashboard and recording configuration pages.
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

-module(controller_admin_analytics).
-moduledoc(#{
    zotonic_keywords => ["reference", "controller", "backend_developer", "logging_and_monitoring", "authorization_and_access_control"]
}).
-moduledoc(<<"
Serve the Site Analytics dashboard and its separate recording configuration page.
Responses disable caching and search-engine indexing.

The `admin_analytics` dispatch renders `admin_analytics.tpl`, requires `use mod_analytics`,
and shows all non-paused recordings in saved order. The query argument `period` selects the reporting
window; its default `day` means six hours. Other supported values are `hour`, `24hours`,
`week`, `month`, `year` and `years`; unknown values also use six hours.

The dispatch option `{is_config, true}` instead requires `use mod_admin_config` and renders
`admin_analytics_config.tpl`. Configuration is independent of permission to view reports.
The selected model result is passed to the template as `analytics`.

```erlang
{admin_analytics_config, [\"admin\", \"analytics\", \"config\"],
    controller_admin_analytics, [{is_config, true}]}
```

Recording mutations are handled by `mod_analytics` events and the `m_analytics` APIs,
which check permissions separately. This controller only renders the pages.
"/utf8>>).

-export([service_available/1,is_authorized/1,process/4]).

service_available(Context) ->
    {true,z_context:set_noindex_header(z_context:set_nocache_headers(Context))}.

is_authorized(Context) ->
    case z_context:get(is_config, Context) of
        true -> z_controller_helper:is_authorized([{use,mod_admin_config}],Context);
        _ -> z_controller_helper:is_authorized([{use,mod_analytics}],Context)
    end.

process(_,_,_,Context) ->
    Period = case z_context:get_q(<<"period">>,Context) of
        undefined -> <<"day">>;
        P -> P
    end,
    {Template,D} = case z_context:get(is_config,Context) of
        true -> {"admin_analytics_config.tpl",m_analytics:configuration(Context)};
        _ -> {"admin_analytics.tpl",m_analytics:overview(Period,Context)}
    end,
    z_context:output(z_template:render(Template,[{analytics,D}],Context),Context).
