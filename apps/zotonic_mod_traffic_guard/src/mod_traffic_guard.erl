%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Behavioral crawl protection and optional aggregate analytics signals.
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

-module(mod_traffic_guard).
-behaviour(supervisor).
-mod_title("Traffic guard").
-mod_description("Observe and throttle excessive content requests, independently of user agents.").
-mod_prio(400).
-mod_schema(1).
-mod_depends([]).
-mod_config([
    #{key => mode, type => string, default => <<"observe">>,
      description => "off, observe or enforce. Observe reports excess without blocking."},
    #{key => period, type => integer, default => 60,
      description => "Rate period in seconds (1..3600)."},
    #{key => request_limit, type => integer, default => 120,
      description => "Sustained requests per period (1..100000)."},
    #{key => request_burst, type => integer, default => 30,
      description => "Immediate request allowance (1..1000)."},
    #{key => max_clients, type => integer, default => 5000,
      description => "Maximum retained client budgets per site and node (1..50000)."}
]).
-moduledoc(#{zotonic_keywords => ["reference", "module", "operator", "backend_developer", "security", "performance", "logging_and_monitoring"]}).
-moduledoc("
Observe or throttle rapid content retrieval without trusting browser user agents.
Enable `mod_traffic_guard` in Modules. The default mode is **observe**, which allows
all requests and reports when a configured budget would reject one. **enforce**
returns HTTP 429 with Retry-After and no-cache headers. **off** bypasses checks.

## Scope and configuration

The guard covers GET/HEAD requests to page, template, HTTP model API and agent
interface controllers. Static assets, POST requests and MQTT messages are excluded.
Custom controllers classified as pages by analytics (`{analytics, page}`) are also
covered, even without analytics enabled. Dispatch option `{traffic_guard, true}`
opts another controller in; `false` opts a
route out. Configuration is under `mod_traffic_guard` in System Configuration.

| Key | Default | Meaning |
| --- | --- | --- |
| mode | observe | off, observe, enforce |
| period | 60 | Rate period, in seconds |
| request_limit | 120 | Requests replenished per period |
| request_burst | 30 | Maximum request credits |
| max_clients | 5000 | Retained client budgets per site/node |

Budgets are shared by all covered routes for a client IP. Cookies and user agents
cannot reset them. There are no automatic admin or claimed-bot exemptions. Shared
networks share budgets: calibrate in observe mode before enforcing. Invalid values
fall back to defaults. Configuration changes reset transient budgets on the next
check. Off mode performs no detection or analytics emission.

## IP exceptions

Use **System → Traffic guard** to edit exact IPv4 and IPv6 allow/block lists (up to
256 addresses per list). Allow takes precedence and bypasses rate limits. Block
entries follow the mode: observe reports would_throttle, enforce returns 429, off
bypasses checks. Lists cover only guarded routes and methods. Saves require a
writable administrator and invalidate the in-memory copy. Invalid entries reject
the whole save. IPv4-mapped IPv6 addresses match IPv4 entries. CIDR networks and
hostnames are not supported. Entries are stored in `traffic_guard_ip`, with a required note and optional UTC
expiry. Expired rules stop applying immediately; hourly cleanup deletes them.
The admin page shows currently rate-limited IPs on this node and offers timed
blocks with automatically generated notes. Global `traffic_guard_allow` and
`traffic_guard_block` lists in zotonic.config are merged on settings load and
shown read-only. See `m_traffic_guard` for model APIs.

## Analytics

With `mod_analytics` enabled, the default Page requests chart includes Would
throttle and Throttled lines for `other` and `unknown` page attempts. A separate
companion recording reserves 10,734 rows within the site budget, preserving the
existing page-view history. Additional breakdowns can use `mod_traffic_guard:request`.
Each checked request emits one decision, including early rejections. Dimensions
are `crawl_behavior` (normal, excessive, unknown), `guard_action` (allow,
would_throttle, throttle), `guard_reason` (none, request_rate,
state_capacity, unavailable, ip_allow, ip_block), `traffic`, and `type` (page, api, other).
Existing request events also receive the three guard dimensions. Unknown means
no reliable guard assessment was available, not a suspected crawler.
Analytics consent and admin exclusions still apply; enforcement never depends on
analytics availability or recording configuration. Guard events and ordinary
request events overlap and should not be added together as a request total.

## Limits and privacy

State is transient and bounded: only client IPs and request-rate budgets are
tracked, with no paths, query strings or cookies. Every covered request consumes
the same budget, including repeated reloads of one page. Idle clients expire and
a restart resets all budgets. Limits are node-local, not shared across a cluster.

Unavailable or busy workers and exhausted client capacity fail open with an
unknown assessment. This is origin protection, not a DDoS firewall. Use trusted
proxy configuration for the client IP and proxy-level protection for distributed
attacks. This version does not implement fingerprinting, challenges, account
budgets, concurrency limits, or bot verification. See the module README for setup
and validation examples.
").
-export([start_link/1, init/1, observe_middleware/3, observe_analytics_events/3, observe_m_config_update/2,
    eligible/3, eligible/4, signals/1, response/2, observe_admin_menu/3, manage_schema/2, observe_tick_1h/2]).
-include_lib("zotonic_core/include/zotonic.hrl").
%% The analytics declaration is optional; no runtime dependency on mod_analytics.
-include_lib("zotonic_mod_analytics/include/analytics.hrl").

-include_lib("zotonic_mod_admin/include/admin_menu.hrl").

observe_admin_menu(#admin_menu{}, Acc, C) ->
    [#menu_item{id=admin_traffic_guard, parent=admin_system, sort=960,
        label=?__("Traffic guard",C), url={admin_traffic_guard},
        visiblecheck={acl,use,mod_admin_config}} | Acc].

manage_schema(_,C) -> m_traffic_guard:install(C).
observe_tick_1h(tick_1h,C) -> m_traffic_guard:cleanup(C).

start_link(Args) -> supervisor:start_link(?MODULE, Args).

init(Args) ->
    {ok, {#{strategy => rest_for_one, intensity => 3, period => 60}, [
        #{id => z_traffic_guard_rules, start => {z_traffic_guard_rules, start_link, [Args]},
          restart => permanent, shutdown => 5000, type => worker, modules => [z_traffic_guard_rules]},
        #{id => z_traffic_guard, start => {z_traffic_guard, start_link, [Args]},
          restart => permanent, shutdown => 5000, type => worker, modules => [z_traffic_guard]}
    ]}}.

observe_m_config_update(#m_config_update{module = mod_traffic_guard}, Context) ->
    z_traffic_guard:reset(Context);
observe_m_config_update(_, _) -> ok.

observe_middleware(#middleware{on = guard}, Context, _) ->
    case eligible(cowmachine_req:method(Context), z_context:get_controller_module(Context),
            z_context:get(traffic_guard, Context), z_context:get(analytics, Context)) of
        false -> Context;
        true ->
            Settings = z_traffic_guard:settings(Context),
            case maps:get(mode, Settings) of
                off -> Context;
                _ -> guard(Settings, Context)
            end
    end;
observe_middleware(#middleware{on = handled}, Context, _) ->
    emit(Context),
    Context;
observe_middleware(_, Context, _) -> Context.

-spec eligible(Method, Controller, Option) -> boolean() when
    Method :: binary(), Controller :: atom(), Option :: term().
eligible(Method,Controller,Option) -> eligible(Method,Controller,Option,undefined).

%% Use the same dispatch classification as analytics, without requiring a
%% recording or consent: eligibility is decided before the response is rendered.
-spec eligible(Method, Controller, Option, Analytics) -> boolean() when
    Method :: binary(), Controller :: atom(), Option :: term(), Analytics :: term().
eligible(Method, _, _, _) when Method =/= <<"GET">>, Method =/= <<"HEAD">> -> false;
eligible(_, _, false, _) -> false;
eligible(_, _, true, _) -> true;
eligible(_, Controller, _, Analytics) ->
    lists:member(Controller, [controller_page, controller_template, controller_api,
        controller_agent_content, controller_agent_discovery]) orelse
        z_analytics_request:dispatch_type(Analytics,Controller) =:= <<"page">>.

guard(Settings, Context) ->
    Client = m_req:get(peer_ip, Context),
    Result = case Client of
        undefined -> z_traffic_guard:decision(maps:get(mode, Settings), unavailable, 0);
        _ -> z_traffic_guard:check(Client, Settings, Context)
    end,
    Ctx1 = z_context:set(traffic_guard_result, Result, Context),
    case maps:get(guard_action, Result) of
        <<"throttle">> ->
            Ctx2 = rejection_context(Ctx1),
            emit(Ctx2),
            response(maps:get(retry_after, Result), Ctx2);
        _ -> Ctx1
    end.

%% Admission needs only trusted proxy metadata. Allowed requests initialize
%% authentication normally in Cowmachine; rejected ones only need it for telemetry.
rejection_context(Context) ->
    case analytics_interested(Context) of
        false -> Context;
        true ->
            try
                Ctx = case z_context:get_cookie(<<"cotonic-sid">>, Context) of
                    undefined -> Context;
                    Sid -> z_context:set_session_id(Sid, Context)
                end,
                z_context:ensure_request_context(Ctx)
            catch _:_ ->
                %% Do not record an unresolved identity as anonymous traffic.
                z_context:set(analytics_exclude, true, Context)
            end
    end.

analytics_interested(Context) ->
    try z_module_manager:active(mod_analytics, Context) andalso
        z_analytics_worker:interested(<<"mod_traffic_guard:request">>,Context)
    catch _:_ -> false end.

-spec response(Retry, Context) -> z:context() when
    Retry :: pos_integer(), Context :: z:context().
response(Retry, Context) ->
    Ctx = z_context:set_resp_headers([
        {<<"retry-after">>, integer_to_binary(max(1,Retry))},
        {<<"cache-control">>, <<"no-store">>},
        {<<"content-type">>, <<"text/plain; charset=utf-8">>}
    ], Context),
    z_context:set(request_guard_response, {429, <<"Too many requests. Please retry later.\n">>}, Ctx).

-spec signals(Context) -> map() when Context :: z:context().
signals(Context) ->
    case z_context:get(traffic_guard_result, Context) of
        #{crawl_behavior := B, guard_action := A, guard_reason := R} ->
            #{<<"crawl_behavior">> => B, <<"guard_action">> => A, <<"guard_reason">> => R};
        _ -> #{}
    end.

emit(Context) ->
    case z_context:get(traffic_guard_result, Context) of
        undefined -> ok;
        _ ->
            %% Optional best-effort telemetry must never alter admission decisions.
            try
                true = analytics_interested(Context),
                Type = case z_context:get_controller_module(Context) of
                    controller_page -> <<"page">>;
                    controller_template -> <<"page">>;
                    controller_api -> <<"api">>;
                    controller_agent_content -> <<"api">>;
                    controller_agent_discovery -> <<"api">>;
                    Controller ->
                        case z_analytics_request:dispatch_type(z_context:get(analytics,Context),Controller) of
                            <<"page">> -> <<"page">>;
                            _ -> <<"other">>
                        end
                end,
                m_analytics:record_event(<<"mod_traffic_guard:request">>, #{<<"type">> => Type},
                    z_context:get_reqdata(Context), Context)
            catch _:_ -> ok end
    end.

observe_analytics_events(#analytics_events{}, Acc, _) ->
    [#{name => <<"mod_traffic_guard:request">>, version => 1,
       title => <<"Traffic guard decisions">>,
       dimensions => #{
           <<"crawl_behavior">> => [<<"normal">>, <<"excessive">>, <<"unknown">>],
           <<"guard_action">> => [<<"allow">>, <<"would_throttle">>, <<"throttle">>],
           <<"guard_reason">> => [<<"none">>, <<"request_rate">>,
               <<"state_capacity">>, <<"unavailable">>, <<"ip_allow">>, <<"ip_block">>],
           <<"traffic">> => [<<"crawler">>, <<"agent">>, <<"other">>, <<"unknown">>],
           <<"type">> => [<<"page">>, <<"api">>, <<"other">>]
       }} | Acc].
