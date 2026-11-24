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
Traffic Guard detects and limits excessive requests from individual IP addresses.
It protects a site from rapid crawling and repeated page requests, including
clients that present themselves as ordinary browsers. It starts in **observe**
mode, recording decisions without rejecting requests.

## When to use it

Use Traffic Guard to limit excessive origin requests without browser challenges,
cookies or user-agent verification. It does not require `mod_analytics`.

Limits are shared by all covered requests from one IP on one site and node.
Offices, carrier NAT and proxies share an allowance, so inspect observe-mode
results before enforcing. Configure trusted proxies correctly: client addresses
come from Cowmachine's trusted-proxy processing. This is not a DDoS firewall;
distributed attacks still need proxy-level protection.

## Getting started

1. Enable `mod_traffic_guard` in the site's Modules screen. Leave the default
   mode at `observe` initially.
2. Open **System → Traffic guard** to inspect currently limited addresses and
   manage IP exceptions.
3. Use **Settings** on the Traffic guard screen to tune all five site settings, then
   set `mode` to `enforce` when the observed decisions are appropriate.

For example, the default settings allow an initial burst of 30 requests per IP
and replenish two request credits per second. Reloading the same page consumes
the same allowance as visiting different pages.

## Configuration and usage

| Key | Default | Meaning |
| --- | --- | --- |
| `mode` | `observe` | `off`, `observe` or `enforce`. |
| `period` | `60` | Rate period in seconds (1–3600). |
| `request_limit` | `120` | Request credits replenished per period (1–100000). |
| `request_burst` | `30` | Maximum request credits (1–1000). |
| `max_clients` | `5000` | Retained client budgets per site/node (1–50000). |

**observe** allows requests and reports `would_throttle` when the rate limit would
reject them. **enforce** rejects excess requests with HTTP 429, `Retry-After` and
no-cache headers. **off** bypasses checks and guard analytics. Rejected requests
do not consume more credits; clients recover automatically. Invalid settings use
the defaults. Rate-setting changes reset transient budgets.

### IP exceptions

Use **System → Traffic guard** to add or edit exact IPv4/IPv6 addresses, with a
required note and optional UTC expiry. Each site list supports up to 256 entries.
Separate addresses with spaces, commas or newlines. Invalid entries reject the
whole save. IPv4-mapped IPv6 addresses match IPv4 entries; CIDR ranges and hostnames
are not supported.

Allow entries take precedence over blocks and bypass the request-rate limit.
Blocks follow the configured mode and only affect guarded routes and methods.
Choose an hour, day, week, 30-day month, forever or an explicit UTC deadline.
Expired entries stop applying immediately. Rule edits preserve other clients'
budgets and throttling evidence.

The current-throttling panel shows up to 200 addresses on this node, including
observe-mode decisions. **Block IP** creates a timed block with a note derived
from the displayed evidence. Both panels require administrator access; writes
also require credentials that are not read-only. See `m_traffic_guard` for APIs.

Global `traffic_guard_allow` and `traffic_guard_block` lists in `zotonic.config`
are merged whenever site settings load. They are permanent, appear read-only in
the admin page, and cannot be removed by deleting site entries. Allow precedence
also applies across global and site rules. The README includes configuration examples.

## Request coverage and behavior

The guard covers GET/HEAD requests to page, template, HTTP model API and agent
interface controllers. Custom controllers classified as pages by analytics
(`{analytics, page}`) are also covered, without requiring analytics collection.
Use `{traffic_guard, true}` to opt another controller in, or `{traffic_guard, false}`
to exempt a route. POST requests and MQTT messages are not covered; static assets
are not covered by default.

There are no automatic administrator or self-declared crawler exemptions.
Cookies and user-agent changes cannot reset an IP's allowance. Limits are
node-local, not cluster-wide; clients rotating IP addresses can evade them.

If rate-limit admission is unavailable, overloaded or at client capacity, requests
fail open with an unknown assessment. Explicit IP-rule hits use a separate lookup
and bypass the admission worker. Unknown does not mean a suspected crawler.

## Analytics

With `mod_analytics` enabled, the default Page requests chart adds **Would throttle**
and **Throttled** lines for `other` and `unknown` page attempts, including early
rejections. A companion recording preserves existing page-view history and starts
collecting guard history when created. It reserves 10,734 group/time slots within
the site budget. Attempts overlap with page views; do not add their totals.

For additional breakdowns, record `mod_traffic_guard:request`. Dimensions include
`crawl_behavior` (normal, excessive, unknown), `guard_action` (allow,
would_throttle, throttle), `guard_reason` (none, request_rate, state_capacity,
unavailable, ip_allow, ip_block), `traffic`, and `type` (page, api, other).
Ordinary request events also receive the three guard dimensions when assessed.
Analytics exclusions and consent still apply; enforcement is independent of them.
Analytics events contain no client IPs or raw paths.

## Implementation and lifecycle

One admission worker per site serializes request-rate accounting. Its bounded,
transient client state contains IP addresses and budgets, not paths, query strings
or cookies. Idle clients expire after their budgets could recover; a restart clears
rate-limit state.

IP exceptions persist in the site's `traffic_guard_ip` table. Requests check a
site-isolated named ETS table directly, with allow precedence. A separate supervised
process removes expired entries every second; lookups also check deadlines, so
cleanup cannot prolong a rule's validity. The admission worker retains indexed
rules as a fallback while the shared table loads. Hourly maintenance deletes
expired database rows. The README contains failure limits, benchmarks and
regression-test commands.
").
-export([start_link/1, init/1, observe_middleware/3, observe_m_config_update/2,
    eligible/3, eligible/4, signals/1, response/2, observe_admin_menu/3, manage_schema/2, observe_tick_1h/2]).
-include_lib("zotonic_core/include/zotonic.hrl").

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
observe_middleware(_, Context, _) -> Context.

-spec eligible(Method, Controller, Option) -> boolean() when
    Method :: binary(), Controller :: atom(), Option :: term().
eligible(Method,Controller,Option) -> eligible(Method,Controller,Option,undefined).

%% Use core dispatch classification without requiring analytics. Eligibility is
%% decided before the response is rendered.
-spec eligible(Method, Controller, Option, Analytics) -> boolean() when
    Method :: binary(), Controller :: atom(), Option :: term(), Analytics :: term().
eligible(Method, _, _, _) when Method =/= <<"GET">>, Method =/= <<"HEAD">> -> false;
eligible(_, _, false, _) -> false;
eligible(_, _, true, _) -> true;
eligible(_, Controller, _, Analytics) ->
    lists:member(Controller, [controller_page, controller_template, controller_api,
        controller_agent_content, controller_agent_discovery]) orelse
        z_dispatcher:request_type(Analytics,Controller) =:= <<"page">>.

guard(Settings, Context) ->
    Client = m_req:get(peer_ip, Context),
    Result = case Client of
        undefined -> z_traffic_guard:decision(maps:get(mode, Settings), unavailable, 0);
        _ -> z_traffic_guard:check(Client, Settings, Context)
    end,
    Ctx1 = z_context:set(request_guard_result, Result#{source => ?MODULE}, Context),
    case maps:get(guard_action, Result) of
        <<"throttle">> ->
            response(maps:get(retry_after, Result), Ctx1);
        _ -> Ctx1
    end.

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
    case z_context:get(request_guard_result, Context) of
        #{crawl_behavior := B, guard_action := A, guard_reason := R} ->
            #{<<"crawl_behavior">> => B, <<"guard_action">> => A, <<"guard_reason">> => R};
        _ -> #{}
    end.
