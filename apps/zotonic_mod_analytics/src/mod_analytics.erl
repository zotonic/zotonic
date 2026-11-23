%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Site Analytics for requests, content, email and module-defined events.
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

-module(mod_analytics).
-behaviour(supervisor).
-mod_title("Site Analytics").
-mod_description("Lightweight analytics for site traffic, content, email, and custom events.").
-mod_depends([admin]).
-mod_schema(5).
-mod_prio(500).
-mod_config([
    #{key=>row_budget,type=>integer,default=>1000000,
      description=>"Maximum reserved group/time slots across all retained recordings."},
    #{key=>require_consent,type=>boolean,default=>false,
      description=>"Only collect request events when analytics_consent is true in the request context."}
]).
-moduledoc(#{
    zotonic_keywords => ["reference", "module", "operator", "backend_developer", "logging_and_monitoring", "privacy_and_consent", "email_delivery"]
}).
-moduledoc(<<"
Site Analytics measures site traffic, popular content, outgoing email activity and
module-defined events without an external tracker. It helps operators understand
site usage and operational trends without storing visitor identifiers, tracking
cookies or raw request logs.

## When to use it

Use this module for aggregate counts and trends, not unique-visitor measurement,
billing or an audit trail. It counts requests that reach Zotonic; requests served
entirely by a CDN are not counted. Collection is best-effort, and reports identify
incomplete data. Run one collecting node per site; distributed collection is not
supported.

## Getting started

1. Enable `mod_analytics` in the site's Modules screen.
2. Open **Content → Site Analytics** to view the standard recordings.
3. Open **System → Analytics configuration** to add recordings or pause/resume
   existing ones. The default Languages recording starts paused.

For example, add a recording for **HTTP API requests** with a `status` breakdown
to compare successful responses and errors. New recordings collect future events;
there is no historical backfill.

## Configuration and usage

A recording selects one event, fixed filters and at most one breakdown dimension.
Use totals for overall activity, breakdowns to compare groups, and filters to
select the population of interest. Popular content shows approximate rankings
with count ranges, rather than unique visitors or exact ranks.

Definitions are immutable: create a new recording to change what is measured.
Pausing preserves history and its storage reservation. Deleting a recording
removes its history and releases that reservation. Drag recordings in configuration
to change their dashboard order without changing their data.

Per-site settings under `mod_analytics`:

| Key | Default | Meaning |
| --- | --- | --- |
| `row_budget` | `1000000` | Maximum reserved group/time slots across retained recordings; not physical SQL rows or bytes. |
| `require_consent` | `false` | Require `analytics_consent = true` in the context for request-associated events. |

Reports require `use mod_analytics`. Configuration and recording mutations require
site administrator access (`use mod_admin_config`). These permissions are independent.

## What is counted

Page events count successful public HTML GET responses, including eligible template
pages without a resource ID. A user who can edit the resolved resource is excluded
from that resource's views, including frontend previews and direct visits. Admin
access alone does not exclude a resource view. Without a resource ID, users with
`use mod_admin` are excluded. Read-only sessions cannot edit resources and can
therefore count as resource views. This applies to both `page_request` and the
page type of `request` events.

API and other classified requests can count errors and expose their final HTTP
status. They continue to exclude users with `use mod_admin`. All request types
respect `analytics_exclude` and the configured consent requirement. Sites supply
the consent flag through their own integration. DNT and GPC headers do not exclude
requests from these aggregate counts.

The `traffic` dimension distinguishes declared user-triggered agents, crawlers,
other clients and unknown clients. These are classifications, not verified
identities; `other` does not necessarily mean human. Page totals, device breakdowns
and popular content exclude detected agents and crawlers unless a recording
explicitly selects them. Traffic-classification and API measurements include them
unless filtered out.

SMTP observers count sent, retry, failed and bounced notifications independently
of requests. These are transport events, not unique emails, inbox deliveries or
opens. Traffic Guard can add would-throttle and throttled attempts to the default
Page requests chart. Guard attempts and page views overlap; do not sum them.

## Integration

`controller_page` and `controller_template` default to the `page` dispatch analytics
type; `controller_api` defaults to `api`. Override with `{analytics, Type}`, or
exclude a route with `{analytics, false}`. Other controllers are opt-in. Dedicated
agent interfaces expose their own dispatch types. MQTT model calls are not counted
by HTTP request analytics.

Modules declare namespaced events with the `analytics_events` fold notification.
Call `m_analytics:record_event/3` for background events, or `/4` to associate an
event explicitly with the current request. Declare coarse dimensions and avoid
personal data or free-form input. See `m_analytics` and the application README for
calling conventions, schemas and examples.

## Implementation and operational limits

A bounded ingress queue feeds an aggregation worker and a separate database writer.
Counters are stored asynchronously in capped minute, quarter-hour, hour and day
slots in the site's PostgreSQL database. Popular content uses bounded approximate
summaries over moving periods. No database call is needed on the normal ingestion
path.

Overload, outages or restarts can lose buffered events; committed history survives
restarts. Reports show gaps or partial counts instead of inventing zero traffic.
Pausing retains storage reservations, and a row budget bounds aggregate capacity,
not PostgreSQL disk usage. Database maintenance remains necessary. The README
contains retention limits, implementation details and validation commands.
"/utf8>>).

-export([
    start_link/1,init/1,manage_schema/2,

    observe_middleware/3,
    observe_admin_menu/3,
    observe_email_sent/2,
    observe_email_failed/2,
    observe_email_bounced/2,
    observe_module_activate/2,
    observe_module_deactivate/2,

    access_log/1,
    event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").

start_link(Args) ->
    supervisor:start_link(?MODULE,Args).

init(Args) ->
    Context = proplists:get_value(context,Args), Site=z_context:site(Context),
    Children = [
        #{
            id=>M,
            start=>{M,start_link,[Site]},restart=>permanent,shutdown=>5000,type=>worker,modules=>[M]
        }
        || M <- [z_analytics_writer,z_analytics_worker]
    ],
    {ok,{#{strategy=>one_for_all,intensity=>3,period=>60},Children}}.

manage_schema({upgrade,5},Context) ->
    ok = m_analytics:store_install(Context),
    ensure_page_guard(Context),
    z_analytics_worker:reload(Context);
manage_schema({upgrade,4},Context) ->
    ok = m_analytics:store_install(Context),
    ok = m_analytics:store_upgrade_traffic_groups(Context),
    z_analytics_worker:reload(Context);
manage_schema({upgrade,3},Context) ->
    ok = m_analytics:store_install(Context),
    add_smtp_recording(Context);
manage_schema(_,Context) ->
    ok = m_analytics:store_install(Context),
    case m_analytics:store_collectors(Context) of
        [] ->
            Registry = z_analytics:registry(Context),
            lists:foreach(
                fun({Name,Dimension,G,State}) ->
                    {ok,D} = z_analytics:definition(#{
                                <<"name">>=>Name,
                                <<"event">>=><<"page_request">>,
                                <<"dimension">>=>Dimension,
                                <<"groups">>=>G
                            },Registry),
                    case m_analytics:store_create(D,State,Context) of
                        {ok,_} -> ok;
                        {error,budget_exceeded} -> ok
                    end
                end,[
                    {<<"Page requests">>,<<>>,1,<<"active">>},
                    {<<"Languages">>,<<"language">>,16,<<"paused">>},
                    {<<"Popular content">>,<<"page">>,100,<<"active">>},
                    {<<"Devices">>,<<"device">>,5,<<"active">>},
                    {<<"Traffic classification">>,<<"traffic">>,5,<<"active">>}
                ]),
            add_smtp_recording(Context);
        _ ->
            ok
    end,
    ensure_page_guard(Context).

%% Upgrade once, append after existing recordings, and respect the site budget.
%% Later installs/restarts must not recreate a recording the administrator deleted.
add_smtp_recording(Context) ->
    {ok,D} = z_analytics:definition(#{
            <<"name">>=><<"SMTP (Email)">>,
            <<"event">>=><<"smtp">>,
            <<"dimension">>=><<"outcome">>,
            <<"groups">>=>5
        },
        z_analytics:registry(Context)),
    case m_analytics:store_create(D,<<"active">>,Context) of
        {ok,_} -> ok;
        {error,{exists,_}} -> ok;
        {error,budget_exceeded} -> ok
    end.

%% Count transport notifications, not unique messages or confirmed inbox delivery.
%% The delayed final-success notification repeats the original send; ignore it.
observe_email_sent(#email_sent{is_final=false},Context) -> smtp_event(<<"sent">>,Context);
observe_email_sent(#email_sent{},_Context) -> ok.

observe_email_failed(#email_failed{reason=bounce},Context) -> smtp_event(<<"bounced">>,Context);
observe_email_failed(#email_failed{is_final=false},Context) -> smtp_event(<<"retry">>,Context);
observe_email_failed(#email_failed{},Context) -> smtp_event(<<"failed">>,Context).

observe_email_bounced(#email_bounced{},Context) -> smtp_event(<<"bounced">>,Context).

smtp_event(Outcome,Context) ->
    m_analytics:record_event(<<"smtp">>,#{<<"outcome">>=>Outcome},Context).

observe_middleware(#middleware{on=handled},Context,_Context) ->
    z_analytics_request:record_guard(handled,Context),
    z_analytics_request:mark(Context);
observe_middleware(#middleware{on=rejected},Context,_Context) ->
    z_analytics_request:record_guard(rejected,Context),
    Context;
observe_middleware(_,Context,_) -> Context.

observe_module_activate(#module_activate{module = M},Context)
        when M =:= mod_traffic_guard; M =:= mod_analytics ->
    ensure_page_guard(Context),
    z_analytics_worker:reload(Context);
observe_module_activate(_,Context) -> z_analytics_worker:reload(Context).

ensure_page_guard(Context) ->
    case m_analytics:store_ensure_page_guard(Context) of
        ok -> ok;
        Error ->
            ?LOG_WARNING(#{in => mod_analytics, text => <<"Could not add default traffic guard graphs">>,
                result => error, reason => Error}),
            ok
    end.

observe_module_deactivate(_,Context) -> z_analytics_worker:reload(Context).

observe_admin_menu(#admin_menu{},Acc,Context) ->
    [
        #menu_item{
            id=admin_analytics,
            parent=admin_content,
            sort=950,
            label=?__("Site Analytics",Context),
            url={admin_analytics},
            visiblecheck={acl,use,mod_analytics}
        },
        #menu_item{
            id=admin_analytics_config,
            parent=admin_system,
            sort=950,
            label=?__("Analytics configuration",Context),
            url={admin_analytics_config},
            visiblecheck={acl,use,mod_admin_config}
        }
        |Acc
    ].

access_log(#{message:=#{payload:=#{site:=Site,timestamp:={Mega,Sec,_}}=Payload}}) ->
    case [E || E <- [z_analytics_request:request_event(Payload),
                     z_analytics_request:typed_event(Payload)], E =/= undefined] of
        [] -> ok;
        Events ->
            Context = z_context:new(Site),
            lists:foreach(
                fun({Event,D}) ->
                    z_analytics_worker:ingest(Event,D,Mega*1000000+Sec,Context)
                end, Events)
    end;
access_log(_) ->
    ok.

%% Signed postbacks still require server-side authorization and input validation.
event(#submit{message=create_recording},Context) ->
    Props=#{
        <<"name">>=>z_context:get_q(<<"name">>,Context),
        <<"description">>=>z_context:get_q(<<"description">>,Context,<<>>),
        <<"event">>=>z_context:get_q(<<"event">>,Context),
        <<"dimension">>=>z_context:get_q(<<"dimension">>,Context),
        <<"groups">>=>integer(z_context:get_q(<<"groups">>,Context)),
        <<"filters">>=>filters(Context)
    },
    result(m_analytics:create(Props,<<"active">>,Context),Context);
event(#postback{message={state,Args}},Context) ->
    result(m_analytics:set_state(proplists:get_value(id,Args),proplists:get_value(state,Args),Context),Context);
event(#postback{message={delete_recording,Args}},Context) ->
    %% The confirm dialog issues this signed postback. Recheck administrator
    %% access in the model even if the user's permissions changed meanwhile.
    case m_analytics:purge(proplists:get_value(id,Args),Context) of
        ok -> z_render:wire({reload,[]},Context);
        _ -> z_render:growl_error(?__("Could not delete the analytics measurement.",Context),Context)
    end;
event(#sort{items=Items,drop=#dragdrop{tag=Tag}},Context)
        when Tag =:= recordings; Tag =:= <<"recordings">> ->
    case m_analytics:reorder([Id || #dragdrop{tag=Id} <- Items],Context) of
        ok ->
            z_render:growl(?__("Display order saved.",Context),Context);
        _ ->
            Context1 = z_render:growl_error(?__("The analytics measurements changed. Reloading their current display order.",Context), Context),
            z_render:wire({reload,[]},Context1)
    end;
event(_,Context) ->
    Context.

filters(Context) ->
    lists:foldl(
        fun(N,A) ->
            K=z_context:get_q(<<"filter",N/binary>>,Context), V=z_context:get_q(<<"value",N/binary>>,Context),
            case {K,V} of
                {<<>>,_}->A; {undefined,_}->A;
                {_,B} when is_binary(B) -> A#{K=>[string:trim(X)||X<-binary:split(B,<<",">>,[global])]};
                _ -> A#{K=>[]}
            end
        end,
        #{},
        [<<"1">>,<<"2">>]).

integer(B) -> try z_convert:to_integer(B) catch _:_ -> 0 end.

result(ok,Context) -> z_render:wire({reload,[]},Context);
result({ok,_},Context) -> z_render:wire({reload,[]},Context);
result({error,budget_exceeded},Context) -> z_render:growl_error(?__("This analytics measurement exceeds the storage budget.",Context),Context);
result({error,{exists,_}},Context) -> z_render:growl_error(?__("An equivalent analytics measurement already exists.",Context),Context);
result(_,Context) -> z_render:growl_error(?__("Could not save. Check the event, breakdown and filter values.",Context),Context).
