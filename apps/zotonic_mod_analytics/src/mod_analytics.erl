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
Site Analytics provides lightweight local analytics for site traffic, popular content,
email activity and module-defined events. Enable `mod_analytics` and open
**Content → Site Analytics**. Configure recordings under **System → Analytics configuration**.
The default Languages recording starts paused and can be resumed there.

## Permissions and privacy

Reports require `use mod_analytics`. Configuration and recording mutations require
site administrator access (`use mod_admin_config`); the two permissions are independent.
Request events exclude users with `use mod_admin` and requests marked `analytics_exclude`.
DNT and GPC headers do not exclude requests from aggregate analytics. With
`require_consent` enabled, the context must contain `analytics_consent = true`.
Ordinary logged-in users are otherwise eligible.
No visitor identifiers, tracking cookies or raw request logs are stored.

## Recordings and storage

Each recording selects one event, fixed filters and at most one breakdown dimension.
Counters are aggregated asynchronously into capped minute, quarter-hour, hour and day
slots in the site's PostgreSQL database. Popular content uses bounded approximate
rankings over moving periods. These are event counts, not unique visitors.
Ingestion is best-effort: overload can drop events, and reports indicate incomplete data.
Pausing preserves history and its storage reservation; deleting releases the reservation.

Per-site configuration:

| Key | Default | Meaning |
| --- | --- | --- |
| `row_budget` | `1000000` | Maximum reserved group/time slots across recordings. |
| `require_consent` | `false` | Require explicit context consent for request-associated events. |

## Integration

`controller_page` and `controller_template` default to the `page` dispatch analytics type;
`controller_api` defaults to `api`. Override with `{analytics, Type}`, or disable a route
with `{analytics, false}`. Page events count successful public HTML GET responses.
Other classified requests can include errors and expose HTTP status for filters or breakdowns.
SMTP observers count sent, retry, failed and bounced notifications independently of requests.

Modules declare namespaced events using the `analytics_events` fold notification and call
`m_analytics:record_event/3` for background events or `/4` with the current request.
See `m_analytics` and the application README for calling conventions and schema examples.
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

observe_middleware(#middleware{on=handled},Context,_Context) -> z_analytics_request:mark(Context);
observe_middleware(_,Context,_) -> Context.

observe_module_activate(#module_activate{module = M},Context)
        when M =:= mod_crawl_guard; M =:= mod_analytics ->
    ensure_page_guard(Context),
    z_analytics_worker:reload(Context);
observe_module_activate(_,Context) -> z_analytics_worker:reload(Context).

ensure_page_guard(Context) ->
    case m_analytics:store_ensure_page_guard(Context) of
        ok -> ok;
        Error ->
            ?LOG_WARNING(#{in => mod_analytics, text => <<"Could not add default crawl guard graphs">>,
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
