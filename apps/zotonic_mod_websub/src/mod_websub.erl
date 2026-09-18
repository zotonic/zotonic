%% @copyright 2021-2026 Marc Worrell
%% @author Marc Worrell <marc@worrell.nl>
%% @doc Publish and subscribe to resources between sites using WebSub.
%% @end

%% Copyright 2021-2026 Marc Worrell
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

-module(mod_websub).
-moduledoc("
This module manages two different subscription lifecycles:

## Export subscriptions

An export subscription is created when a remote site subscribes to one of our local
resource topics through the WebSub callback controller. The subscription is stored
in `websub_export` and points from a local resource to the subscriber callback URL.

When the local resource changes, its current version is queued for every active export
subscription. The queue keeps only the newest pending version per subscription, so
outdated pushes are discarded before they are sent.

An export subscription stays active until one of these things happens:

* the remote site unsubscribes successfully
* the subscribed local resource is deleted, which cascades the subscription away
* the subscriber no longer has access to the resource, in which case the subscription
  is removed on push processing
* repeated push failures exhaust the retry budget, after which the subscription is
  flagged erroneous

Erroneous export subscriptions are retained for inspection and are removed during
daily cleanup after about a month.

## Import subscriptions

An import subscription is created when this site subscribes to a remote topic. The
subscription is stored in `websub_import` together with the local resource id, the
subscribing user id, and whether the original subscription used credential-backed
fetching.

When a push is received, the module records the pushed version and then:

* queues a refetch from the source if the subscription uses credentials
* queues the pushed anonymous payload directly if the subscription has no credentials

Again, only the newest pending version per import subscription is kept in the queue,
so older work is superseded.

An import subscription stays active until one of these things happens:

* this site unsubscribes from the remote hub
* credentialed refetch is refused by the source, which is recorded on the subscription
  and followed by unsubscribe
* the mapped local resource disappears and `local_rsc_id` becomes `null`, after which
  the subscription is unsubscribed on the next matching push or during daily maintenance

The minute tick only schedules queue processing; the actual work runs in a unique
sidejob so overlapping queue runs for the same site do not happen.
WebSub specification is at https://www.w3.org/TR/websub/
").

-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Resource WebSub").
-mod_description("Publish and subscribe to resources between sites using WebSub.").
-mod_depends([ cron ]).
-mod_schema(1).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    observe_resource_headers/3,
    observe_rsc_update_done/2,
    observe_tick_1m/2,
    observe_tick_24h/2,
    manage_schema/2,
    sidejob_check_queues/1
]).

observe_resource_headers(#resource_headers{ id = Id }, Acc, Context) when is_integer(Id) ->
    ContextNoLanguage = z_context:set_language('x-default', Context),
    HubUrl = z_context:abs_url(z_dispatcher:url_for(websub, [], ContextNoLanguage), ContextNoLanguage),
    SelfPath = z_dispatcher:url_for(id, [ {id, Id} ], ContextNoLanguage),
    SelfUrl = z_context:abs_url(SelfPath, Context),
    [
        {<<"link">>, <<"<", HubUrl/binary, ">; rel=\"hub\"">>},
        {<<"link">>, <<"<", SelfUrl/binary, ">; rel=\"self\"">>}
        | Acc
    ];
observe_resource_headers(#resource_headers{}, Acc, _Context) ->
    Acc.


observe_rsc_update_done(#rsc_update_done{ action = Action, id = Id, post_props = Props }, Context)
    when Action =:= insert; Action =:= update ->
    case maps:get(<<"version">>, Props, undefined) of
        Version when is_integer(Version) ->
            m_websub:queue_push(Id, Version, Context);
        _ ->
            ok
    end;
observe_rsc_update_done(#rsc_update_done{}, _Context) ->
    ok.

observe_tick_1m(tick_1m, Context) ->
    case z_sidejob:start_site_unique(?MODULE, ?MODULE, sidejob_check_queues, [], Context) of
        {ok, _Pid} ->
            ok;
        {error, already_running} ->
            ok;
        {error, overload} ->
            ok
    end.

observe_tick_24h(tick_24h, Context) ->
    ok = m_websub:cleanup_deleted_imports(Context),
    ok = m_websub:cleanup(Context).

manage_schema(Version, Context) ->
    m_websub:manage_schema(Version, Context).

sidejob_check_queues(Context) ->
    ok = m_websub:process_push_queue(Context),
    ok = m_websub:process_import_queue(Context).
