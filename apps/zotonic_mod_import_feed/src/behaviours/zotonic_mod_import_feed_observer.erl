%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Observer behaviour for feed import notifications.
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

-module(zotonic_mod_import_feed_observer).
-author("Marc Worrell <marc@worrell.nl>").

-moduledoc(#{
    zotonic_keywords => [
        "reference", "backend_developer", "notification", "import_and_migration", "import"
    ]
}).
-moduledoc("""
Observer behaviour for the notifications published by `mod_import_feed`.

## Implementing an observer

Add this behaviour to your site or module to type-check its feed import observers.
You **must also include** `zotonic_mod_import_feed/include/mod_import_feed.hrl`
to use the `#import_feed_item{}` notification record. Declaring the behaviour does
not import the record, and the core `zotonic.hrl` does not define it.

```erlang
-behaviour(zotonic_mod_import_feed_observer).
-include_lib("zotonic_mod_import_feed/include/mod_import_feed.hrl").
-export([observe_import_feed_item/2]).

observe_import_feed_item(#import_feed_item{}, _Context) ->
    undefined.
```

Enable the observing site/module and `mod_import_feed`. Zotonic registers exported
observer functions on active modules. Both callbacks are optional: implement either
`observe_import_feed_item/2` or `pid_observe_import_feed_item/3` for this notification.
The PID-aware variant receives the observing module's process ID as its first
argument; its remaining arguments and return contract are identical.
""").

-include_lib("zotonic_mod_import_feed/include/mod_import_feed.hrl").

-export_type([import_result/0]).

-type import_result() ::
    undefined
    | {ok, m_rsc:resource_id()}
    | {ok, m_rsc:resource_id(), #{binary() => m_rsc:resource_id()}}
    | {skip, term()}
    | {error, term()}.

-doc(#{
    zotonic_keywords => [
        "reference", "backend_developer", "notification", "import_and_migration", "import"
    ]
}).
-doc("""
Handle an entry before the feed importer writes its resource, media or connections.

Type: [first](/id/doc_developerguide_notifications#notification-first).
Observers are called until one returns a value other than `undefined`. If all return
`undefined`, the default importer handles the entry. This is a replacement-import
hook, not a fold for returning modified entry data.

Include the record definition in every module using this notification:

```erlang
-include_lib("zotonic_mod_import_feed/include/mod_import_feed.hrl").
```

## Notification fields

`#import_feed_item{}` contains:

* `item`: a map with the stable source `uri` (binary), resource `props` (a map),
  `edges` (a list of `{Predicate, ObjectIdOrUri}` pairs), and `medium` (a URL or
  `undefined`). Ordinary entries use predicate IDs; native Zotonic exports can
  also use predicate names. `media_aliases` maps image variants to their selected
  source URLs.
* `dependents`: a list of maps with `uri`, `props` and `medium`, describing media
  resources for ordinary feeds. For native Zotonic entries these describe referenced
  resources, with empty `props` and `medium` set to `undefined`; they are not fully
  fetched resource exports.
* `feed`: the saved feed settings and metadata, including its `id`, `url` and
  configured `user_id`. Internal `user_groups` and `lease_token` fields are omitted.
* `raw`: the original parsed feed entry, with WordPress response data when enrichment
  succeeds. Its representation depends on the feed parser and enrichment step.

For native Zotonic entries, `item` additionally contains `zotonic_export` (the decoded
resource export), `native_options` (core import options), and `default_connections`.
The default importer delegates these entries to the Zotonic resource import routines.

## Context and timing

The callback runs synchronously inside the entry's database transaction, using the
configured import user's context. Reuse this context for database and resource
operations so writes participate in the transaction and enforce that user's ACLs.
Unchanged successful entries and deleted source identities are skipped before the
notification. Explicit reimports bypass the unchanged check and invoke it again.
Fetching, parsing, enrichment and configured defaults/cleanup precede this callback;
the data is still untrusted. Custom importers must sanitize content, enforce ACLs,
and preserve stable source identities to avoid duplicates.

## Return values

* `undefined`: let subsequent observers, then the default importer, handle the entry.
* `{ok, Id}`: the observer has imported the article. `Id` must be an existing integer
  resource ID editable by the import user. The feed importer records its source URI
  and successful import status.
* `{ok, Id, UriToIdMap}`: additionally register dependent resources for feed history
  and URI-based deduplication. Keys are binary source URIs; values are existing integer
  resource IDs visible to the import user.
* `{skip, Reason}`: deliberately skip this entry and record the reason in feed history.
  A skipped entry can be offered again on a subsequent poll.
* `{error, Reason}`: fail the entry, roll back its database transaction, and record
  the error. A later poll can retry it. External side effects are not rolled back.

Other return values fail the import with `invalid_observer_result`.
""").
-callback observe_import_feed_item(Notification, Context) -> Result
    when
        Notification :: #import_feed_item{},
        Context :: z:context(),
        Result :: import_result().

-doc("""
PID-aware variant of `observe_import_feed_item/2`.

`Pid` is the observing module's process ID. The notification fields, transactional
context and return values are identical to `observe_import_feed_item/2`. Include
`zotonic_mod_import_feed/include/mod_import_feed.hrl` to use the notification record.
""").
-callback pid_observe_import_feed_item(Pid, Notification, Context) -> Result
    when
        Pid :: pid(),
        Notification :: #import_feed_item{},
        Context :: z:context(),
        Result :: import_result().

-optional_callbacks([
    observe_import_feed_item/2,
    pid_observe_import_feed_item/3
]).
