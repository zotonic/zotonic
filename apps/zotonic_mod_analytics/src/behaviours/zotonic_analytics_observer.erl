%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Notification contracts owned by Site Analytics.
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

-module(zotonic_analytics_observer).
-moduledoc(#{zotonic_notification_behaviour => true}).
-moduledoc("Notification contracts owned by mod_analytics.").

-include_lib("zotonic_mod_analytics/include/analytics.hrl").

-type event_definition() :: #{
    name := binary(),
    version := pos_integer(),
    title := binary(),
    dimensions := #{
        binary() => text | [binary()]
    }
}.

-doc(#{
        zotonic_keywords => [
            "reference", "backend_developer", "notification",
            "logging_and_monitoring", "privacy_and_consent"
        ]
}).

-doc(<<"Declare events that Site Analytics can record. This is a `foldl` notification,
starting with an empty list. Prepend your declarations to the accumulator and
return the resulting list; preserve declarations from other observers.

Include `zotonic_mod_analytics/include/analytics.hrl` to use `#analytics_events{}`.
The notification record has no fields. Enabled modules export this observer:

```erlang
observe_analytics_events(#analytics_events{}, Acc, _Context) ->
    [#{name => <<\"mod_newsletter:sent\">>, version => 1,
       title => <<\"Newsletter sent\">>,
       dimensions => #{<<\"kind\">> => [<<\"digest\">>, <<\"single\">>],
                       <<\"language\">> => text}} | Acc].
```

Each declaration contains `name`, a namespaced binary of at most 100 bytes;
`version`, a positive integer; `title`, a binary of at most 200 bytes; and
`dimensions`, a map of at most eight fields. Dimension names are nonempty binaries
of at most 40 bytes. A type is `text` or an enum of at most 100 nonempty UTF-8
binaries, each at most 255 bytes. Properties must match the declared schema.
Use coarse categories, not personal data or free-form input. Fields named `ip`,
`user_id`, `session_id` and `user_agent` are rejected. Only the first 64 module
declarations are considered; duplicate names and invalid declarations are ignored.
Increment the version when the schema or meaning changes: existing recordings
must not silently start collecting a different event.

After a successful domain operation, record a background event:

```erlang
m_analytics:record_event(<<\"mod_newsletter:sent\">>,
    #{<<\"kind\">> => <<\"digest\">>, <<\"language\">> => <<\"en\">>}, Context).
```

For a request-associated event, pass `z_context:get_reqdata(Context)` as the third
argument to `record_event/4`. Request exclusions and consent policy then apply;
request dimensions declared in the schema are derived from the context and cannot
be overridden. The three-argument API does not implicitly associate a request.
Events are best-effort aggregate measurements, not durable audit records.
">>).

-callback observe_analytics_events(Notification, Acc, Context) -> Result when
    Notification :: #analytics_events{},
    Acc :: [event_definition()],
    Context :: z:context(),
    Result :: [event_definition()].

-optional_callbacks([observe_analytics_events/3]).
