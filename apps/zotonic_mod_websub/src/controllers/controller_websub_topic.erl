%% @copyright 2026 Marc Worrell
%% @author Marc Worrell <marc@worrell.nl>
%% @doc Stable, language-independent JSON representation for WebSub delivery.
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

-module(controller_websub_topic).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "integrator", "controller", "export_and_syndication", "structured_data", "websub", "json", "http"
    ]
}).
-moduledoc(<<"Serve the complete JSON representation advertised as a resource's WebSub topic.

The `websub_topic` dispatch is `/.zotonic/websub/topic/:id` and supports GET and
HEAD. It offers only `application/json`, independently of the representations
available through the semantic `/id` URI. The export uses the `x-default` language
context, avoiding a language-dependent delivery topic.

Successful responses use the resource-export envelope:

```json
{\"status\":\"ok\",\"result\":{\"uri\":\"https://example.com/id/123\",\"resource\":{}}}
```

The example abbreviates the result; `m_rsc_export:full/2` supplies the complete
export, including connections, media metadata, and discovery links. The resource's
`result.uri` remains its language-less semantic identifier. The HTTP Link self
points to this JSON topic, and the hub link identifies `controller_websub`.

Only authoritative local resources are served. Export access uses the current
request's ACL context; an export error returns 403, while a missing or
non-authoritative resource returns 404. Successful responses disable caching and
include resource discovery headers. Private exports require authorized access.

`m_websub` uses the same full-export representation for delivery to verified
subscribers, including non-Zotonic subscribers. See `mod_websub` for discovery,
subscription, and the distinction between resource identity and delivery topic."/utf8>>).

-export([allowed_methods/1, content_types_provided/1, process/4]).

allowed_methods(Context) ->
    {[<<"GET">>, <<"HEAD">>], Context}.
content_types_provided(Context) ->
    {[{<<"application">>, <<"json">>, []}], Context}.
process(_, _, _, Context0) ->
    Context = z_context:ensure_qs(Context0),
    Id = m_rsc:rid(z_context:get_q(<<"id">>, Context), Context),
    case m_rsc:p_no_acl(Id, <<"is_authoritative">>, Context) of
        true ->
            case m_rsc_export:full(Id, z_context:set_language('x-default', Context)) of
                {ok, Export} ->
                    Ctx = z_context:set_resource_headers(Id, z_context:set_nocache_headers(Context)),
                    {z_json:encode(#{<<"status">> => <<"ok">>, <<"result">> => Export}), Ctx};
                {error, _} ->
                    {{halt, 403}, Context}
            end;
        _ ->
            {{halt, 404}, Context}
    end.
