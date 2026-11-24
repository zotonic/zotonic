%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Return the next page in a menu or collection.
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

-module(filter_next_in).
-moduledoc(#{
    zotonic_keywords => ["reference", "frontend_developer", "template_filter", "navigation", "collection"]
}).
-moduledoc(<<"
Return the next page in a menu or collection: `{{ id|next_in:container_id }}`.

Visits the container itself, then menu children and ordered `haspart` descendants
in depth-first order. Invisible subtrees and repeated resources are skipped.
Returns `undefined` at the boundary or when the page is not in the container.
">>).
-export([next_in/3]).

-spec next_in(Id, Container, Context) -> m_rsc:resource_id() | undefined when
    Id :: m_rsc:resource(),
    Container :: m_rsc:resource(),
    Context :: z:context().
next_in(Id, Container, Context) ->
    z_rsc_sequence:adjacent(next, Id, Container, Context).
