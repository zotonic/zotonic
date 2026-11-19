%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell <marc@worrell.nl>
%% @doc Display the diagnostic from legacy serialized delivery failures.
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

-module(filter_mailinglist_delivery_detail).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "frontend_developer", "template_filter",
        "mailing_lists", "email_delivery", "format"
    ]
}).
-moduledoc(<<"
Extract the SMTP diagnostic from legacy `{bounce, <<\"...\">>}` and related
mailing-run details, including nested errors and missing diagnostics. New details
are stored as plain text. Only known wrappers, quoted strings and restricted
identifiers are accepted; arbitrary Erlang terms are never evaluated. The returned text is not HTML-safe: use the escape filter.
">>).
-export([mailinglist_delivery_detail/2]).

-spec mailinglist_delivery_detail(Detail, Context) -> binary() | undefined when
    Detail :: binary() | undefined,
    Context :: z:context().
mailinglist_delivery_detail(Detail, _Context) ->
    z_mailinglist_delivery_detail:format(Detail).
