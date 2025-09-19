%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2023 Marc Worrell
%% @doc 'unescape' filter, remove html escaping.
%% @end

%% Copyright 2010-2023 Marc Worrell
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

-module(filter_unescape).
-moduledoc("
See also

[escape](/id/doc_template_filter_filter_escape), [force\\_escape](/id/doc_template_filter_filter_force_escape)

Removes HTML escaping from a text.

Expands the entities added by the [escape](/id/doc_template_filter_filter_escape) filter or
[force\\_escape](/id/doc_template_filter_filter_force_escape) filter. This is useful when you want to display a field
from the database in a text-only format medium.

For example:


```django
Title: {{ m.rsc[id].title|unescape }}
```

Be careful that you only use this filter when you are absolutely sure that the output is not used in HTML or XML.
").
-export([unescape/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

unescape(undefined, _Context) ->
	<<>>;
unescape(#trans{} = Tr, Context) ->
    unescape(z_trans:lookup_fallback(Tr, Context), Context);
unescape(In, _Context) ->
	z_html:unescape(In).

