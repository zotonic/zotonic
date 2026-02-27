%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%% @doc Escapes a text and makes 'nofollow' links of any url.

%% Copyright 2012 Marc Worrell
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

-module(filter_escape_link).
-moduledoc("
Convert any URLs in a plaintext into HTML links, with adding the `rel=\"nofollow\"` attribute, and replaces all newlines
with `<br\\>` tags.

Example:


```django
{{ \"Hello http://foo.bar/\\n\\nAnd bye.\"|escape_link }}
```

Outputs:


```django
\"Hello <a href=\"http://foo.bar/\" rel=\"noopener nofollow noreferrer\">http://foo.bar/</a><br /><br />And bye.\"
```

This filter is very useful when displaying user-generated plaintexts, like comments.

See also

[urlize](/id/doc_template_filter_filter_urlize)").

-export([escape_link/2]).

escape_link(Text, Context) ->
    z_sanitize:escape_link(Text, Context).

