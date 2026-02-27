%% @author    Marc Worrell <marc@worrell.nl>
%% @copyright 2021-2023 Marc Worrell
%% @doc 'urldecode' filter, decode the %-encoded characters in a string.
%% @end

%% Copyright 2022-2023 Marc Worrell
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

-module(filter_urldecode).
-moduledoc("
Decode a text where characters are encoded as URL-safe characters.

Translates all percent encoded characters back to their original encoding.

For example:


```django
{{ value|urldecode }}
```

When value is “msg%3DHello%26World” then the output is “msg=Hello&World”.

See also

[sanitize_url](/id/doc_template_filter_filter_sanitize_url), [is_site_url](/id/doc_template_filter_filter_is_site_url), [url_abs](/id/doc_template_filter_filter_url_abs), [url](/id/doc_template_filter_filter_url), [urlencode](/id/doc_template_filter_filter_urlencode)").
-export([urldecode/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

urldecode(undefined, _Context) ->
    undefined;
urldecode(#trans{} = Tr, Context) ->
    urldecode(z_trans:lookup_fallback(Tr, Context), Context);
urldecode(Input, _Context) when is_binary(Input) ->
    z_url:url_decode(Input);
urldecode(Input, _Context) when is_list(Input) ->
    z_url:url_decode(iolist_to_binary(Input));
urldecode(_Input, _Context) ->
    <<>>.
