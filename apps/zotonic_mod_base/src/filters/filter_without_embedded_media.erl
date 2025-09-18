%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010-2023 Arjan Scherpenisse
%% @doc 'without_embedded_media' filter, remove media ids embedded in texts.
%% @end

%% Copyright 2010-2023 Arjan Scherpenisse
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

-module(filter_without_embedded_media).
-moduledoc("
See also

[show\\_media](/id/doc_template_filter_filter_show_media), [embedded\\_media](/id/doc_template_filter_filter_embedded_media), [media\\_for\\_language](/id/doc_template_filter_filter_media_for_language)

Filter out media ids that are embedded in the `body`, `body_extra` and *text* blocks of your page.

This filter lets you loop over every image that is not included in the embedded `body` texts of the given page. This
makes it easy to only show images that have not been shown already:


```django
{% for media_id in m.rsc[id].media|without_embedded_media:id %}
    {% media media_id width=315 extent %}
{% endfor %}
```

The only argument to the filter is the id of the page that you want to consider for filtering from the body text.

There is an optional second argument to only consider media ids in the `body` and `body_extra` properties:


```django
{% for media_id in m.rsc[id].media|without_embedded_media:id:0 %}
    {% media media_id width=315 extent %}
{% endfor %}
```
").
-export([
    without_embedded_media/3,
    without_embedded_media/4
    ]).

without_embedded_media(Input, Id, Context) ->
    without_embedded_media(Input, Id, true, Context).

without_embedded_media(undefined, _Id, _IsCheckBlocks, _Context) ->
    undefined;
without_embedded_media(Input, undefined, _IsCheckBlocks, Context) ->
    z_template_compiler_runtime:to_list(Input, Context);
without_embedded_media(Input, Id, IsCheckBlocks, Context) ->
    case z_template_compiler_runtime:to_list(Input, Context) of
        [] -> [];
        Ids ->
            Ids -- filter_embedded_media:embedded_media(Id, IsCheckBlocks, Context)
    end.
