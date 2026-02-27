%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'slugify' filter, translate a string to a slug

%% Copyright 2010 Marc Worrell
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

-module(filter_slugify).
-moduledoc("
Converts a text into a slug.

Makes the value safe for use as a part in an url. Mostly used for adding titles or descriptions to an url.

For example:


```django
{{ value|slugify }}
```

When value is “Nichts is unmöglich!” then the output will be “nichts-is-unmoglich”.

See also

[stringify](/id/doc_template_filter_filter_stringify)").
-export([slugify/2]).


slugify(undefined, _Context) ->
    undefined;
slugify({trans, _} = Tr, Context) ->
    slugify(z_trans:lookup_fallback(Tr, Context), Context);
slugify(Input, _Context) ->
    z_string:to_slug(Input).
