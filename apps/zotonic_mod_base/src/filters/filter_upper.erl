%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2023 Marc Worrell
%% @doc 'upper' filter, translate to upper case.
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

-module(filter_upper).
-moduledoc("
Translates the value to upper case.

For example:


```django
{{ value|upper }}
```

When value is “Hello World” then the output is “HELLO WORLD”.

**Note:** There is partial support for multi-byte unicode characters.

See also

[capfirst](/id/doc_template_filter_filter_capfirst), [lower](/id/doc_template_filter_filter_lower)").
-export([upper/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

upper(undefined, _Context) ->
    undefined;
upper(#trans{} = Tr, Context) ->
    upper(z_trans:lookup_fallback(Tr, Context), Context);
upper(Input, _Context) when is_list(Input) or is_binary(Input) ->
    z_string:to_upper(Input);
upper(Input, Context) ->
    upper(z_template_compiler_runtime:to_list(Input, Context), Context).
