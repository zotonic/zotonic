%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013-2023 Marc Worrell
%% @doc Pickle a term, for inclusion in an input field or query argument.
%% @end

%% Copyright 2013-2023 Marc Worrell
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

-module(filter_pickle).
-moduledoc("
Pickle an Erlang value so that it can be safely submitted with a form.

The Erlang value is encoded using erlang:term_to_binary/1 and signed with the site’s secret key. In Erlang the
value can be unpacked using z_utils:depickle/2

Usage:


```django
<input type=\"hidden\" name=\"confirm\" value=\"{{ 'Hello world' | pickle }}\" />
```

This will generate something like:


```django
<input type=\"hidden\" name=\"confirm\" value=\"duZTXcXaxuruD3dhpt-rxWokrhuDbQAAAAtIZWxsbyB3b3JsZA\" />
```
").
-export([pickle/2]).

pickle(Data, Context) ->
    z_crypto:pickle(Data, Context).
