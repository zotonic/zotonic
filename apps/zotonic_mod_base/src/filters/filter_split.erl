%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%% @copyright 2010-2025 Dmitrii Dimandt
%% @doc 'unjoin' filter, "reverse" of join, split a string into a list.
%% @end

%% Copyright 2010-2025 Dmitrii Dimandt
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

-module(filter_split).
-moduledoc("
See also

[join](/id/doc_template_filter_filter_join)

Splits the filter value into a list of values.

The input value is split by the filter argument, for example:


```django
{{ \"foo bar baz\"|split:\" \" }}
```

Will create the list `[\"foo\", \"bar\", \"baz\"]`.
").
-export([split/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

split(undefined, _Sep, _Context) ->
	undefined;
split(V, undefined, _Context) ->
	V;
split(<<>>, _Sep, _Context) ->
	[];
split([], _Sep, _Context) ->
	[];
split(#trans{} = Tr, Sep, Context) ->
    split(z_trans:lookup_fallback(Tr, Context), Sep, Context);
split(String, #trans{} = Tr, Context) ->
    split(String, z_trans:lookup_fallback(Tr, Context), Context);
split(String, Sep, _Context) when is_binary(String) ->
	binary:split(String, z_convert:to_binary(Sep), [global]);
split(String, Sep, _Context) ->
    z_string:split(z_convert:to_list(String), z_convert:to_list(Sep)).

