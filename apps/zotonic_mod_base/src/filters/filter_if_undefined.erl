%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2012 Arjan Scherpenisse
%% @doc Fall back to given value *only* when the value is undefined

%% Copyright 2012 Arjan Scherpenisse
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

-module(filter_if_undefined).
-moduledoc("
See also

[default](/id/doc_template_filter_filter_default), [is\\_defined](/id/doc_template_filter_filter_is_defined),
[is\\_undefined](/id/doc_template_filter_filter_is_undefined), [if](/id/doc_template_filter_filter_if)

Tests whether a value is undefined, returning the given argument.

Whereas the |default filter also falls back to the default value when a value is an empty string or `false`,
this filter only falls back to its value when the input value is the Erlang `undefined` atom.

This can be used for setting values which default to true if they are never set.

For example:


```django
{% if value|if_undefined:true %}The value is true or undefined{% endif %}
```

If the value is `undefined`, the output will be “The value is true or undefined”.

Alias for [default_if_none](/id/doc/template_filter_filter_default_if_none).

").
-export([if_undefined/3]).

if_undefined(V, Default, Context) ->
    case not(filter_is_defined:is_defined(V, Context)) of
        true ->
            Default;
        false ->
            V
    end.
