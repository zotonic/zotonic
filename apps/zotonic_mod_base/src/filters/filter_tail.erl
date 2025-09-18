%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'tail' filter, return the tail of a list

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

-module(filter_tail).
-moduledoc("
See also

[first](/id/doc_template_filter_filter_first), [nthtail](/id/doc_template_filter_filter_nthtail)

Fetch the tail of a list.

Returns the tail of a list. Useful when you want to skip the first element of a list when looping.

For example:


```django
{% for a in value|tail %}{{ a|format_number }}{% endfor %}
```

When value is the list `[1,2,3]` then the output is `23`.
").
-export([tail/2]).


tail(In, Context) ->
    case z_template_compiler_runtime:to_list(In, Context) of
        [_|T] -> T;
        _ -> []
    end.


