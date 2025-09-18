%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'ne_day' filter, check if two dates are on the different days

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

-module(filter_ne_day).
-moduledoc("
See also

[eq\\_day](/id/doc_template_filter_filter_eq_day)

Tests if two dates are not equal.

Tests if the value is a date and not equal to the argument. The value and the argument must be a tuple of the format
`{Y,M,D}` or `{{Y,M,D},{H,I,S}}`.

For example:


```django
{% if value|ne_day:othervalue %}different days{% endif %}
```

This outputs “different days” if value and othervalue are dates and different.

This is useful in combination with for example the if tag.
").
-export([ne_day/3]).


ne_day(A, B, Context) ->
    not filter_eq_day:eq_day(A,B, Context).
