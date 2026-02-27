%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'in_past' filter, test if a date is in the past.

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

-module(filter_in_past).
-moduledoc("
Tests if a date is in the past.

Tests if the value is a date and in the past. The value must be a tuple of the format `{Y,M,D}` or `{{Y,M,D},{H,I,S}}`.
When the value is not a date or datetime, the result is `undefined`.

For example:


```django
{% if value|in_past %}Those days have gone.{% endif %}
```

This outputs “Those days have gone.” if the value is a date and in the past.

See also

[in_future](/id/doc_template_filter_filter_in_future)").
-export([in_past/2]).


in_past({_,_,_} = Date, _Context) ->
	{LocalDate, _} = erlang:universaltime(),
	Date < LocalDate;
in_past({{_,_,_}, {_,_,_}} = DateTime, _Context) ->
	DateTime < erlang:universaltime();
in_past(_, _Context) ->
	undefined.


