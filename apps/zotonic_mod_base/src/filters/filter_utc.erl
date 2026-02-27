%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2023 Marc Worrell
%% @doc 'utc' filter, translate a datetime to UTC
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

-module(filter_utc).
-moduledoc("
Translates a datetime from local time to UTC.

For example:


```django
{{ id.modified|utc|date:\"Ymd:His\\\\Z\" }}
```

Displays the modification date and time of a resource in Universal Time.

See also

[date](/id/doc_template_filter_filter_date)").
-export([utc/2]).


utc(undefined, _Context) ->
	undefined;
utc(Input, _Context) ->
	hd(calendar:local_time_to_universal_time_dst(Input)).


