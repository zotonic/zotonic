%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'timesince' filter, show a textual representation how far a date is from now.

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

-module(filter_timesince).
-moduledoc("
See also

[date](/id/doc_template_filter_filter_date), [now](/id/doc_template_tag_tag_now)

Show a readable version of a date/time difference.

Translates the difference between two dates into a simple readable string like “2 minutes, 10 seconds ago”.

Optionally takes an argument with the date to compare against, which is by default the current local date/time.

Example:


```django
{{ my_date|timesince }}
```

When “my\\_date” is `{{2008,12,10},{15,30,0}}` and the current date/time is `{{2009,11,4},{13,50,0}}` then this
outputs “10 months, 24 days ago”. When the time value is in the future then it outputs a string like “in X minutes”.

This function does not take daylight saving changes into account.



Extra arguments
---------------

The `timesince` filter can take several extra arguments, in the order of arguments:

*   Base date to use. Useful to show the difference between two dates, defaults to `now`
*   Text to use for the relative time designations, defaults to `\"ago,now,in\"`
*   Format for the printout. Now two options `1` and `2`, for the number of components shown. For example `2` will show *2 minutes, 10 seconds ago* where `1` will show *2 minutes ago*



Example
-------

Show the time between creation and modification of a resource:


```django
{{ id.created|timesince:id.modified:\"\":1 }}
```

This might display something like:


```django
10 days
```
").
-export([timesince/2, timesince/3, timesince/4, timesince/5]).


timesince(undefined, _Context) ->
    undefined;
timesince(Date, Context) ->
	z_datetime:timesince(Date, Context).
timesince(undefined, _Base, _Context) ->
    undefined;
timesince(Date, Base, Context) ->
	z_datetime:timesince(Date, Base, Context).
timesince(Date, Base, When, Context) ->
    z_datetime:timesince(Date, Base, When, Context).
timesince(Date, Base, When, Mode, Context) ->
    z_datetime:timesince(Date, Base, When, Mode, Context).

