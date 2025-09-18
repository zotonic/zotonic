%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Show the spinner element

%% Copyright 2009 Marc Worrell
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

-module(scomp_base_spinner).
-moduledoc("
Add an AJAX activity indicator.

Whenever an AJAX call is made the HTML element with the id `#spinner` will be shown during the call.

You can add a spinner element to your page yourself or use this tag to place it somewhere on your page.

Example:


```erlang
{% spinner %}
```

Outputs the HTML code:


```erlang
<div id=\"spinner\" class=\"spinner\" style=\"display: none\">
  <img alt=\"activity indicator\" src=\"/lib/images/spinner.gif\" />
</div>
```

The spinner tag accepts a single argument “image”. The “image” argument must contain the URL for the image
displayed. It defaults to “`/lib/images/spinner.gif`”.
").
-behaviour(zotonic_scomp).

-export([vary/2, render/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

vary(_Params, _Context) -> nocache.

render(Params, _Vars, _Context) ->
    Image = proplists:get_value(image, Params, <<"/lib/images/spinner.gif">>),
    {ok, <<"<div id=\"spinner\" class=\"spinner\" style=\"display: none\"><img alt=\"activity indicator\" src=\"">>,Image,<<"\" /></div>">>}.
