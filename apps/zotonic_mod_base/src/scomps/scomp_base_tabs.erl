%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-07-15
%% @doc Add the tabs UI to an element

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

-module(scomp_base_tabs).
-moduledoc("
Make a HTML element into a tab set.

Note

There is no default styling for jQuery UI elements in the zotonic CSS files. See these two threads in the zotonic users
mailinglist: [does tabs scomp still work?](https://groups.google.com/d/topic/zotonic-users/mIxPpKSzugM/discussion), and
[playing with tabs scomp](https://groups.google.com/d/topic/zotonic-users/BnZtNvVWds0/discussion). See also the [jQuery
UI](http://jqueryui.com) documentation.

This is a simple interface to the jQuery UI tabs functionality. It will show tabs to switch between different panes.

The only argument is “id” which is the id of the container of the tabs.

The following example will show an interface with three tabs:


```erlang
{% tabs id=\"tabs\" %}
<div id=\"tabs\">
  <ul>
    <li><a href=\"#tabs-1\">Nunc tincidunt</a></li>
    <li><a href=\"#tabs-2\">Proin dolor</a></li>
    <li><a href=\"#tabs-3\">Aenean lacinia</a></li>
  </ul>
  <div id=\"tabs-1\">
    <p>Proin elit arcu, rutrum commodo, vehicula tempus.</p>
  </div>
  <div id=\"tabs-2\">
    <p>Morbi tincidunt, dui sit amet facilisis feugiat.</p>
  </div>
  <div id=\"tabs-3\">
    <p>Mauris eleifend est et turpis.</p>
  </div>
</div>
```
").
-behaviour(zotonic_scomp).

-export([vary/2, render/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

vary(_Params, _Context) -> nocache.

render(Params, _Vars, Context) ->
    Id = proplists:get_value(id, Params),
    Script = case proplists:get_value(hash, Params) of
                true ->
                    [ "$('#", Id, "').tabs({ select: function(event, ui) { window.location.hash = ui.tab.hash; }});" ];
                _ ->
                    [ "$('#", Id, "').tabs();" ]
             end,
    {ok, z_render:wire({script, [{script, Script}]}, Context)}.

