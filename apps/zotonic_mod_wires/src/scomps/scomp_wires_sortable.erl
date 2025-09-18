%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Based on code (c) 2008-2009 Rusty Klophaus
%%
%% @doc Mark an element as a sortable.  A sortable is sorted inside a sorter

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

-module(scomp_wires_sortable).
-moduledoc("
Mark an element as sortable.

Sortables are part of a sorter. Sortables in a sorter can be reordered by drag & drop.

Example:


```erlang
{% sorter id=\"sorter\" tag=\"mysorter\" %}
{% sortable id=\"sort1\" tag=1 %}
{% sortable id=\"sort2\" tag=2 %}
<ul id=\"sorter\">
  <li id=\"sort1\">Sortable 1</li>
  <li id=\"sort2\">Sortable 2</li>
</ul>
```

This creates a list where the order of the list items can be changed by dragging and dropping them.

When the order of the sortables is changed, an event is sent to the sorter’s page
[controller](/id/doc_glossary#term-controller) or [delegate](/id/doc_glossary#term-delegate). The event contains the
ordered list of sortable tags.

The event handler function is called like:


```erlang
event({sort, Sortables, Sorter}, Context).
```

Where “Sortables” is the list of sortables and “Sorter” is the sorter. Both are “#dragdrop” records:


```erlang
-record(dragdrop, {tag, delegate, id}).
```

Where “tag” is the tag of the sortable or sorter, “delegate” is the module that handles the event and “id”
is the HTML id of the sortable or sorter.

Note

if the tag is a string then the `#dragdrop` tag will be an atom.

The sortable can have the following arguments:

| Argument | Description                                                                      | Example                 |
| -------- | -------------------------------------------------------------------------------- | ----------------------- |
| id       | The HTML id of the sortable element.                                             | id=”mysortable1”        |
| tag      | Tag that identifies the sortable to the event handler. Can be any value. A string will be converted to an atom. | tag=”my\\\\_atom\\\\_value” |
| delegate | The delegate of the sortable, currently unused will be passed in the sortables’s #dragdrop record. | delegate=”mymodule”     |
| class    | A CSS class that will be added to the sortable. The class “sortable” will always be added. | class=”dragitem”        |

See also

the [sorter](/id/doc_template_scomp_scomp_sorter#scomp-sorter) tag.
").
-behaviour(zotonic_scomp).

-export([vary/2, render/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

vary(_Params, _Context) -> nocache.

% -record(sortitem, {?ELEMENT_BASE(element_sortitem), tag, body=[] }).

render(Params, _Vars, Context) ->
    Id       = proplists:get_value(id, Params),
    Tag      = proplists:get_value(tag, Params),
    Delegate = proplists:get_value(delegate, Params),
    Class    = proplists:get_all_values(class, Params),

	% Get properties...
	Delegate1    = case Delegate of
	                undefined -> z_context:get_controller_module(Context);
	                _ -> z_convert:to_atom(Delegate)
	               end,

   case Id of
       undefined ->
           {error, "sortable scomp, please give the id of the sortable element"};
       _ ->
        	PickledTag  = z_crypto:pickle({Tag,Delegate1,Id}, Context),
        	Script      = io_lib:format("z_sortable($('#~s'), '~s');", [Id, PickledTag]),

            Actions = [
                        {script,    [{script, Script}]},
                        {add_class, [{class, ["sortable "|Class]}]}
                    ],

        	{ok, z_render:wire(Id, Actions, Context)}
    end.
