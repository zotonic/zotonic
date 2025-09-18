%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Based on code (c) 2008-2009 Rusty Klophaus
%%
%% @doc Make an element draggable
%%      {% @draggable id="xxx" tag="sometag" group="group1" group="group2" handle="selector" %}

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

-module(scomp_wires_draggable).
-moduledoc("
Mark a html element as draggable.

The draggable tag is used in conjunction with the [\\{% droppable
%\\}](/id/doc_template_scomp_scomp_droppable#scomp-droppable) tag to implement drag & drop. Elements that are marked as
draggable can be dropped on elements marked as droppable. Drag & drop generates dragdrop events that are sent to the
[controller](/id/doc_glossary#term-controller) or the [delegate](/id/doc_glossary#term-delegate).

For example:


```erlang
<div id=\"drag1\">Drag me</div>
{% draggable id=\"drag1\" tag=\"drag-one\" %}
```

Now the div with id “drag1” can be dragged. When it is dropped then `event/2` of the controller or delegate Erlang
module is called signaling the drag (and also the drop to the module receiving the droppable events):


```erlang
event({drag, Drag, Drop}, Context).
```

Where both Drag and Drop are `#dragdrop` records:


```erlang
-record(dragdrop, {tag, delegate, id}).
```

The draggable tag accepts the following arguments:

| Argument | Description                                                                      | Example                                      |
| -------- | -------------------------------------------------------------------------------- | -------------------------------------------- |
| id       | The id of the element that becomes draggable.                                    | id=”drag1”                                   |
| tag      | The tag of the draggable that is sent as part of the drag and drop events. This can be any value, including a tuple. | tag=\\\\{subject\\\\_list id=42 changed=false\\\\} |
| clone    | Clone the element when dragging or drag the element itself. Defaults to false.   | clone=true                                   |
| revert   | When the element has to revert to its starting position. Defaults to “invalid”, i.e. when the drop was above an invalid position. Other options are true, false and “valid”. | revert=false                                 |
| axis     | Constrain the drag movement to either the x or y direction. Normally the drag is not constrained. Acceptable values are “x” or “y” axis=”x” |                                              |
| handle   | The css selector that is the handle to drag with. Defaults to the whole element. | handle=”handleclass”                         |
| group    | The name of this drag group, for use in the droppable element’s “accept” argument. Multiple groups are allowed. | group=”edges”                                |
| opacity  | Change the opacity while dragging. Defaults to “0.8”.                            | opacity=”0.5”                                |
| delegate | The Erlang module that will receive the drag event after a successful drop.      |                                              |

See also

the [droppable](/id/doc_template_scomp_scomp_droppable#scomp-droppable) tag.
").
-behaviour(zotonic_scomp).

-export([vary/2, render/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

vary(_Params, _Context) -> nocache.

%% -record(draggable, {?ELEMENT_BASE(element_draggable), tag, body=[], group, handle, clone=true, revert=true}).

render(Params, _Vars, Context) ->
    Id      = proplists:get_value(id, Params),
    Tag     = proplists:get_value(tag, Params),
    Clone   = proplists:get_value(clone, Params, false),
    Revert  = proplists:get_value(revert, Params, <<"invalid">>),
    Axis    = proplists:get_value(axis, Params),
    Handle  = proplists:get_value(handle, Params),
    Groups  = proplists:get_all_values(group, Params),
    Opacity = proplists:get_value(opacity, Params, <<"0.8">>),
    Delegate= proplists:get_value(delegate, Params),
    ConnectToSortable= proplists:get_value(to_sorter, Params),

    Groups1 = case Groups of
                [] -> ["dragdrop"];
                _ -> Groups
               end,

	% Get properties...
	Delegate1    = case Delegate of
	                undefined -> z_context:get_controller_module(Context);
	                _ -> z_convert:to_atom(Delegate)
	               end,
	PickledTag   = z_crypto:pickle({Tag,Delegate1,Id}, Context),
	GroupClasses = groups_to_classes(Groups1),

	Helper       =  case z_utils:is_true(Clone) orelse ConnectToSortable /= undefined of
            	        true  -> "'clone'";
            		    false -> "'original'"
            	    end,

	RevertText   =  case z_convert:to_binary(Revert) of
                		<<"true">>    -> <<"true">>;
                		<<"false">>   -> <<"false">>;
                		<<"valid">>   -> <<"'valid'">>;
                		<<"invalid">> -> <<"'invalid'">>
                    end,

    HandleText   = case z_convert:to_binary(Handle) of
                        <<>> -> "null";
                        _ -> [$',z_utils:js_escape(Handle),$']
                    end,

    AxisText   = case z_convert:to_binary(Axis) of
                        <<"x">> -> "'x'";
                        <<"y">> -> "'y'";
                        _ -> "null"
                    end,

    ConnectToSortableText = case ConnectToSortable of
                        undefined -> "false";
                        _ ->  [$', $#, z_utils:js_escape(ConnectToSortable),$']
                    end,

	% Write out the script to make this element draggable...
	Script = io_lib:format("z_draggable($('#~s'), { handle: ~s, helper: ~s, revert: ~s, opacity: ~s, scroll: true, cursor: 'hand', axis: ~s, connectToSortable: ~s }, '~s');", [
            		Id,
            		HandleText,
            		Helper,
            		RevertText,
            		Opacity,
            		AxisText,
            		ConnectToSortableText,
            		PickledTag
            	]),

    % Hook the actions to the element
    Actions = [
                {script,    [{script, Script}]},
                {add_class, [{class, GroupClasses}]}
            ],
    {ok, z_render:wire(Id, Actions, Context)}.


groups_to_classes(Groups) ->
	Groups1 = ["drag_group_" ++ z_convert:to_list(X) || X <- Groups],
	string:join(Groups1, " ").

