%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Based on code (c) 2008-2009 Rusty Klophaus
%%
%% @doc Mark an element as a sorter.  A sorter is a container for sortables.

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

-module(scomp_wires_sorter).
-moduledoc("
A sorter is a container for sortables.

A sorter contains sortables and handles the events when the order of the sortables is changed. Sortables in a sorter can
be reordered by drag & drop.

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

When the order of the sortables is changed, an event is send to the sorter’s page
[controller](/id/doc_glossary#term-controller) or [delegate](/id/doc_glossary#term-delegate). The event contains the
ordered list of sortable tags.

The event handler function is called like:


```erlang
event({sort, Sortables, Sorter}, Context).
```

Where “Sortables” is the list of sortables and “Sorter” is the sorter. Both are `#dragdrop` records:


```erlang
-record(dragdrop, {tag, delegate, id}).
```

Where “tag” is the tag of the sortable or sorter, “delegate” is the module that handles the event and “id”
is the HTML id of the sortable or sorter.

Note

If the tag is a string then the `#dragdrop` tag will be an atom.

The sorter can have the following arguments:

| Argument        | Description                                                                      | Example                   |
| --------------- | -------------------------------------------------------------------------------- | ------------------------- |
| id              | The HTML id of the sortable element.                                             | id=”mysorter”             |
| tag             | Tag that identifies the sortable to the event handler. Can be any value. A string will be converted to an atom. | tag=”my\\\\_atom\\\\_value”   |
| delegate        | The delegate of the sorter. The sort event will be send to the delegate module. Defaults to the resource that handled the page request. | delegate=”mymodule”       |
| class           | A CSS class that will be added to the sorter. The class “sorter” will always be added. | class=”bunny-sorter”      |
| handle          | jQuery selector for the handles of the sortables. When not defined then the whole sortable can be clicked on for dragging. | handle=”.sortable-handle” |
| connect\\\\_group | Name of the group this sorter connects with. Sortables from this sorter can then be dragged to sorters with that group name. This argument can be repeated to connect with multiple groups. Special values are “all” and “none” to either connect to all other sorters or to no other sorter. | connect\\\\_group=”bunnies” |
| group           | The group of this sorter. Used in connection with the “connect\\\\_group” argument. Sortables can be dragged between sorters of the same group. | group=”cows”              |
| axis            | If defined the items can only be dragged horizontally or vertically. Possible values are “x” and “y”. | axis=”y”                  |
| containment     | Constrains dragging of the sortables to within the bounds of the specified element. Possible values are “parent”, “document”, “window”, or a jQuery selector. | containment=”parent”      |
| opacity         | Opacity a sortable is set to when being dragged. Defaults to “1.0”.              | opacity=”0.8”             |
| placeholder     | Class that gets applied to the otherwise white space that will show between sortables as the new place of the sortable. | class=”drophere”          |

See also

the [sortable](/id/doc_template_scomp_scomp_sortable#scomp-sortable) tag.
").
-behaviour(zotonic_scomp).

-export([vary/2, render/3, event/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

vary(_Params, _Context) -> nocache.

% -record(sortblock, {?ELEMENT_BASE(element_sortblock), tag, items=[], group, connect_with_groups=none, handle }).

render(Params, _Vars, Context) ->
    Id           = proplists:get_value(id, Params),
    Tag          = proplists:get_value(tag, Params),
    Class        = proplists:get_value(class, Params, []),
    Handle       = proplists:get_value(handle, Params),
    ConnectGroups= proplists:get_all_values(connect_group, Params),
    Groups       = proplists:get_all_values(group, Params),
    Delegate     = proplists:get_value(delegate, Params),
    Axis     	 = proplists:get_value(axis, Params),
	Containment	 = proplists:get_value(containment, Params),
	Opacity	 	 = proplists:get_value(opacity, Params),
	Placeholder	 = proplists:get_value(placeholder, Params),


    case Id of
        undefined ->
            {error, "sorter scomp, please give the id of the sorter container"};
        _ ->

			Delegate1	 = case Delegate of
							undefined -> z_context:get_controller_module(Context);
							_ -> z_convert:to_atom(Delegate)
						   end,

			PickledPostbackInfo = z_render:make_postback_info({Tag,Delegate1}, sort, Id, Id, ?MODULE, Context),
			Handle1			  = case Handle of
									undefined -> "null";
									_		  -> [$', Handle, $']
								end,

			ConnectWithGroups = case ConnectGroups of
									[All] when All == "all" orelse All == <<"all">> -> ["*"];
									[None] when None == "none" orelse None == <<"none">> -> [];
									_ -> groups_to_connect_with(lists:usort(ConnectGroups ++ Groups))
								end,
			GroupClasses	  = groups_to_classes(Groups),

			Axis1				= case Axis of
									undefined -> "null";
									_		  -> [$', Axis, $']
								end,

			Containment1	   = case Containment of
									undefined -> "null";
									_		  -> [$', Containment, $']
								end,

			Opacity1	   = case Opacity of
									undefined -> "null";
									_		  -> [$', Opacity, $']
								end,

			Placeholder1   = case Placeholder of
									undefined -> "null";
									_		  -> [$', Placeholder, $']
								end,


        	% Emit the javascript...
        	Script = io_lib:format( "z_sorter($('#~s'), { handle: ~s, connectWith: [~s], axis: ~s, containment: ~s, opacity: ~s, placeholder: ~s }, '~s');",
        	                        [Id, Handle1, string:join(ConnectWithGroups, ", "), Axis1, Containment1, Opacity1, Placeholder1, PickledPostbackInfo]),

            Actions = [
                        {script,    [{script, Script}]},
                        {add_class, [{class, "sorter " ++ string:join(GroupClasses, " ") ++ " " ++ z_convert:to_list(Class)}]}
                    ],

    	    {ok, z_render:wire(Id, Actions, Context)}
    end.


%% @doc Handle the drop of a sortable in a sorter
event(#postback{message={SortTag,SortDelegate}, trigger=TriggerId}, Context) ->
	SortItems = z_context:get_q(<<"sort_items">>, Context),
    UnpickleF = fun(X) ->
                    {DragTag,DragDelegate,DragId} = z_crypto:depickle(X, Context),
                    #dragdrop{tag=DragTag, delegate=DragDelegate, id=DragId}
                end,

    Sorted = lists:map(UnpickleF, binary:split(SortItems, <<",">>, [global, trim_all])),
    Drop   = #dragdrop{tag=SortTag, delegate=SortDelegate, id=TriggerId},

	try
	    SortDelegate:event(#sort{items=Sorted, drop=Drop}, Context)
    catch
        _M:E:Stacktrace ->
        	?LOG_WARNING(
        		"Error in routing sort to \"~s:event/2\"; error: \"~p\"",
        		[SortDelegate, E],
        		#{ stack => Stacktrace }),
            Error = io_lib:format("Error in routing sort to \"~s:event/2\"; error: \"~p\"", [SortDelegate,E]),
            z_render:wire({growl, [{text,Error}, {stay,1}, {type, error}]}, Context)
    end.


groups_to_classes(Groups) ->
	["drag_group_" ++ z_convert:to_list(X) || X <- Groups].

groups_to_connect_with(Groups) ->
	["'.drag_group_" ++ z_convert:to_list(X) ++ "'" || X <- Groups].
