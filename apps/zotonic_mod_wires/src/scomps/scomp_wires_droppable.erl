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

-module(scomp_wires_droppable).
-moduledoc("
Mark an element as valid drag destination.

The droppable tag is used in conjunction with the [\\{% draggable
%\\}](/id/doc_template_scomp_scomp_draggable#scomp-draggable) tag to implement drag & drop. Elements that are marked as
droppable can receive drops of draggable elements. Drag & drop generates dragdrop events that are sent to the
[controller](/id/doc_glossary#term-controller) or the [delegate](/id/doc_glossary#term-delegate).

For example:


```erlang
<div id=\"dropzone\">Drop your stuff here</div>
{% droppable id=\"dropzone\" tag=\"drop-tag\" %}
```

Now draggable elements can de dropped onto the div with id “dropzone”. When a draggable is dropped then `event/2` of
the controller or delegate Erlang module is called signaling the drop (and also the drag to the module receiving the
draggable events):


```erlang
event({drop, Drag, Drop}, Context).
```

Where both Drag and Drop are `#dragdrop` records:


```erlang
-record(dragdrop, {tag, delegate, id}).
```

The droppable tag accepts the following arguments:

| Argument | Description                                                                      | Example                  |
| -------- | -------------------------------------------------------------------------------- | ------------------------ |
| id       | The id of the element that will accept drops of draggables.                      | id=”dropzone”            |
| tag      | The tag of the droppable that is sent as part of the drag and drop events. This can be any value, including a tuple. | tag=\\\\{subject id=123\\\\} |
| active   | The droppable will have this CSS class added when there is an acceptable draggable being dragged. | active=”draghere”        |
| hover    | The droppable will have this CSS class added when there is an acceptable draggable hovering over it. | active=”dropnow”         |
| accept   | The group the droppable accepts. See the group argument of the draggable. A droppable can accept multiple groups, just repeat the accept argument. | accept=”edges”           |
| delegate | The Erlang module that will receive the drop event after a successful drop.      |                          |

See also

the [draggable](/id/doc_template_scomp_scomp_draggable#scomp-draggable) tag.
").
-behaviour(zotonic_scomp).

-export([vary/2, render/3, event/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

vary(_Params, _Context) -> nocache.

%% -record(droppable, {?ELEMENT_BASE(element_droppable), tag, body=[], accept_groups=all, active_class=active, hover_class=hover}).

render(Params, _Vars, Context) ->
    Id           = proplists:get_value(id, Params),
    Tag          = proplists:get_value(tag, Params),
    ActiveClass  = proplists:get_value(active_class, Params, <<"active">>),
    HoverClass   = proplists:get_value(active_class, Params, <<"hover">>),
    AcceptGroups = proplists:get_all_values(accept, Params),
    Delegate     = proplists:get_value(delegate, Params),

    case Id of
        undefined ->
            {error, "droppable scomp, please give the id of the droppable"};
        _ ->
            Delegate1 = case Delegate of
                            undefined -> z_context:get_controller_module(Context);
                            _ -> z_convert:to_atom(Delegate)
                        end,

        	AcceptGroups1       = groups_to_accept(AcceptGroups),
        	PickledPostbackInfo = z_render:make_postback_info({Tag,Delegate1}, sort, Id, Id, ?MODULE, Context),

        	Script = io_lib:format( "z_droppable($('#~s'), { activeClass: '~s', hoverClass: '~s', accept: '~s' }, '~s');",
        	                        [Id, ActiveClass, HoverClass, AcceptGroups1, PickledPostbackInfo]),

            % Hook the actions to the element
            Actions = {script, [{script, Script}]},
            {ok, z_render:wire(Id, Actions, Context)}
    end.


%% @doc Drops will be delegated to this event handler, which will call the postback resource.
event(#postback{message={DropTag,DropDelegate}, trigger=TriggerId}, Context) ->
	DragItem = z_context:get_q(<<"drag_item">>, Context),
	{DragTag,DragDelegate,DragId} = z_crypto:depickle(DragItem, Context),

    Drop = #dragdrop{tag=DropTag, delegate=DropDelegate, id=TriggerId},
    Drag = #dragdrop{tag=DragTag, delegate=DragDelegate, id=DragId},

	try
	    Context1 = DropDelegate:event(#drop{drag=Drag, drop=Drop}, z_context:set_controller_module(DropDelegate, Context)),

	    % also notify the dragged element that it has been dragged
	    try
	        DragDelegate:event(#drag{drag=Drag, drop=Drop}, z_context:set_controller_module(DragDelegate, Context1))
	    catch
	        _M1:_E1 -> Context1
	    end

    catch
        M2:E:Stack ->
            ?LOG_ERROR(#{
                text => <<"Error in drop routing">>,
                in => zotonic_mod_wires,
                result => M2,
                reason => E,
                drop_delegate => DropDelegate,
                drag_delegate => DragDelegate,
                stack => Stack
            }),
            Error = io_lib:format("Error in routing drop to module \"~s\"; error: \"~p\"", [DropDelegate,E]),
            z_render:wire({growl, [{text,Error}, {stay,1}]}, Context)
    end.


groups_to_accept([]) -> <<"*">>;
groups_to_accept([<<"all">>]) -> <<"*">>;
groups_to_accept([<<"none">>]) -> <<>>;
groups_to_accept(Groups) ->
	Groups1 = [".drag_group_" ++ z_convert:to_list(X) || X <- Groups],
	string:join(Groups1, ", ").
