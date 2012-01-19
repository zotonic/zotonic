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

-module(scomp_base_droppable).
-behaviour(gen_scomp).

-export([vary/2, render/3, event/2]).

-include("zotonic.hrl").

vary(_Params, _Context) -> nocache.

%% -record(droppable, {?ELEMENT_BASE(element_droppable), tag, body=[], accept_groups=all, active_class=active, hover_class=hover}).

render(Params, _Vars, Context) ->
    Id           = proplists:get_value(id, Params),
    Tag          = proplists:get_value(tag, Params),
    ActiveClass  = proplists:get_value(active_class, Params, "active"),
    HoverClass   = proplists:get_value(active_class, Params, "hover"),
    AcceptGroups = proplists:get_all_values(accept, Params),
    Delegate     = proplists:get_value(delegate, Params),
    
    case Id of
        undefined ->
            {error, "droppable scomp, please give the id of the droppable"};
        _ ->
            Delegate1 = case Delegate of
                            undefined -> z_context:get_resource_module(Context);
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
	DragItem = z_context:get_q("drag_item", Context),
	{DragTag,DragDelegate,DragId} = z_utils:depickle(DragItem, Context),

    Drop = #dragdrop{tag=DropTag, delegate=DropDelegate, id=TriggerId},
    Drag = #dragdrop{tag=DragTag, delegate=DragDelegate, id=DragId},

	try
	    Context1 = DropDelegate:event(#drop{drag=Drag, drop=Drop}, z_context:set_resource_module(DropDelegate, Context)),
	    
	    % also notify the dragged element that it has been dragged
	    try
	        DragDelegate:event(#drag{drag=Drag, drop=Drop}, z_context:set_resource_module(DragDelegate, Context1))
	    catch
	        _M1:_E1 -> Context1
	    end

    catch
        _M2:E ->
            ?ERROR("Error in drop routing: ~p~n~p", [E, erlang:get_stacktrace()]),
            Error = io_lib:format("Error in routing drop to module \"~s\"; error: \"~p\"", [DropDelegate,E]),
            z_render:wire({growl, [{text,Error}, {stay,1}]}, Context)
    end.


groups_to_accept([]) -> "*";
groups_to_accept(["all"]) -> "*";
groups_to_accept(["none"]) -> "";
groups_to_accept(Groups) ->
	Groups1 = [".drag_group_" ++ z_convert:to_list(X) || X <- Groups],
	string:join(Groups1, ", ").
