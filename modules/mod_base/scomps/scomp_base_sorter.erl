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

-module(scomp_base_sorter).
-behaviour(gen_scomp).

-export([vary/2, render/3, event/2]).

-include("zotonic.hrl").

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
	SortItems = z_context:get_q("sort_items", Context),

    UnpickleF = fun(X) ->
                    {DragTag,DragDelegate,DragId} = z_utils:depickle(X, Context),
                    #dragdrop{tag=DragTag, delegate=DragDelegate, id=DragId}
                end,

    Sorted = lists:map(UnpickleF, string:tokens(SortItems, ",")),
    Drop   = #dragdrop{tag=SortTag, delegate=SortDelegate, id=TriggerId},

	try
	    SortDelegate:event(#sort{items=Sorted, drop=Drop}, Context)
    catch
        _M:E ->
            Error = io_lib:format("Error in routing sort to \"~s:event/2\"; error: \"~p\"", [SortDelegate,E]),
            z_render:wire({growl, [{text,Error}, {stay,1}, {type, error}]}, Context)
    end.


groups_to_classes(Groups) ->
	["drag_group_" ++ z_convert:to_list(X) || X <- Groups].
	
groups_to_connect_with(Groups) ->
	["'.drag_group_" ++ z_convert:to_list(X) ++ "'" || X <- Groups].
