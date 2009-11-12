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

-module(scomp_base_draggable).
-behaviour(gen_scomp).

-export([init/1, varies/2, terminate/2, render/4]).

-include("zotonic.hrl").

init(_Args) -> {ok, []}.
varies(_Params, _Context) -> undefined.
terminate(_State, _Context) -> ok.

%% -record(draggable, {?ELEMENT_BASE(element_draggable), tag, body=[], group, handle, clone=true, revert=true}).

render(Params, _Vars, Context, _State) ->
    Id      = proplists:get_value(id, Params),
    Tag     = proplists:get_value(tag, Params),
    Clone   = proplists:get_value(clone, Params, false),
    Revert  = proplists:get_value(revert, Params, "invalid"),
    Axis    = proplists:get_value(axis, Params),
    Handle  = proplists:get_value(handle, Params),
    Groups  = proplists:get_all_values(group, Params),
    Opacity = proplists:get_value(opacity, Params, "0.8"),
    Delegate= proplists:get_value(delegate, Params),
    
    Groups1 = case Groups of
                [] -> ["dragdrop"];
                _ -> Groups
               end,
               
	% Get properties...
	Delegate1    = case Delegate of
	                undefined -> z_context:get_resource_module(Context);
	                _ -> Delegate
	               end,
	PickledTag   = z_utils:pickle({Tag,Delegate1,Id}, Context),
	GroupClasses = groups_to_classes(Groups1),

	Helper       =  case z_utils:is_true(Clone) of
            	        true  -> "'clone'";
            		    false -> "'original'"
            	    end,
	
	RevertText   =  case z_convert:to_list(Revert) of
                		"true"    -> "true";
                		"false"   -> "false";
                		"valid"   -> "'valid'";
                		"invalid" -> "'invalid'"
                    end,
    
    HandleText   = case Handle of
                        undefined -> "null";
                        []        -> "null";
                        <<>>      -> "null";
                        _ -> [$',z_utils:js_escape(Handle),$']
                    end,

    AxisText   = case Axis of
                        "x" -> "'x'";
                        <<"x">> -> "'x'";
                        "y" -> "'y'";
                        <<"y">> -> "'y'";
                        _ -> "null"
                    end,
                    
	% Write out the script to make this element draggable...
	Script = io_lib:format("z_draggable($('#~s'), { handle: ~s, helper: ~s, revert: ~s, opacity: ~s, scroll: true, cursor: 'hand', axis: ~s }, '~s');", [
            		Id, 
            		HandleText, 
            		Helper, 
            		RevertText,
            		Opacity,
            		AxisText,
            		PickledTag
            	]),

    % Hook the actions to the element
    Actions = [
                {script,    [{script, Script}]},
                {add_class, [{class, GroupClasses}]}
            ],
    {ok, z_render:wire(Id, Actions, Context)}.

	
groups_to_classes([]) -> "";
groups_to_classes(undefined) -> "";
groups_to_classes(Groups) ->
	Groups1 = ["drag_group_" ++ z_convert:to_list(X) || X <- Groups],
	string:join(Groups1, " ").
	
