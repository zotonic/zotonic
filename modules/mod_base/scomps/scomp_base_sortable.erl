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

-module(scomp_base_sortable).
-behaviour(gen_scomp).

-export([init/1, varies/2, terminate/2, render/4]).

-include("zotonic.hrl").

init(_Args) -> {ok, []}.
varies(_Params, _Context) -> undefined.
terminate(_State, _Context) -> ok.

% -record(sortitem, {?ELEMENT_BASE(element_sortitem), tag, body=[] }).

render(Params, _Vars, Context, _State) ->
    Id       = proplists:get_value(id, Params),
    Tag      = proplists:get_value(tag, Params),
    Delegate = proplists:get_value(delegate, Params),
    Class    = proplists:get_all_values(class, Params),
    
	% Get properties...
	Delegate1    = case Delegate of
	                undefined -> z_context:get_resource_module(Context);
	                _ -> Delegate
	               end,

   case Id of
       undefined ->
           {error, "sortable scomp, please give the id of the sortable element"};
       _ ->
        	PickledTag  = z_utils:pickle({Tag,Delegate1,Id}, Context),
        	Script      = io_lib:format("z_sortable($('#~s'), '~s');", [Id, PickledTag]),

            Actions = [
                        {script,    [{script, Script}]},
                        {add_class, [{class, ["sortable "|Class]}]}
                    ],

        	{ok, z_render:wire(Id, Actions, Context)}
    end.
