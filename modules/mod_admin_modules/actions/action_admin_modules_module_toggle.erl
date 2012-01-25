%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-06-11
%% @doc Activate/dactivate a module

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

-module(action_admin_modules_module_toggle).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Module = proplists:get_value(module, Args),
    StatusId = proplists:get_value(status_id, Args),
    Postback = {module_toggle, Module, StatusId},
    {PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
    {PostbackMsgJS, Context}.


%% @doc Delete a media.  After the deletion the user is redirected, and/or some items on the page are faded out.
%% @spec event(Event, Context1) -> Context2
event(#postback{message={module_toggle, Module, StatusId}, trigger=TriggerId}, Context) ->
    case z_acl:is_allowed(use, mod_admin_modules, Context) of
        true ->
            Active = z_module_manager:active(Context),
            case lists:member(Module, Active) of
                true ->
                    z_module_manager:deactivate(Module, Context),
                    Context1 = z_render:update(TriggerId, "Activate", Context),
                    z_render:wire([
                            {set_class, [{target, StatusId}, {class, "icon_status icon_status_"}]},
                            {growl, [{text, ["Deactivated ", atom_to_list(Module), "."]}]}
                        ], Context1);
                false ->
                    z_module_manager:activate(Module, Context),
                    Context1 = z_render:update(TriggerId, "Deactivate", Context),
                    z_render:wire([
                            {set_class, [{target, StatusId}, {class, "icon_status icon_status_starting"}]},
                            {growl, [{text, ["Activated ", atom_to_list(Module), "."]}]}
                        ], Context1)
            end;
        false ->
            z_render:growl_error("You are not allowed to activate or deactivate modules.", Context)
    end.
