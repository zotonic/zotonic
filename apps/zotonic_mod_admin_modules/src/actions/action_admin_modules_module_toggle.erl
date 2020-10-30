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

-include_lib("zotonic_core/include/zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Module = proplists:get_value(module, Args),
    StatusId = proplists:get_value(status_id, Args),
    IsActivate = proplists:get_value(is_activate, Args, false),
    Postback = case proplists:get_value(is_deactivate, Args, false) of
        true ->
            {module_deactivate, Module, StatusId};
        false when IsActivate ->
            {module_activate, Module, StatusId};
        false when not IsActivate ->
            {module_toggle, Module, StatusId}
    end,
    {PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
    {PostbackMsgJS, Context}.


event(Event, Context) ->
    case z_acl:is_allowed(use, mod_admin_modules, Context) of
        true ->
            event_1(Event, Context);
        false ->
            z_render:growl_error(?__("You are not allowed to activate or deactivate modules.", Context), Context)
    end.

%% @doc Activate or Deactivate a module.
event_1(#postback{ message={module_activate, Module, _StatusId} }, Context) ->
    case z_module_manager:activate_precheck(Module, Context) of
        ok ->
            activate_module(Module, Context);
        {error, Missing} when is_map(Missing) ->
            z_render:dialog(
                ?__("Module dependencies", Context),
                "_dialog_admin_modules_activate_depending.tpl",
                #{
                    module => Module,
                    missing => Missing
                },
                Context);
        {error, {cyclic, Cycle}} ->
            z_render:dialog(
                ?__("Cyclic dependencies", Context),
                "_dialog_admin_modules_activate_cycle.tpl",
                #{
                    module => Module,
                    cycle => Cycle
                },
                Context)
    end;
event_1(#postback{ message={module_activate_confirm, [ {module, Module} ]} }, Context) ->
    activate_module(Module, Context);
event_1(#postback{ message={module_deactivate, Module, _StatusId} }, Context) ->
    case z_module_manager:deactivate_precheck(Module, Context) of
        ok ->
            deactivate_module(Module, Context);
        {error, Missing} when is_map(Missing) ->
            z_render:dialog(
                ?__("Module dependencies", Context),
                "_dialog_admin_modules_deactivate_depending.tpl",
                #{
                    module => Module,
                    missing => Missing
                },
                Context);
        {error, {cyclic, _Cycle}} ->
            deactivate_module(Module, Context)
    end;
event_1(#postback{ message={module_deactivate_confirm, [ {module, Module} ]} }, Context) ->
    deactivate_module(Module, Context);
event_1(#postback{ message={module_toggle, Module, _StatusId} }, Context) ->
    Active = z_module_manager:active(Context),
    case lists:member(Module, Active) of
        true ->
            deactivate_module(Module, Context);
       false ->
            activate_module(Module, Context)
    end.

-spec activate_module( atom(), z:context() ) -> z:context().
activate_module(Module, Context) ->
    case z_module_manager:activate(Module, Context) of
        ok ->
            z_render:wire({reload, []}, Context);
        {error, not_found} ->
            z_render:wire({alert, [ {text, ?__("Could not find the module.", Context)} ]}, Context)
    end.

-spec deactivate_module( atom(), z:context() ) -> z:context().
deactivate_module(Module, Context) ->
    ok = z_module_manager:deactivate(Module, Context),
    z_render:wire({reload, []}, Context).
