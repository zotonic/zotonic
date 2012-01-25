%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-07-04
%% @doc Force a rescan of all modules, actions, templates etc. This is needed after a template, action or 
%% validation has been added.  It will also tell the dispatcher to reload all dispatch rules.

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

-module(action_admin_modules_module_rescan).
-author("Marc Worrell <marc@worrell.nl").
-include("zotonic.hrl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

render_action(TriggerId, TargetId, Args, Context) ->
    Actions = proplists:get_all_values(action, Args),
    Postback = {module_rescan, Actions},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Signal the module indexer to rescan all modules for actions, templates etc.
%% @spec event(Event, Context1) -> Context2
event(#postback{message={module_rescan, Actions}}, Context) ->
    z_notifier:notify(module_ready, Context),
    Context1 = z_render:growl("Module rescan is in progress.", Context),
    z_render:wire(Actions, Context1).
