%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% Date: 2010-09-07
%% @doc Toggle the value of a config setting, set it to the value of the checkbox.

%% Copyright 2010 Marc Worrell
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

-module(action_admin_config_config_toggle).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Module = proplists:get_value(module, Args),
    Key = proplists:get_value(key, Args),
    Postback = {config_toggle, Module, Key},
    {PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
    {PostbackMsgJS, Context}.


%% @doc Change a config key.
%% @spec event(Event, Context1) -> Context2
event(#postback{message={config_toggle, Module, Key}}, Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        true ->
            m_config:set_value(Module, Key, z_context:get_q("triggervalue", Context), Context),
            z_render:growl("Changed config setting.", Context);
        false ->
            z_render:growl_error("Only administrators can change configurations.", Context)
    end.
