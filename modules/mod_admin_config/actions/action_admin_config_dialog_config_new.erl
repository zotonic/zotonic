%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-08-07
%% @doc Open a dialog with some fields to make a new configuration.

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

-module(action_admin_config_dialog_config_new).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    OnSuccess = proplists:get_all_values(on_success, Args),
    Postback = {config_new_dialog, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the new group form. The form will be posted back to this module.
%% @spec event(Event, Context1) -> Context2
event(#postback{message={config_new_dialog, OnSuccess}}, Context) ->
    Vars = [
        {delegate, atom_to_list(?MODULE)},
        {on_success, OnSuccess}
    ],
    z_render:dialog(?__("Add configuration key", Context), "_action_dialog_config_new.tpl", Vars, Context);


event(#submit{message={config_new, Args}}, Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        true ->
            Module = z_string:to_name(z_context:get_q_validated("module", Context)),
            Key = z_string:to_name(z_context:get_q_validated("key", Context)),
            Value = z_context:get_q("val", Context, ""),
            OnSuccess = proplists:get_all_values(on_success, Args),

            case m_config:get_id(Module, Key, Context) of
                undefined ->
                    m_config:set_value(Module, Key, Value, Context),
                    z_render:wire([{dialog_close, []} | OnSuccess], Context);
                _ ->
                    z_render:growl_error(?__("The config key already exists, please choose another key name.", Context), Context)
            end;
        false ->
            z_render:growl_error(?__("Only an administrator can add configuration keys.", Context), Context)
    end.

