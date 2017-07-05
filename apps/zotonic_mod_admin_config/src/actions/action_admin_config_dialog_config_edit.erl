%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-08-07
%% @doc Open a dialog to change the module/key/value of a config entry.

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

-module(action_admin_config_dialog_config_edit).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Module = proplists:get_value(module, Args),
    Key = proplists:get_value(key, Args),
    Value = proplists:get_value(value, Args),
    OnSuccess = proplists:get_all_values(on_success, Args),
    Postback = {config_edit_dialog, Module, Key, Value, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the new group form. The form will be posted back to this module.
%% @spec event(Event, Context1) -> Context2
event(#postback{message={config_edit_dialog, Module, Key, Value, OnSuccess}}, Context) ->
    Vars = [
        {module, Module},
        {key, Key},
        {value, Value},
        {on_success, OnSuccess}
    ],
    z_render:dialog([?__("Edit", Context), " ", Module, "." , Key], "_action_dialog_config_edit.tpl", Vars, Context);


%% @doc Add a member to a group.  The roles are in the request (they come from a form)
%% @spec event(Event, Context1) -> Context2
event(#submit{message={config_edit, Args}}, Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        true ->
            CurrentModule = proplists:get_value(module, Args),
            CurrentKey = proplists:get_value(key, Args),
            Module = z_context:get_q(<<"module">>, Context, <<>>),
            Key = z_context:get_q(<<"key">>, Context, <<>>),
            Value = z_context:get_q(<<"val">>, Context, <<>>),
            OnSuccess = proplists:get_all_values(on_success, Args),
            update_entry(CurrentModule, CurrentKey, Module, Key, Value, Context),
            z_render:wire([{dialog_close, []} | OnSuccess], Context);
        false ->
            z_render:growl_error(?__("Only administrators can change the configuration.", Context), Context)
    end.

%% Update config entry. First delete old entry if module or key name is not equal to current name.
update_entry(Module, Key, Module, Key, Value, Context) ->
    m_config:set_value(Module, Key, Value, Context);
update_entry(CurrentModule, CurrentKey, Module, Key, Value, Context) ->
    m_config:delete(CurrentModule, CurrentKey, Context),
    m_config:set_value(Module, Key, Value, Context).
