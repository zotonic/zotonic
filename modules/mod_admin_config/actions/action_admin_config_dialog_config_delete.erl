%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-08-07
%% @doc Open a dialog that asks confirmation to delete a configuration.

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

-module(action_admin_config_dialog_config_delete).
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
    OnSuccess = proplists:get_all_values(on_success, Args),
    Postback = {delete_config_dialog, Module, Key, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the delete confirmation template. The next step will ask to delete the config.
%% @spec event(Event, Context1) -> Context2
event(#postback{message={delete_config_dialog, Module, Key, OnSuccess}}, Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        true ->
            Vars = [ {on_success, OnSuccess}, {module, Module}, {key, Key} ],
            z_render:dialog(?__("Confirm delete", Context), "_action_dialog_config_delete.tpl", Vars, Context);
        false ->
            z_render:growl_error(?__("Only administrators can delete configurations.", Context), Context)
    end.
