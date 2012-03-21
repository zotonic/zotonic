%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-28
%% @doc Open a dialog that asks confirmation to delete user credentials.

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

-module(action_admin_identity_dialog_delete_username).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Id = z_convert:to_integer(proplists:get_value(id, Args)),
    OnSuccess = proplists:get_all_values(on_success, Args),
    Postback = {dialog_delete_username, Id, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the delete confirmation template. The next step will ask to delete the username from the user id.
%% @spec event(Event, Context1) -> Context2
event(#postback{message={dialog_delete_username, Id, OnSuccess}}, Context) ->
    case z_acl:is_allowed(delete, Id, Context) of
        true ->
            case m_identity:get_username(Id, Context) of
                undefined ->
                    z_render:growl(?__("There is no username coupled to this person.", Context), Context);
                Username ->
                    Vars = [
                        {on_success, OnSuccess},
                        {id, Id},
                        {username, Username}
                    ],
                    z_render:dialog(?__("Confirm user deletion", Context), "_action_dialog_delete_username.tpl", Vars, Context)
            end;
        false ->
            z_render:growl_error(?__("Only an administrator can delete usernames.", Context), Context)
    end.
