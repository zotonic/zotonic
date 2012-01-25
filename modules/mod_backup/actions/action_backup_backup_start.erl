%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% Date: 2010-02-15

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

-module(action_backup_backup_start).
-include("zotonic.hrl").
-export([
    render_action/4, 
    event/2
]).

render_action(TriggerId, TargetId, _Args, Context) ->
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(backup_start, undefined, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Download a backup.
%% @spec event(Event, Context1) -> Context2
event(#postback{message=backup_start}, Context) ->
    case z_acl:is_allowed(use, mod_backup, Context) of
        true ->
            case mod_backup:start_backup(Context) of
                ok ->
        	        z_render:growl("Started the backup. You can keep this page open or continue working.", Context);
        	    {error, in_progress} ->
        	        z_render:growl_error("Could not start the backup because a backup is already in progress.", Context)
        	end;
        false ->
            z_render:growl_error("Only administrators can start a backup.", Context)
    end.

