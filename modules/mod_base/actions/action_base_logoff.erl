%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Logoff the current user

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

-module(action_base_logoff).
-include("zotonic.hrl").
-export([render_action/4, event/2]).

render_action(TriggerId, TargetId, _Args, Context) ->
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(logoff, undefined, TriggerId, TargetId, ?MODULE, Context),
	{[PostbackMsgJS], Context}.

event(#postback{message=logoff}, Context) ->
    Context1 = z_auth:logoff(Context),
    z_render:wire({reload, []}, Context1).
