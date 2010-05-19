%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell

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

-module(action_base_postback).
-include("zotonic.hrl").
-export([render_action/4]).

render_action(TriggerId, TargetId, Args, Context) -> 
	Postback  = proplists:get_value(postback, Args),
    Delegate  = z_convert:to_atom(proplists:get_value(delegate, Args)),
    Actions   = proplists:get_all_values(action, Args),

	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, undefined, TriggerId, TargetId, Delegate, Context),
	{ActionsJS, Context1} = z_render:render_actions(TriggerId, TargetId, Actions, Context),
	{[ PostbackMsgJS, ActionsJS ], Context1}.
