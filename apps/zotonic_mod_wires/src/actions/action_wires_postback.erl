%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2013 Marc Worrell

%% Copyright 2009-2013 Marc Worrell
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

-module(action_wires_postback).
-include_lib("zotonic_core/include/zotonic.hrl").
-export([render_action/4]).

render_action(TriggerId, TargetId, Args, Context) ->
	Postback0 = proplists:get_value(postback, Args),
    Delegate  = z_convert:to_atom(proplists:get_value(delegate, Args)),
    Actions   = proplists:get_all_values(action, Args),
    QArgs     = proplists:get_all_values(qarg, Args),

    %% check for injection of args into postback message
    Postback  = case {Postback0, proplists:get_value(inject_args, Args)} of
                    {{PbTag, PbArgs}, true} ->
                        {PbTag, lists:map(
                                 fun({K, V}) ->
                                         {K, proplists:get_value(K, Args, V)}
                                 end, PbArgs)};
                    {Pb, _} -> Pb
                end,

	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, undefined, TriggerId, TargetId,
                                                               Delegate, QArgs, Context),
	{ActionsJS, Context1} = z_render:render_actions(TriggerId, TargetId, Actions, Context),
	{[ PostbackMsgJS, ActionsJS ], Context1}.
