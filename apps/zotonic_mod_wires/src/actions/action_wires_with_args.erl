%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2025 Marc Worrell
%% @doc Create a callback where extra name/value are merged with the other actions
%% before they are performed.
%% @end

%% Copyright 2009-2025 Marc Worrell
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

-module(action_wires_with_args).

-export([
    render_action/4
]).

render_action(TriggerId, TargetId, Args, Context) ->
    Actions   = proplists:get_all_values(action, Args),
    ArgList   = proplists:get_all_values(arg, Args),
    ArgValue  = [ lookup_arg(Arg, Args) || Arg <- ArgList, Arg /= undefined ],
    Actions1  = z_render:action_with_args(Actions, ArgValue),
	z_render:render_actions(TriggerId, TargetId, Actions1, Context).

lookup_arg({ArgName, [{ArgValue,true}]}, Args) ->
    {ArgName, proplists:get_value(ArgValue, Args)}.
