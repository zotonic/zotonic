%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Create a callback where extra name/value are merged with the other actions before they are performed.

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

-module(action_base_with_args).
-include("zotonic.hrl").
-export([
    render_action/4
]).

render_action(TriggerId, TargetId, Args, Context) -> 
    Actions   = proplists:get_all_values(action, Args),
    ArgList   = proplists:get_all_values(arg, Args),
    ArgValue  = [ lookup_arg(Arg, Args) || Arg <- ArgList, Arg /= undefined ],
    Actions1  = [ append_args(Action, ArgValue) || Action <- lists:flatten(Actions), Action /= undefined ],
	z_render:render_actions(TriggerId, TargetId, Actions1, Context).    

lookup_arg({ArgName, [{ArgValue,true}]}, Args) ->
    {ArgName, proplists:get_value(ArgValue, Args)}.
    
append_args({Action, ActionArgs}, Args) ->
    {Action, ActionArgs ++ Args}.
