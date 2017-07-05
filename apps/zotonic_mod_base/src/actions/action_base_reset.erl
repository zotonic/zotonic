%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Resets a form.  Either a targeted form or form closest to an id or the trigger.

%% Copyright 2011 Marc Worrell
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

-module(action_base_reset).
-include_lib("zotonic_core/include/zotonic.hrl").
-export([render_action/4]).

render_action(TriggerId, _TargetId, Args, Context) ->
    case proplists:get_value(closest, Args) of
        undefined ->
            case proplists:get_value(id, Args, proplists:get_value(target, Args)) of
                undefined ->
                    {["$(\"#", TriggerId, "\").closest(\"form\")[0].reset();"], Context};
                Id ->
                    {[ $$, $(, $", $#, z_utils:js_escape(z_convert:to_list(Id)), <<"\")[0].reset();">> ], Context}
            end;
        true ->
            {[ $$, $(, $", $#, z_utils:js_escape(z_convert:to_list(TriggerId)), <<"\").closest(\"form\")[0].reset();">> ], Context};
        CloseId ->
            {[ $$, $(, $", $#, z_utils:js_escape(z_convert:to_list(CloseId)), <<"\").closest(\"form\")[0].reset();">> ], Context}
    end.
