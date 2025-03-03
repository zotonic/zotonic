%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2025 Marc Worrell
%% @doc Set the value of an input element
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

-module(action_wires_set_value).

-export([render_action/4]).

render_action(_TriggerId, TargetId, Args, Context) ->
    CssSelector = z_render:css_selector(proplists:get_value(id, Args, TargetId), Args),
    Attr = proplists:get_value(value_arg, Args, value),
    Value = proplists:get_value(Attr, Args, <<>>),
    Script = iolist_to_binary([
         $$, $(, $', CssSelector, $', $),
         <<".val(\"">>, z_utils:js_escape(Value), $", $)
       ]),
   Script1 = case proplists:get_value(trigger_event, Args) of
      undefined -> Script;
      false -> Script;
      true ->
         [
            Script,
            <<".trigger(\"changed\")">>
         ];
      Event ->
         [
            Script,
            <<".trigger(\"">>, z_utils:js_escape(Event), $", $)
         ]
   end,
   {[Script1, $;], Context}.
