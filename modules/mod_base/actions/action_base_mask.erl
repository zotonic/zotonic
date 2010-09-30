%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc Mask an element or the whole page with a busy indicator.

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

-module(action_base_mask).
-include("zotonic.hrl").
-export([render_action/4]).

render_action(_TriggerId, TargetId, Args, Context) -> 
    Selector = case proplists:get_value(body, Args) of
        true -> "body";
        _ -> 
            case z_render:css_selector(TargetId, Args) of
                [] -> "body";
                S -> S
            end
    end,
    Message = proplists:get_value(message, Args, ""),
    Delay = proplists:get_value(delay, Args, 0),
    Script = [ <<"try { ">>, z_render:render_css_selector(Selector) ,<<".mask('">>, 
                z_utils:js_escape(Message), $',$,,
                z_convert:to_list(Delay),
                <<"); } catch (e) {};">>],
    {Script, Context}.
