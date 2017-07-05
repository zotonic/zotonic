%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015 Marc Worrell
%% @doc Show progress on a mask element.

%% Copyright 2015 Marc Worrell
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

-module(action_base_mask_progress).
-include_lib("zotonic_core/include/zotonic.hrl").
-export([render_action/4]).

render_action(_TriggerId, TargetId, Args, Context) ->
    Selector = case proplists:get_value(body, Args) of
        true ->
            "body";
        _ ->
            case z_render:css_selector(TargetId, Args) of
                [] -> "body";
                S -> S
            end
    end,
    Percent = z_convert:to_integer(proplists:get_value(percent, Args, 0)),
    Script = [ <<"try { ">>, z_render:render_css_selector(Selector) ,<<".maskProgress(">>,
                    z_convert:to_list(Percent),
                <<"); } catch (e) {};">>],
    {Script, Context}.
