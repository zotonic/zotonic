%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% Date: 2010-11-22
%% @doc Add WYSIWYG editor controls to all textarea.z_editor in the target

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

-module(action_base_editor_remove).
-include_lib("zotonic_core/include/zotonic.hrl").
-export([
    render_action/4
]).

render_action(_TriggerId, TargetId, Args, Context) ->
    CssSelector = z_render:css_selector(proplists:get_value(id, Args, TargetId), Args),
    {[ <<"z_editor_remove(\"">>, z_utils:js_escape(CssSelector), $", $), $;], Context}.
