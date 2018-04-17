%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell
%% @doc Open an overlay with content from a template.

%% Copyright 2016 Marc Worrell
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

-module(action_wires_overlay_open).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    {PostbackMsgJS, _PickledPostback} = z_render:make_postback({overlay, Args}, click, TriggerId, TargetId, ?MODULE, Context),
    {PostbackMsgJS, Context}.

%% @spec event(Event, Context1) -> Context2
event(#postback{message={overlay, Args}}, Context) ->
    {template, Template} = proplists:lookup(template, Args),
    z_render:overlay(Template, Args, Context).

