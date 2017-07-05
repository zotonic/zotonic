%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2010 Maas-Maarten Zeeman
%% Date: 2010-12-03
%% @doc Emit a signal to connected pages.

%% Copyright 2010 Maas-Maarten Zeeman
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

-module(action_signal_emit).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-include_lib("zotonic_core/include/zotonic.hrl").
-export([render_action/4, event/2]).

render_action(TriggerId, TargetId, Args, Context) ->
    Signal = proplists:get_value(signal, Args),
    Postback = {emit, [{signal, Signal}]},
    {Script, _Context} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
    {Script, Context}.

event(#postback{message={emit, [{signal, Signal}]}}, Context) ->
    mod_signal:emit(Signal, Context),
    Context.
