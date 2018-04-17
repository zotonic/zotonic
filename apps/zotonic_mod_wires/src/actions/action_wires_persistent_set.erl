%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell
%% @doc Set a persistent key/value pair

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

-module(action_wires_persistent_set).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    render_action/4,
    event/2
]).

render_action(TriggerId, TargetId, Args, Context) ->
    case proplists:get_value(key, Args) of
        undefined ->
            {<<>>, Context};
        Key ->
            Value = proplists:get_value(value, Args),
            {PostbackMsgJS, _PickledPostback} = z_render:make_postback({set, Key, Value}, click, TriggerId, TargetId, ?MODULE, Context),
            {PostbackMsgJS, Context}
    end.

event(#postback{message={set, Key, Value}}, Context) ->
    z_context:set_persistent(z_convert:to_atom(Key), Value, Context).
