%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl.nl>
%% @copyright 2015 Maas-Maarten Zeeman

%% Copyright 2015 Maas-Maarten Zeeman
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


-module(action_mqtt_publish).

-export([render_action/4]).

render_action(_TriggerId, _TargetId, Args, Context) ->
    {topic, Topic} = proplists:lookup(topic, Args),
    Topic1 = z_mqtt:flatten_topic( z_mqtt:map_topic( Topic, Context ) ),
    TopicJS = z_utils:js_escape( Topic1 ),

    Message = case proplists:get_value(js_msg, Args) of
        undefined ->
            z_utils:js_object(proplists:delete(topic, Args));
        Msg ->
            case Msg of
                {trust, JsValue} -> JsValue;
                _ -> js_value(z_utils:js_escape(Msg))
            end
    end,

    Script = iolist_to_binary([<<"cotonic.broker.publish('">>, TopicJS, <<"',">>, Message, <<");">>]),
    {Script, Context}.

%%
%% Helpers
%%

js_value(Str) when is_binary(Str) orelse is_list(Str) -> [$", Str, $"];
js_value(Int) when is_integer(Int) -> z_convert:to_list(Int);
js_value(true) -> <<"true">>;
js_value(false) -> <<"false">>;
js_value(undefined) -> <<"undefined">>.

