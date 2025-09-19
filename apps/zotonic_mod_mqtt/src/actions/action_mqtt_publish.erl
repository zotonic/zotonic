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
-moduledoc("
Publishes a message on the topic tree of the current page.

Example:


```erlang
{% button text=\"Hello\" action={publish topic=\"some/topic\" js_msg=\"Hello World!\"} %}
```

When clicked, the message set to the message `\"Hello World!\"` is published.

Another example, now publishing a more complex message:


```erlang
{% button text=\"hello\" action={publish topic=\"some/topic\" foo=\"bar\" spam=\"eggs\"} %}
```

When clicked, the message set to the message `{foo: \"bar\", spam: \"eggs\"}` is published.

Prefix the topic with `bridge/origin/` to relay it to the topic tree on the server.

| Argument | Description                                                     | Example                |
| -------- | --------------------------------------------------------------- | ---------------------- |
| topic    | The topic of the message.                                       | topic=”test”           |
| js\\\\_msg | Literal javascript message to publish. Will be escaped.         | js\\\\_msg=”Hello World” |
| &lt;key> | A literal javascript value, will be escaped and added to object | spam=”eggs”            |
").

-export([render_action/4]).

render_action(_TriggerId, _TargetId, Args, Context) ->
    {topic, Topic} = proplists:lookup(topic, Args),
    {ok, Topic1} = z_mqtt:map_topic( Topic, Context ),
    Topic2 = z_mqtt:flatten_topic( Topic1 ),
    TopicJS = z_utils:js_escape( Topic2 ),

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

