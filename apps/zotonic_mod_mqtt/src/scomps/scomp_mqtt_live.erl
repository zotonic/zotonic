%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2026 Marc Worrell
%% @doc Simple live updating events
%% @end

%% Copyright 2014-2026 Marc Worrell
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

-module(scomp_mqtt_live).
-moduledoc(#{
    zotonic_keywords => ["reference", "frontend_developer", "scomp", "messaging_and_pubsub", "mqtt", "render"]
}).
-moduledoc("
Live-updating templates driven by MQTT topics.

::: note
This scomp is provided by `module#mod_mqtt`, which must be enabled.
:::

This tag renders templates that are automatically re-rendered after a publication to an MQTT topic.



Example
-------

An example of a template showing the newest content of a resource:


```erlang
{% live template=\"_detail.tpl\" topic=id id=id %}
```

This renders the template `_detail.tpl`. If the resource with id `id` is updated then the template will be replaced with
a freshly rendered template.

The tag can subscribe to multiple topics at once.

Add the argument `catinclude` to do a `tag#catinclude` instead of a normal
`tag#include`. For a catinclude the argument `id` must be present:


```erlang
{% live template=\"_detail.tpl\" topic=id catinclude id=id %}
```



Arguments
---------

Use either `template` to render a template or, without `template`, supply
`postback`, `delegate`, and `target` to call an event handler.

| Argument | Default | Description |
| -------- | ------- | ----------- |
| `topic` | None | MQTT topic to subscribe to. Repeat this argument for multiple topics. Accepts a topic string, a list of topic segments, a resource id, or an edge-topic tuple; see Live topics below. |
| `template` | None | Template to render on notifications. When present, selects template mode. |
| `catinclude` | `false` | Select a category-specific version of `template`, using the resource passed as `id`. |
| `id` | None | Template variable identifying the resource. Required when using `catinclude`; does not automatically subscribe to that resource's topic. |
| `target` | Generated element id in template mode | DOM element id to update, without `#`. When supplied, no wrapper is generated: the caller must provide the target element. Required in postback mode. |
| `element` | `div` | HTML tag for the generated wrapper in template mode. Ignored when `target` is supplied. An empty string suppresses the wrapper; the template must then provide an element with the generated `target` id. |
| `method` | `update` in template mode | How to insert or update rendered HTML; see Update methods below. In postback mode the delegate handles rendering. |
| `postback` | None | Message delivered in `#postback{message = Message, target = Target}` to the delegate. Required when `template` is omitted. |
| `delegate` | None | Module implementing `event/2` for the postback. Required when `template` is omitted. |
| `throttle` | `0` | Non-negative integer interval in milliseconds between refreshes during a burst. `0` disables throttling; see Throttling updates below. |

In template mode, arguments other than `topic`, `template`, `catinclude`,
`element`, `method`, and `throttle` are passed as template variables. Pass any
required variables explicitly: the surrounding template's variables are not
automatically inherited. The scomp also supplies `target` and `is_live_update`.
The latter is `false` for the initial render and `true` for notification-driven
renders.

Update methods
--------------

| Method | Initial render | On notification |
| ------ | -------------- | --------------- |
| `update` | Render the template at the tag's location. | Replace the target's contents. |
| `updateonly` | No template render. | Replace the target's contents. |
| `top` | No template render. | Prepend rendered HTML inside the target. |
| `bottom` | No template render. | Append rendered HTML inside the target. |
| `before` | No template render. | Insert rendered HTML before the target. |
| `after` | No template render. | Insert rendered HTML after the target. |
| `patch` | No template render. | Update the target through Cotonic's UI model. |

Without an explicit `target`, a wrapper is generated even for methods that do
not render the template initially. With an explicit `target`, place the live
tag inside that element if using `update` and its initial render is wanted there.

For example, append a rendered item to an existing list on each notification:

```django
<ul id=\"{{ #items }}\"></ul>
{% live topic=\"bridge/origin/public/items\"
        template=\"_item.tpl\" target=#items method=\"bottom\"
%}
```

In notification-driven template renders, a map or proplist MQTT payload is
available as query arguments (`q`). Other payload values are available as
`q.payload`. These values are untrusted input: escape them when outputting HTML.

To handle notifications in Erlang instead of rendering a template:

```django
<div id=\"{{ #status }}\"></div>
{% live topic=\"bridge/origin/public/status\"
        postback={refresh_status id=id}
        delegate=\"mod_example\" target=#status throttle=3000
%}
```

The delegate receives the configured message and target, and can read the
notification's topic and MQTT message using `z_context:get_q/2` with the binary
keys `topic` and `message`. Extra live-tag arguments are not automatically added
to the postback message; include them in `postback={...}` explicitly. The target
must remain in the DOM for the subscription to stay active.

Throttling updates
------------------

Use `throttle=3000` to limit refreshes during a burst to once every three seconds.
The first notification refreshes after a short delay (at most 100 milliseconds),
then the full interval starts from that refresh. After no notifications for a
full interval, the next notification refreshes quickly again. Notifications are
combined using the latest topic and message. Continuous events do not postpone
the refresh, and the last event is included even if events stop.
Initial rendering is unchanged. The default is `0` (no throttling).

The interval is shared by all topics on one `live` tag. Separate `live` tags
have independent intervals. The argument applies to both template rendering
and the `postback`/`delegate` form of the `live` tag; it is not passed to the
rendered template as a variable. Throttling combines browser refresh requests,
not the MQTT publications themselves.

Use this for templates showing current state, not event-by-event inserts:

```django
{% live template=\"_detail.tpl\" topic=id id=id throttle=3000 %}
```

Live topics
-----------

Any MQTT topic can be used. The topics are interpreted as local to the page. There are three special topics:

*   Use any integer to map to the resource’s update topic. For example if id is `1234` then the topic will be `bridge/origin/model/rsc/event/1234`
*   Use the tuple `{object id=...}` to listen to changes of outgoing connections from a page. An example of a mapped topic is bridge/origin/model/edge/event/1234/o/+\\`. Use the tuple `{object id=... predicate=...}` to listen to changes of a specific predicate of a page. An example of a mapped topic is `bridge/origin/model/edge/event/1234/o/author`
*   Use the tuple `{subject id=... }` to listen to changes of incoming connections to a page. An example of a mapped topic is `bridge/origin/model/edge/event/1234/s/author`

Note that the topics refer to *client side topics*, that is why the bridge is used to subscribe to server side model events.

It is possible to subscribe to client topics like `\"my/local/topic\"` and have the actions triggered by publish to
`cotonic.broker.publish(\"my/local/topic\", {});` (with any payload).



Live actions
------------

It is possible to wire actions or postbacks to a MQTT topic.

Use the `scomp#wire` with argument `type={mqtt topic=... topic=...}` to
connect to one or more MQTT topics. Add `throttle` inside `type={mqtt ...}`
to combine rapid notifications before executing the wire's actions and postback.
It uses the same millisecond interval, quick first event, and idle reset as the
live tag. All topics of one wire share an interval; separate wires are independent.
The latest notification supplies the event arguments. Omit `throttle` or use `0`
for immediate execution of every notification:


```django
{% wire type={mqtt topic=\"bridge/origin/public/hello\" throttle=3000}
        action={growl text=\"hello\"}
%}
```

And in Erlang this will trigger the above *growl*:


```erlang
z_mqtt:publish(<<\"public/hello\">>, <<>>, z:c(mysite)).
```
").
-behaviour(zotonic_scomp).

-export([
        vary/2,
        render/3,
        event/2,

        event_type_mqtt/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% ---------------------------------------------------------------------
%% Event Type API
%% ---------------------------------------------------------------------

%% @doc Special rendering for the {mqtt} wire event type
event_type_mqtt(#action_event_type{event={mqtt, Args}, postback_js = PostbackJS, action_js = ActionJS}, Context) ->
    Topics = map_topics( proplists:get_all_values(topic, Args), Context ),
    Script = iolist_to_binary([
        <<"cotonic.broker.subscribe(">>,
            z_utils:js_array(Topics),
            <<", z_live.throttle(function(msg, _params, options) { ",
                "var zEvtArgs = undefined; ",
                "if (typeof msg == 'object') { ",
                "    var zEvtArgs = ensure_name_value(msg); ",
                "    zEvtArgs.unshift({name: 'topic', value: options.topic}); ",
                "    zEvtArgs.unshift({name: 'wid', value: options.wid}); ",
                "}">>,
                PostbackJS,
                ActionJS,
            "},", integer_to_binary(throttle(Args)), ")",
        ");"
    ]),
    {ok, <<"cotonic.ready.then( function() { ", Script/binary, " });">>, Context}.

%% ---------------------------------------------------------------------
%% Scomp API
%% ---------------------------------------------------------------------

vary(_Params, _Context) ->
    nocache.

render(Params, _Vars, Context) ->
    case proplists:get_value(template, Params) of
        undefined ->
            render_as_postback(Params, Context);
        Template ->
            render_as_template(Template, Params, Context)
    end.

render_as_template(Template, Params, Context) ->
    {LiveVars, TplVars} = lists:partition(
        fun({topic, _}) -> true;
           ({template, _}) -> true;
           ({catinclude, _}) -> true;
           ({element, _}) -> true;
           ({method, _}) -> true;
           ({throttle, _}) -> true;
           (_) -> false
        end,
        Params),
    Template1 = case z_convert:to_bool(proplists:get_value(catinclude, LiveVars)) of
        true when is_list(Template); is_binary(Template) -> {cat, Template};
        _ -> Template
    end,
    LiveVars1 = case Template1 of
        Template -> LiveVars;
        _ -> [ {template, Template1} | proplists:delete(template, LiveVars) ]
    end,
    {HasTarget, Target} = case proplists:get_value(target, Params) of
        undefined ->
            {false, <<"live-", (z_ids:id(10))/binary>>};
        TargetParam ->
            {true, TargetParam}
    end,
    Method = z_convert:to_binary(proplists:get_value(method, Params, <<"update">>)),
    case Method of
        <<"update">> ->
            % In case of 'update' we do an initial render of the template.
            % In all other cases we only render the template if the live tag
            % is triggered.
            TplVars1 = [
                {is_live_update, false},
                {target, Target}
                | TplVars
            ],
            Html = opt_wrap_element(
                        HasTarget,
                        proplists:get_value(element, LiveVars, "div"),
                        Target,
                        z_template:render(Template1, TplVars1, Context)),
            {ok, [
                Html,
                {javascript, script(Target, Method, LiveVars1, TplVars, Context)}
            ]};
        _ ->
            Html = opt_wrap_element(
                        HasTarget,
                        proplists:get_value(element, LiveVars, "div"),
                        Target,
                        <<>>),
            {ok, [
                Html,
                {javascript, script(Target, Method, LiveVars1, TplVars, Context)}
            ]}
    end.

render_as_postback(Params, Context) ->
    {postback, Tag} = proplists:lookup(postback, Params),
    {delegate, Delegate} = proplists:lookup(delegate, Params),
    {target, Target} = proplists:lookup(target, Params),
    Method = proplists:get_value(method, Params),
    Postback = z_render:make_postback_info(Tag, undefined, undefined, Target, Delegate, Context),
    Topics = map_topics(  proplists:get_all_values(topic, Params), Context ),
    Script = iolist_to_binary([
        <<"z_live.subscribe(">>,
            z_utils:js_array(Topics),$,,
            $',z_utils:js_escape(Target), $',$,,
            if
                Method == <<"patch">> -> <<"true">>;
                true -> <<"false">>
            end, $,,
            $',Postback,$',$,,
            integer_to_binary(throttle(Params)),
        $), $;
    ]),
    {ok, {javascript, Script}}.

event(#postback{message={live, Method, Template, TplVars}, target=Target}, Context) ->
    Context1 = maybe_add_q(z_context:get_q(<<"message">>, Context), Context),
    Render = #render{
        template=Template,
        vars=[
            {is_live_update, true},
            {target, Target}
            | TplVars
        ]
    },
    render(Method, Target, Render, Context1).

maybe_add_q(#{ <<"payload">> := Payload }, Context) ->
    add_q(Payload, Context);
maybe_add_q(#{ payload := Payload }, Context) ->
    add_q(Payload, Context);
maybe_add_q(_, Context) ->
    Context.

add_q(undefined, Context) ->
    Context;
add_q([ {K, _} | _ ] = Qs, Context) when is_binary(K) ->
    Context1 = z_context:delete_q([ <<"topic">>, <<"message">> ], Context),
    z_context:add_q(Qs, Context1);
add_q([ [K, _] | _ ] = Qs, Context) when is_binary(K) ->
    Context1 = z_context:delete_q([ <<"topic">>, <<"message">> ], Context),
    z_context:add_q(Qs, Context1);
add_q(Qs, Context) when is_map(Qs) ->
    Context1 = z_context:delete_q([ <<"topic">>, <<"message">> ], Context),
    z_context:add_q(Qs, Context1);
add_q(V, Context) ->
    z_context:add_q(<<"payload">>, V, Context).


%% ---------------------------------------------------------------------
%% Support functions
%% ---------------------------------------------------------------------

opt_wrap_element(true, _, _, Html) ->
    Html;
opt_wrap_element(false, "", _, Html) ->
    Html;
opt_wrap_element(false, <<>>, _, Html) ->
    Html;
opt_wrap_element(false, Element, Id, Html) ->
    [
        $<, Element, " id='", z_utils:js_escape(Id), "'>",
            Html,
        $<, $/, Element, $>
    ].

script(Target, Method, LiveVars, TplVars, Context) ->
    Tag = {live, Method, proplists:get_value(template, LiveVars), TplVars},
    Postback = z_render:make_postback_info(Tag, undefined, undefined, Target, ?MODULE, Context),
    Topics = map_topics( proplists:get_all_values(topic, LiveVars), Context ),
    iolist_to_binary([
        <<"z_live.subscribe(">>,
            z_utils:js_array(Topics),$,,
            $',z_utils:js_escape(Target), $',$,,
            if
                Method == <<"patch">> -> <<"true">>;
                true -> <<"false">>
            end, $,,
            $',Postback,$',$,,
            integer_to_binary(throttle(LiveVars)),
        $), $;
    ]).

%% Minimum interval between live refreshes, in milliseconds. Disabled by default.
throttle(Params) ->
    case z_convert:to_integer(proplists:get_value(throttle, Params, 0)) of
        N when is_integer(N), N > 0 -> N;
        _ -> 0
    end.

map_topics(Topics, Context) ->
    lists:filtermap(
        fun(T) ->
            case z_mqtt:map_topic(T, Context) of
                {ok, T1} when is_binary(T); is_list(T) ->
                    {true, z_mqtt:flatten_topic(T1)};
                {ok, T1} ->
                    % Ensure that topics for predicates or resources
                    % are referring to the origin (aka server)
                    {true, z_mqtt:origin_topic( z_mqtt:flatten_topic(T1) )};
                {error, Reason} ->
                    ?LOG_NOTICE(#{
                        text => <<"Error on mapping wire topic">>,
                        in => zotonic_mod_mqtt,
                        result => error,
                        reason => Reason,
                        topic => T
                    }),
                    false
            end
        end,
        Topics).

render(<<"patch">>, Target, Render, Context) ->
    #render{
        template = Template,
        vars = Vars
    } = Render,
    {Html, _} = z_template:render_to_iolist(Template, Vars, Context),
    Html1 = iolist_to_binary(Html),
    z_mqtt:publish(<<"~client/model/ui/update/", Target/binary>>, Html1, Context),
    Context;
render(<<"top">>, Target, Render, Context) ->
    z_render:insert_top(Target, Render, Context);
render(<<"bottom">>, Target, Render, Context) ->
    z_render:insert_bottom(Target, Render, Context);
render(<<"after">>, Target, Render, Context) ->
    z_render:insert_after(Target, Render, Context);
render(<<"before">>, Target, Render, Context) ->
    z_render:insert_before(Target, Render, Context);
render(_Update, Target, Render, Context) ->
    % "update", "updateonly"
    z_render:update(Target, Render, Context).
