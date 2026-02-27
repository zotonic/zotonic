%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2023 Marc Worrell
%% @doc Simple live updating events

%% Copyright 2014-2023 Marc Worrell
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
-moduledoc("
Live updating templates connected to [MQTT topics](/id/doc_module_mod_mqtt).

Note

The live tag is provided by [mod_mqtt](/id/doc_module_mod_mqtt), which must be enabled.

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

Add the argument `catinclude` to do a [catinclude](/id/doc_template_tag_tag_catinclude) instead of a normal
[include](/id/doc_template_tag_tag_include). For a catinclude the argument `id` must be present:


```erlang
{% live template=\"_detail.tpl\" topic=id catinclude id=id %}
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

Use the [wire tag](/id/doc_template_scomp_scomp_wire#scomp-wire) with argument `type={mqtt topic=... topic=...}` to
connect to one or more MQTT topics:


```django
{% wire type={mqtt topic=\"bridge/origin/public/hello\"} action={growl text=\"hello\"} %}
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
            <<", function(msg, _params, options) { ",
                "var zEvtArgs = undefined; ",
                "if (typeof msg == 'object') { ",
                "    var zEvtArgs = ensure_name_value(msg); ",
                "    zEvtArgs.unshift({name: 'topic', value: options.topic}); ",
                "    zEvtArgs.unshift({name: 'wid', value: options.wid}); ",
                "}">>,
                PostbackJS,
                ActionJS,
            "}",
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
            $',Postback,$',
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
            $',Postback,$',
        $), $;
    ]).

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
