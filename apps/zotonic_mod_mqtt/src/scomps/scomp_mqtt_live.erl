%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014 Marc Worrell
%% @doc Simple live updating events

%% Copyright 2014 Marc Worrell
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
    Topics = [ z_mqtt:map_topic(V, Context) || V <- proplists:get_all_values(topic, Args) ],
    Script = iolist_to_binary([
        <<"pubzub.subscribe_multi(">>,
            z_utils:js_array(Topics),$,,
            <<"function(topic, msg, sub_id) { var zEvtArgs=pubzub.make_zEvtArgs(topic, msg, sub_id);">>,
                PostbackJS,
                ActionJS,
              $},
        $), $;
    ]),
    {ok, Script, Context}.

%% ---------------------------------------------------------------------
%% Scomp API
%% ---------------------------------------------------------------------

vary(_Params, _Context) ->
    nocache.

render(Params, _Vars, Context) ->
    case proplists:get_value(template, Params) of
        undefined ->
            render_postback(Params, Context);
        Template ->
            render_template(Template, Params, Context)
    end.

render_template(Template, Params, Context) ->
    Target = proplists:get_value(target, Params, z_ids:identifier(16)),
    Where = z_convert:to_binary(proplists:get_value(where, Params, <<"update">>)),
    {LiveVars,TplVars} = lists:partition(
                fun({topic,_}) -> true;
                   ({template,_}) -> true;
                   ({catinclude,_}) -> true;
                   ({element, _}) -> true;
                   ({where, _}) -> true;
                   (_) -> false
                end,
                Params),
    case Where of
        <<"update">> ->
            Template1 = case z_convert:to_bool(proplists:get_value(catinclude, LiveVars)) of
                            true when is_list(Template); is_binary(Template) ->
                                {cat, Template};
                            _ -> Template
                        end,
            LiveVars1 = case Template1 of
                            Template -> LiveVars;
                            _ ->
                                [{template, Template1} | proplists:delete(template, LiveVars)]
                        end,
            TplVars1 = [ {target, Target} | TplVars ],
            Html = opt_wrap_element(
                        proplists:get_value(element, LiveVars, "div"),
                        Target,
                        z_template:render(Template1, TplVars1, Context)),
            {ok, [
                    Html,
                    {javascript, script(Target, Where, LiveVars1, TplVars, Context)}
                ]};
        _ ->
            {ok, {javascript, script(Target, Where, LiveVars, TplVars, Context)}}
    end.

render_postback(Params, Context) ->
    {postback, Tag} = proplists:lookup(postback, Params),
    {delegate, Delegate} = proplists:lookup(delegate, Params),
    {target, Target} = proplists:lookup(target, Params),
    Postback = z_render:make_postback_info(Tag, undefined, undefined, Target, Delegate, Context),
    Topics = [ z_mqtt:map_topic(V, Context) || V <- proplists:get_all_values(topic, Params) ],
    Script = iolist_to_binary([
        <<"z_live.subscribe(">>,
            z_utils:js_array(Topics),$,,
            $',z_utils:js_escape(Target), $',$,,
            $',Postback,$',
        $), $;
    ]),
    {ok, {javascript, Script}}.

event(#postback{message={live, Where, Template, TplVars}, target=Target}, Context) ->
    Render = #render{template=Template, vars=[{target,Target}|TplVars]},
    render(Where, Target, Render, Context).


%% ---------------------------------------------------------------------
%% Support functions
%% ---------------------------------------------------------------------

opt_wrap_element("", _, Html) ->
    Html;
opt_wrap_element(<<>>, _, Html) ->
    Html;
opt_wrap_element(Element, Id, Html) ->
    [
        $<, Element, " id='", z_utils:js_escape(Id), "'>",
            Html,
        $<, $/, Element, $>
    ].

script(Target, Where, LiveVars, TplVars, Context) ->
    Tag = {live, Where, proplists:get_value(template,LiveVars), TplVars},
    Postback = z_render:make_postback_info(Tag, undefined, undefined, Target, ?MODULE, Context),
    Topics = [ z_mqtt:map_topic(V, Context) || V <- proplists:get_all_values(topic, LiveVars) ],
    iolist_to_binary([
        <<"z_live.subscribe(">>,
            z_utils:js_array(Topics),$,,
            $',z_utils:js_escape(Target), $',$,,
            $',Postback,$',
        $), $;
    ]).

render(<<"top">>, Target, Render, Context) ->
    z_render:insert_top(Target, Render, Context);
render(<<"bottom">>, Target, Render, Context) ->
    z_render:insert_bottom(Target, Render, Context);
render(<<"after">>, Target, Render, Context) ->
    z_render:insert_after(Target, Render, Context);
render(<<"before">>, Target, Render, Context) ->
    z_render:insert_before(Target, Render, Context);
render(_Update, Target, Render, Context) ->
    z_render:update(Target, Render, Context).


