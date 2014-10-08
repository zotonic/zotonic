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
-behaviour(gen_scomp).

-export([
        vary/2,
        render/3,
        event/2,

        event_type_mqtt/2
    ]).

-include("zotonic.hrl").

%% ---------------------------------------------------------------------
%% Event Type API
%% ---------------------------------------------------------------------

%% @doc Special rendering for the {mqtt} wire event type
event_type_mqtt(#action_event_type{event={mqtt, Args}, postback_js = PostbackJS, action_js = ActionJS}, Context) ->
    Topics = [ map_topic(V, Context) || V <- proplists:get_all_values(topic, Args) ],
    Script = iolist_to_binary([
        <<"pubzub.subscribe_multi(">>,
            z_utils:js_array(Topics),$,,
            <<"function(topic, msg, sub_id) {">>, 
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
    Template = proplists:get_value(template, Params),
    Target = proplists:get_value(target, Params, z_convert:to_binary(z_ids:identifier(16))),
    Where = z_convert:to_binary(proplists:get_value(where, Params, <<"update">>)), 
    {LiveVars,TplVars} = lists:partition(
                fun({topic,_}) -> true;
                   ({template,_}) -> true;
                   ({element, _}) -> true;
                   ({where, _}) -> true;
                   (_) -> false
                end,
                Params),
    case Where of
        <<"update">> ->
            TplVars1 = [ {target, Target} | TplVars ],
            Html = opt_wrap_element(
                        proplists:get_value(element, LiveVars, "div"), 
                        Target,
                        z_template:render(Template, TplVars1, Context)),
            {ok, [
                    Html,
                    {javascript, script(Target, Where, LiveVars, TplVars, Context)}
                ]};
        _ ->
            {ok, {javascript, script(Target, Where, LiveVars, TplVars, Context)}}
    end.

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
    Topics = [ map_topic(V, Context) || V <- proplists:get_all_values(topic, LiveVars) ],
    iolist_to_binary([
        <<"z_live.subscribe(">>, 
            z_utils:js_array(Topics),$,,
            $',z_utils:js_escape(Target), $',$,,
            $',Postback,$',
        $), $;
    ]).

map_topic(Id, _Context) when is_integer(Id) ->
    <<"~site/rsc/",(z_convert:to_binary(Id))/binary>>;
map_topic({object, Props}, Context) when is_list(Props) ->
    map_topic_edge($o, Props, Context);
map_topic({subject, Props}, Context) when is_list(Props) ->
    map_topic_edge($s, Props, Context);
map_topic(Topic, _Context) ->
    z_convert:to_binary(Topic).

map_topic_edge(ObjSub, Props, Context) ->
    Id = proplists:get_value(id, Props),
    Predicate = proplists:get_value(predicate, Props),
    Name = to_predicate_name(Predicate, Context),
    <<"~site/rsc/",(z_convert:to_binary(Id))/binary, $/, ObjSub, $/, Name/binary>>.


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


to_predicate_name(undefined, _Context) -> <<"+">>;
to_predicate_name(<<"*">>, _Context) -> <<"+">>;
to_predicate_name("*", _Context) -> <<"+">>;
to_predicate_name('*', _Context) -> <<"+">>;
to_predicate_name(<<>>, _Context) -> <<"+">>;
to_predicate_name(Id, Context) when is_integer(Id) ->
    {ok, Name} = m_predicate:to_name(Id, Context),
    z_convert:to_binary(Name);
to_predicate_name(Pred, _Context) ->
    z_convert:to_binary(Pred).
