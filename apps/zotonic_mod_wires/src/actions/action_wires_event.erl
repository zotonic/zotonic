%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2013 Marc Worrell
%%
%% Based on code copyright (c) 2008-2009 Rusty Klophaus

%% Copyright 2009-2013 Marc Worrell
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

-module(action_wires_event).
-include_lib("zotonic_core/include/zotonic.hrl").
-export([render_action/4]).

render_action(TriggerId, TargetId, Args, Context) ->
    Trigger   = proplists:get_value(id, Args, TriggerId),
    EventType = proplists:get_value(type, Args),
    Propagate = z_convert:to_bool(proplists:get_value(propagate, Args, false)),
    Postback  = proplists:get_value(postback, Args),
    Delegate  = proplists:get_value(delegate, Args),
    Actions   = proplists:get_all_values(action, Args),
    QArgs     = proplists:get_all_values(qarg, Args),

    {PostbackMsgJS, PickledPostback} = z_render:make_postback(Postback, EventType, Trigger, TargetId,
                                                              Delegate, QArgs, Context),
    {ActionsJS,Context1} = z_render:render_actions(Trigger, TargetId, Actions, Context),

    script(map_type(EventType), TriggerId, Trigger, PostbackMsgJS, PickledPostback, ActionsJS, Propagate, Args, Context1).

%%% ---------------------------------------------------------------------------------
%%% Render actions for the different types
%%% ---------------------------------------------------------------------------------

map_type("enterkey") -> enterkey;
map_type("continuation") -> continuation;
map_type("interval") -> interval;
map_type("submit") -> submit;
map_type("named") -> named;
map_type("none") -> none;
map_type("load") -> load;
map_type("inline") -> inline;
map_type("visible") -> visible;
map_type(<<"enterkey">>) -> enterkey;
map_type(<<"continuation">>) -> continuation;
map_type(<<"interval">>) -> interval;
map_type(<<"submit">>) -> submit;
map_type(<<"named">>) -> named;
map_type(<<"none">>) -> none;
map_type(<<"load">>) -> load;
map_type(<<"inline">>) -> inline;
map_type(<<"visible">>) -> visible;
map_type(Type) -> Type.

script(enterkey, _TriggerId, Trigger, PostbackMsgJS, _PickledPostback, ActionsJS, Propagate, Args, Context) ->
    {[
        z_render:render_css_selector(z_render:css_selector(Trigger, Args)),
        <<"'.on('keypress', ">>,
        <<"function(event) { if (z_is_enter_key(event)) { ">>, PostbackMsgJS, ActionsJS,
        case Propagate of
            true -> $;;
            false -> <<"; return false;">>
        end,
        <<" } } );\n">>
    ], Context};

%%% Interval - periodically execute the actions

script(continuation, TriggerId, Trigger, PostbackMsgJS, PickledPostback, ActionsJS, Propagate, Args, Context) ->
    script(interval, TriggerId, Trigger, PostbackMsgJS, PickledPostback, ActionsJS, Propagate, Args, Context);
script(interval, _TriggerId, _Trigger, PostbackMsgJS, _PickledPostback, ActionsJS, _Propagate, Args, Context) ->
    Interval = proplists:get_value(interval, Args, 250),
    {[
        <<"setTimeout(function() { ">>,
            z_utils:js_escape(PostbackMsgJS),
            z_utils:js_escape(ActionsJS),
        <<" }, ">>,
        io_lib:format("~p", [Interval]), <<");\n">>
    ], Context};

%%% Submit - connected to a form

script(submit, TriggerId, _Trigger, _PostbackMsgJS, PickledPostback, ActionsJS, _Propagate, _Args, Context) ->
    SubmitPostback = [
        <<"$('#">>, TriggerId, <<"').data('z_submit_postback',\"">>, PickledPostback, <<"\")">>
    ],
    {case ActionsJS of
        [] -> [SubmitPostback, $;, $\n];
        _  -> [SubmitPostback, <<".data('z_submit_action', function() { ">>, ActionsJS, <<"});\n">>]
     end, Context};

%%% Named - register for later trigger

script(named, _TriggerId, Trigger, PostbackMsgJS, _PickledPostback, ActionsJS, _Propagate, Args, Context) ->
    Name = proplists:get_value(name, Args, Trigger),
    {[
        <<"z_event_register(\"">>,z_utils:js_escape(Name),<<"\", function(zEvtArgs) {">>,
            PostbackMsgJS, ActionsJS, <<"});\n">>
    ], Context};

%%% Inline

script(none, TriggerId, Trigger, PostbackMsgJS, PickledPostback, ActionsJS, Propagate, Args, Context) ->
    script(inline, TriggerId, Trigger, PostbackMsgJS, PickledPostback, ActionsJS, Propagate, Args, Context);
script(load, TriggerId, Trigger, PostbackMsgJS, PickledPostback, ActionsJS, Propagate, Args, Context) ->
    script(inline, TriggerId, Trigger, PostbackMsgJS, PickledPostback, ActionsJS, Propagate, Args, Context);
script(undefined, TriggerId, Trigger, PostbackMsgJS, PickledPostback, ActionsJS, Propagate, Args, Context) ->
    script(inline, TriggerId, Trigger, PostbackMsgJS, PickledPostback, ActionsJS,Propagate, Args,  Context);
script(inline, _TriggerId, _Trigger, PostbackMsgJS, _PickledPostback, ActionsJS, _Propagate, _Args, Context) ->
    {[
        PostbackMsgJS, ActionsJS
    ], Context};

%%% Visible - keep track of visibility, trigger when target becomes visible

script(visible, TriggerId, _Trigger, PostbackMsgJS, _PickledPostback, ActionsJS, _Propagate, _Args, Context) ->
    {[
        <<"z_on_visible('#">>, TriggerId, <<"', function() {">>, PostbackMsgJS, ActionsJS, <<"});\n">>
    ], Context};

%%% Custom events, always a tuple with optional args.

script(EventType, TriggerId, Trigger, PostbackMsgJS, PickledPostback, ActionsJS, _Propagate, _Args, Context) when is_tuple(EventType) ->
    case z_notifier:first(#action_event_type{
            event = EventType,
            trigger_id = TriggerId,
            trigger = Trigger,
            postback_js = PostbackMsgJS,
            postback_pickled = PickledPostback,
            action_js = ActionsJS
        }, Context)
    of
        {ok, Script, Context} ->
            {Script, Context};
        undefined ->
            ?LOG_ERROR(#{
                text => <<"Unhandled event type for wires scomp">>,
                in => zotonic_mod_wires,
                type => EventType
            }),
            {[], Context}
    end;

%%% DOM Events

script(EventType, _TriggerId, Trigger, PostbackMsgJS, _PickledPostback, ActionsJS, Propagate, Args, Context) ->
    {[
        z_render:render_css_selector(z_render:css_selector(Trigger, Args)),
        <<".on('">>, z_convert:to_list(EventType), <<"', ">>,
        <<"function(event) { ">>, PostbackMsgJS, ActionsJS,
        case Propagate of
            true -> <<>>;
            false -> <<" return z_opt_cancel(this);">>
        end,
        <<" } );\n">>
    ], Context}.
