%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Based on code copyright (c) 2008-2009 Rusty Klophaus

%% Copyright 2009 Marc Worrell
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

-module(action_base_jquery_effect).
-include("zotonic.hrl").
-export([render_action/4]).


render_action(_TriggerId, TargetId, Args, Context) ->
    Type    = proplists:get_value(type, Args),
    Effect  = proplists:get_value(effect, Args),
    Speed   = proplists:get_value(speed, Args),
    Class   = proplists:get_value(class, Args),
    Easing  = proplists:get_value(easing, Args),
    Effect  = proplists:get_value(effect, Args),
    Options = to_js(proplists:get_value(options, Args, [])),

    Script = case z_convert:to_atom(Type) of
        'show'          -> [<<"show(">>, map_speed(Speed), $), $;];
        'hide'          -> [<<"hide(">>, map_speed(Speed), $), $;];
        'remove'        -> 
            case proplists:get_value(fadeout, Args) of
                true -> [<<"fadeOut(">>, map_speed_default(Speed), <<",function() { $(this).remove(); });">>];
                _ -> <<"remove();">>
            end;
        'slide_toggle'  -> [<<"slideToggle(">>, map_speed(Speed), $), $;];
        'toggle'        -> <<"toggle();">>;
        'set_class'     -> [<<"attr('class','">>, Class, $', $), $;];
        'add_class'     -> [<<"addClass('">>, Class, $', $), $;];
        'remove_class'  -> [<<"removeClass('">>, Class, $', $), $;];
        'toggle_class'  -> [<<"toggleClass('">>, Class, $', $), $;];
        'fade_in'       -> [<<"fadeIn(">>, map_speed(Speed), $), $;];
        'fade_out'      -> [<<"fadeOut(">>, map_speed(Speed), $), $;];
        'slide_down'    -> [<<"slideDown(">>, map_speed(Speed), $), $;];
        'slide_up'      -> [<<"slideUp(">>, map_speed(Speed), $), $;];
        'slide_fade_out'-> [<<"animate({opacity: 'hide', height: 'hide'}, ">>, map_speed_default(Speed), $), $;];
        'slide_fade_in' -> [<<"animate({opacity: 'show', height: 'show'}, ">>, map_speed_default(Speed), $), $;];
        'disable'       -> "attr('disabled', true).addClass('disabled');";
        'enable'        -> "attr('disabled', false).removeClass('disabled');";

        %% @todo check these, i think that with jQuery 1.3+ they should be 'animate' with a js_object/2 output
        'effect'        -> io_lib:format("effect('~s', ~s, ~p);", [Effect, Options, map_speed_default(Speed)]);
        'animate'       -> io_lib:format("animate(~s, ~p, '~s');", [Options, map_speed_default(Speed), Easing])
    end,
    case z_render:css_selector(TargetId, Args) of
        Empty when Empty == undefined; Empty == []; Empty == <<>> -> {Script, Context};
        Selector -> {[z_render:render_css_selector(Selector),$.,Script], Context}
    end.


map_speed_default(Speed) ->
    case map_speed(Speed) of
        [] -> "500";
        S -> S
    end.

map_speed(N) when is_integer(N) -> integer_to_list(N);
map_speed("normal") -> [];
map_speed(undefined) -> [];
map_speed(<<>>) -> [];
map_speed([]) -> [];
map_speed(Speed) -> 
    case z_utils:only_digits(Speed) of
        true -> Speed;
        false -> [$', z_utils:js_escape(Speed), $']
    end.

    
to_js(Options) ->
    F = fun({Key, Value}) ->
        case Value of 
            true -> [Key, <<": true">>];
            false -> [Key, <<": false">>];
            V -> [Key, $', z_utils:js_escape(V), $']
        end
    end,
    [${, string:join([F(X) || X <- Options], ","), $}].

