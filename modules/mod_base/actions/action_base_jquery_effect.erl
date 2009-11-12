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
    Id      = proplists:get_value(id, Args, TargetId),
	Type    = proplists:get_value(type, Args),
	Effect  = proplists:get_value(effect, Args),
	Speed   = proplists:get_value(speed, Args, 250),
	Class   = proplists:get_value(class, Args),
	Easing  = proplists:get_value(easing, Args),
	Effect  = proplists:get_value(effect, Args),
	Options = to_js(proplists:get_value(options, Args, [])),

	Script = case z_convert:to_atom(Type) of
	    'show'   when Speed=="normal" -> "show();";
		'hide'   when Speed=="normal" -> "hide();";
		'show'          -> io_lib:format("show(~p);", [Speed]);
		'hide'          -> io_lib:format("hide(~p);", [Speed]);
		'slide_toggle'  -> io_lib:format("slideToggle(~p);", [Speed]);
		'toggle'        -> <<"toggle();">>;
		'add_class'     -> io_lib:format("addClass('~s');", [Class]);
		'remove_class'  -> io_lib:format("removeClass('~s');", [Class]);
		'toggle_class'  -> io_lib:format("toggleClass('~s');", [Class]);
		'fade_in'       -> io_lib:format("fadeIn(~p);", [Speed]);
		'fade_out'      -> io_lib:format("fadeOut(~p);", [Speed]);
		'slide_down'    -> io_lib:format("slideDown(~p);", [Speed]);
		'slide_up'      -> io_lib:format("slideUp(~p);", [Speed]);
		'slide_fade_out'-> io_lib:format("animate({opacity: 'hide', height: 'hide'}, ~p);", [Speed]);
		'slide_fade_in' -> io_lib:format("animate({opacity: 'show', height: 'show'}, ~p);", [Speed]);
        'disable'       -> "attr('disabled', true).addClass('disabled');";
        'enable'        -> "attr('disabled', false).removeClass('disabled');";

        %% @todo check these, i think that with jQuery 1.3 they should be 'animate' with a js_object/2 output
		'effect'        -> io_lib:format("effect('~s', ~s, ~p);", [Effect, Options, Speed]);
		'animate'       -> io_lib:format("animate(~s, ~p, '~s');", [Options, Speed, Easing])
	end,

	Script2 = [<<"$('#">>,Id,<<"').">>,Script],
	
	{Script2, Context}.

	
to_js(Options) ->
	F = fun({Key, Value}) ->
		case Value of 
    		true ->
    			io_lib:format("~s: true", [Key]);
        	false ->
        		io_lib:format("~s: false", [Key]);
			V ->
				io_lib:format("~s: '~s'", [Key, z_utils:js_escape(V)])
		end
	end,
	Options1 = [F(X) || X <- Options],
	Options2 = string:join(Options1, ","),
	io_lib:format("{ ~s }", [Options2]).
