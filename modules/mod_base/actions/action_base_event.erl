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

-module(action_base_event).
-include("zotonic.hrl").
-export([render_action/4]).

render_action(TriggerId, TargetId, Args, Context) -> 
	EventType = proplists:get_value(type, Args),
	Postback  = proplists:get_value(postback, Args),
    Delegate  = proplists:get_value(delegate, Args),
    Actions   = proplists:get_all_values(action, Args),

	{PostbackMsgJS, PickledPostback} = z_render:make_postback(Postback, EventType, TriggerId, TargetId, Delegate, Context),
	{ActionsJS,Context1} = z_render:render_actions(TriggerId, TargetId, Actions, Context),

    Script = if
        		EventType == enterkey orelse EventType == "enterkey" ->
        			[
        	            <<"$('#">>, TriggerId, <<"').bind('keypress', ">>,
        	            <<"function(event) { if (z_is_enter_key(event)) { ">>, PostbackMsgJS, ActionsJS, <<"; return false; } } );\n">>
        			];

        		EventType == interval   orelse EventType == continuation orelse
        		EventType == "interval" orelse EventType == "continuation" ->
        		    Interval = proplists:get_value(interval, Args, 250),
        		    [
        		        <<"setTimeout(\"">>,z_utils:js_escape(PostbackMsgJS), z_utils:js_escape(ActionsJS), <<"\", ">>,
        		        io_lib:format("~p", [Interval]), <<");\n">>
        		    ];

                EventType == submit orelse EventType == "submit" ->
                    SubmitPostback = [
                        <<"$('#">>, TriggerId, <<"').data('z_submit_postback',\"">>, PickledPostback, <<"\")">>
                    ],
                    case ActionsJS of
                        [] -> [SubmitPostback, $;, $\n];
                        _  -> [SubmitPostback, <<".data('z_submit_action', \"">>, z_utils:js_escape(ActionsJS), <<"\");\n">>]
                    end;

                %% Should check this against the wire() function.
                EventType == undefined orelse EventType == "none" ->
                    [
                        PostbackMsgJS, ActionsJS
                    ];
                    
        		true ->
        		    [
    	                <<"$('#">>, TriggerId, <<"').bind('">>, z_convert:to_list(EventType), <<"', ">>,
        	            <<"function(event) { ">>, PostbackMsgJS, ActionsJS, <<" return z_opt_cancel(this); } );\n">>
        	        ]
        	end,

	{Script,Context1}.
