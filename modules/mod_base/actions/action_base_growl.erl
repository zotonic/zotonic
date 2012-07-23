%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell

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

-module(action_base_growl).
-include("zotonic.hrl").
-export([render_action/4]).

render_action(_TriggerId, _TargetId, Args, Context) ->
    Text   = proplists:get_value(text, Args, ""),
    Stay   = proplists:get_value(stay, Args, 0),
    Type   = proplists:get_value(type, Args, "notice"),

    TextJS = z_utils:js_escape(Text, Context),
    StayJS = case z_convert:to_bool(Stay) of 
                true  -> $1;
                false -> $0
             end,
	TypeJS = z_utils:js_escape(Type, Context),
	Script = [<<"z_growl_add(\"">>,TextJS,<<"\", ">>, StayJS,<<",\"">>, TypeJS, $", $), $;],
	{Script, Context}.
