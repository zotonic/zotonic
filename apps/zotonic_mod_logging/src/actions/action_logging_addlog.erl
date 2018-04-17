%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse <arjan@scherpenisse.net>

%% Copyright 2010 Arjan Scherpenisse <arjan@scherpenisse.net>
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

-module(action_logging_addlog).
-include_lib("zotonic_core/include/zotonic.hrl").
-export([
    render_action/4
]).

render_action(_TriggerId, TargetId, Args, Context) ->
    {Tpl, Context1} = z_template:render_to_iolist(proplists:get_value(template, Args, "_admin_log_row.tpl"), Args, Context),
    Tpl2 = lists:flatten(z_string:line(erlang:iolist_to_binary(Tpl))),
    {[], z_render:add_script(
           ["$('", z_utils:js_escape(Tpl2),
            "').hide().prependTo('#", TargetId, "').fadeIn();",
            "$('h3').effect('shake', {direction: 'up', distance: 5, times: 2});"],
           Context1)}.

