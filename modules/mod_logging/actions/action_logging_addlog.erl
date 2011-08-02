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
-include("zotonic.hrl").
-export([
    render_action/4
]).

render_action(_TriggerId, TargetId, Args, Context) ->
    SignalProps = proplists:get_value(signal_props, Args),
    Type = proplists:get_value(type, SignalProps),

    {Tpl, Context1} = z_template:render_to_iolist(proplists:get_value(template, Args, "_admin_log_row.tpl"), Args, Context),
    Tpl2 = lists:flatten(z_string:line(erlang:iolist_to_binary(Tpl))),
    {[], z_script:add_script([
                              "$('", z_utils:js_escape(Tpl2), 
                              "').hide().insertBefore('#", TargetId, " li:first').fadeIn().css({backgroundColor:'",
                              log_color(Type), "'}).animate({backgroundColor:'",
                              log_color(bg), "'}, 8000, 'linear');"], Context1)}.


log_color(debug) -> "#ffffff";
log_color(info) -> "#ffff99";
log_color(warning) -> "#ffcc99";
log_color(bg) -> "#f1f1f1";
log_color(_) -> "#f1f1f1".
