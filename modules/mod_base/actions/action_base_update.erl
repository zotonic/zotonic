%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-07-16
%% @doc Replace the content of the target with the result of a render action.

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

-module(action_base_update).
-include("zotonic.hrl").
-export([render_action/4]).

render_action(_TriggerId, TargetId, Args, Context) ->
    Update = proplists:get_value(update, Args, TargetId),
    {Html, ContextHtml} = case proplists:get_value(template, Args) of
        undefined ->
            { proplists:get_value(text, Args, ""), Context };
        Template ->
            z_template:render_to_iolist(Template, Args, Context)
    end,
    {[], z_render:update(Update, Html, ContextHtml)}.
