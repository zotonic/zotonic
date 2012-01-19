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

-module(action_base_confirm).
-include("zotonic.hrl").
-export([
    render_action/4,
    event/2
]).


render_action(TriggerId, TargetId, Args, Context) -> 
    {PostbackMsgJS, _PickledPostback} = z_render:make_postback({confirm, Args}, click, TriggerId, TargetId, ?MODULE, Context),
    {PostbackMsgJS, Context}.

%% @doc Fill the dialog with the confirmation template.
%% @spec event(Event, Context1) -> Context2
event(#postback{message={confirm, Args}}, Context) ->
    Title = proplists:get_value(title, Args, ?__(<<"Confirm">>, Context)),
    {IsTemplate, Text,Context1} = case proplists:get_value(text_template, Args) of
              undefined ->
                  {false, proplists:get_value(text, Args), Context};
              Template ->
                  {Txt, Ctx} = z_template:render_to_iolist(Template, Args, Context),
                  {true, Txt, Ctx}
           end,
    Vars = [
        {title, Title},
        {text, Text},
        {is_template, IsTemplate},
        {ok, proplists:get_value(ok, Args)},
        {cancel, proplists:get_value(cancel, Args)},
        {action, proplists:get_all_values(action, Args)},
        {on_cancel, proplists:get_all_values(on_cancel, Args)},
        {postback, proplists:get_value(postback, Args)},
        {delegate, proplists:get_value(delegate, Args)}
    ],
    z_render:dialog(Title, "_action_dialog_confirm.tpl", Vars, Context1).
