%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-26
%% @doc Remove an edge between two resources

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

-module(action_admin_unlink).
-author("Marc Worrell <marc@worrell.nl").
-include_lib("zotonic_core/include/zotonic.hrl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

render_action(TriggerId, TargetId, Args, Context) ->
    EdgeId = z_convert:to_integer(proplists:get_value(edge_id, Args)),
    SubjectId = z_convert:to_integer(proplists:get_value(subject_id, Args)),
    ObjectId = z_convert:to_integer(proplists:get_value(object_id, Args)),
    Predicate = proplists:get_value(predicate, Args),
    Hide = proplists:get_value(hide, Args),
    EdgeTemplate = proplists:get_value(edge_template, Args),
    Action = proplists:get_all_values(action, Args),
    UndoAction = proplists:get_all_values(undo_action, Args),
    UndoMessageId = proplists:get_value(undo_message_id, Args, "unlink-undo-message"),

    Action1 = case Hide of
                  undefined -> Action;
                  _ -> [{remove, [{fadeout, true}, {target, Hide}, {speed, 200}]}|Action]
              end,

    Postback = {unlink, EdgeId, SubjectId, Predicate, ObjectId, UndoMessageId, EdgeTemplate, Action1, UndoAction},
    {PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
    {PostbackMsgJS, Context}.


%% @doc Unlink the edge, on success show an undo message in the element with id "undo-message"
%% @spec event(Event, Context1) -> Context2
event(#postback{message={unlink, EdgeId, SubjectId, Predicate, ObjectId, UndoMessageId, EdgeTemplate, Action, UndoAction}}, Context) ->
    case z_acl:rsc_linkable(SubjectId, Context) of
        true ->
            {SubjectId, Predicate1, ObjectId1} = case EdgeId of
                    undefined -> {SubjectId, Predicate, ObjectId};
                    _ -> m_edge:get_triple(EdgeId, Context)
                end,
            ok = m_edge:delete(SubjectId, Predicate1, ObjectId1, Context),
            Vars = [
                {subject_id, SubjectId},
                {predicate, Predicate1},
                {object_id, ObjectId1},
                {action, UndoAction},
                {edge_template, EdgeTemplate}
            ],
            Html = z_template:render("_action_unlink_undo.tpl", Vars, Context),
            Context1 = z_render:update(UndoMessageId, Html, Context),
            z_render:wire(Action, Context1);
        false ->
            z_render:growl_error("Sorry, you have no permission to edit this page.", Context)
    end.

