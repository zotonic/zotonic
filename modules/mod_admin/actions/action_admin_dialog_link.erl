%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-26
%% @doc Open a dialog where the user can select an object

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

-module(action_admin_dialog_link).
-author("Marc Worrell <marc@worrell.nl").
-include("zotonic.hrl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

render_action(TriggerId, TargetId, Args, Context) ->
    SubjectId = z_convert:to_integer(proplists:get_value(subject_id, Args)),
    Predicate = proplists:get_value(predicate, Args),
    ElementId = proplists:get_value(element_id, Args),
    EdgeTemplate = proplists:get_value(edge_template, Args),
    Actions   = proplists:get_all_values(action, Args),
    Postback = {dialog_link, SubjectId, Predicate, ElementId, EdgeTemplate, Actions},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Unlink the edge, on success show an undo message in the element with id "unlink-message"
%% @spec event(Event, Context1) -> Context2
event(#postback{message={dialog_link, SubjectId, Predicate, ElementId, EdgeTemplate, Actions}}, Context) ->
    Pred = m_predicate:get(Predicate, Context),
    Title = ["Add a connection: ", ?__(proplists:get_value(title, Pred), Context)],
    PredCat = case m_predicate:object_category(Predicate, Context) of
                  [{Id}] -> Id;
                  _ -> undefined
              end,
    Vars = [
        {subject_id, SubjectId},
        {predicate, Predicate},
        {element_id, ElementId},
        {action, Actions},
        {predicate_cat, PredCat},
        {edge_template, EdgeTemplate}
    ],
    z_render:dialog(Title, "_action_dialog_link.tpl", Vars, Context).
