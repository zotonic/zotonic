%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse
%% Date: 2011-06-25
%% @doc Edit the basic properties of a rsc in a dialog.

%% Copyright 2011 Arjan Scherpenisse
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

-module(action_admin_dialog_edit_basics).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    EdgeId = proplists:get_value(edge_id, Args),
    Callback = proplists:get_value(callback, Args),
    RscId = proplists:get_value(id, Args),
    Template = proplists:get_value(template, Args),
    Actions = proplists:get_all_values(action, Args),
    Postback = {edit_basics, RscId, EdgeId, Template, Actions, Callback},
    {PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
    {PostbackMsgJS, Context}.


%% @doc Fill the dialog with the edit basics form. The form will be posted back to this module.
%% @spec event(Event, Context1) -> Context2
event(#postback{message={edit_basics, RscId, EdgeId, Template, Actions, Callback}, target=TargetId}, Context) ->
    ObjectId = case RscId of
                    undefined ->
                        case EdgeId of
                            undefined ->
                                z_convert:to_integer(z_context:get_q("id", Context));
                            _ ->
                                {_, _, OId} = m_edge:get_triple(EdgeId, Context),
                                OId
                        end;
                    _ -> 
                        RscId
               end,
    TargetId1 = z_context:get_q("element_id", Context, TargetId),
    Vars = [
        {delegate, atom_to_list(?MODULE)},
        {id, ObjectId},
        {edge_id, EdgeId},
        {template, Template},
        {update_element, TargetId1},
        {is_update, z_convert:to_bool(z_context:get_q("is_update", Context))},
        {actions, Actions},
        {callback, Callback}
    ],
    Title = z_convert:to_list(z_trans:lookup_fallback(m_rsc:p(ObjectId, title, Context), Context)),
    z_render:dialog("Edit " ++ Title, "_action_dialog_edit_basics.tpl", Vars, Context);

%% @doc Save the thing and close the dialog.
event(#submit{message={rsc_edit_basics, Args}}, Context) ->
    {id, Id} = proplists:lookup(id, Args),
    {edge_id, EdgeId} = proplists:lookup(edge_id, Args),
    Actions = proplists:get_value(actions, Args, []),

    Post = z_context:get_q_all_noz(Context),
    Props = controller_admin_edit:filter_props(Post),
    Props1 = proplists:delete("id", Props),

    case m_rsc:update(Id, Props1, Context) of
        {ok, _} ->
            Vars = case EdgeId of
                     undefined ->
                        [ {id, Id} ];
                     _Other ->
                        {SubjectId, Predicate, Id} = m_edge:get_triple(EdgeId, Context),
                        [
                            {subject_id, SubjectId},
                            {predicate, Predicate},
                            {object_id, Id},
                            {edge_id, EdgeId}
                        ]
                  end,
            Html = z_template:render(case proplists:get_value(template, Args) of 
                                        undefined -> "_rsc_edge.tpl"; 
                                        X -> X
                                      end,
                                      Vars,
                                      Context),
            Context1 = case proplists:get_value(is_update, Args) of
                          true -> z_render:update(proplists:get_value(update_element, Args), Html, Context);
                          false -> z_render:replace(proplists:get_value(update_element, Args), Html, Context)
                        end,
            Context2 = z_render:wire({dialog_close, []}, Context1),
            %% wire any custom actions
            Context3 = case proplists:get_value(callback, Args) of
                            undefined -> Context2;
                            Callback -> 
                                Title = m_rsc:p(Id, title, Context2),
                                z_render:wire({script, [{script, [
                                                Callback,
                                                $(,$",integer_to_list(Id),$",$,,
                                                   $",z_utils:js_escape(Title,Context2),$",$),$;
                                            ]}]}, Context2)
                       end,
            z_render:wire([{Action, [{id, Id}|ActionArgs]}|| {Action, ActionArgs} <- Actions], Context3);

        {error, _Reason} ->
            z_render:growl_error(?__("Something went wrong. Sorry.", Context), Context)
    end.
