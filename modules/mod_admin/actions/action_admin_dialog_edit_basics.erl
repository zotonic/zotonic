%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse
%% @date 2011-06-25
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
    EdgeTemplate = proplists:get_value(edge_template, Args),
    Postback = {edit_basics, EdgeId, EdgeTemplate},
    {PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
    {PostbackMsgJS, Context}.


%% @doc Fill the dialog with the edit basics form. The form will be posted back to this module.
%% @spec event(Event, Context1) -> Context2
event({postback, {edit_basics, EdgeId, EdgeTemplate}, _TriggerId, TargetId}, Context) ->
    {_, _, ObjectId} = m_edge:get_triple(EdgeId, Context),
    Vars = [
        {delegate, atom_to_list(?MODULE)},
        {id, ObjectId},
        {edge_id, EdgeId},
        {edge_template, EdgeTemplate},
        {update_element, TargetId}
    ],
    Title = z_convert:to_list(z_trans:lookup_fallback(m_rsc:p(ObjectId, title, Context), Context)),
    z_render:dialog("Edit " ++ Title, "_action_dialog_edit_basics.tpl", Vars, Context);

%% @doc Save the thing and close the dialog.
event({submit, {rsc_edit_basics, Args}, _TriggerId, _TargetId}, Context) ->

    Post = z_context:get_q_all_noz(Context),
    Props = resource_admin_edit:filter_props(Post),
    Props1 = proplists:delete("id", Props),

    EdgeId = proplists:get_value(edge_id, Args),
    {SubjectId, Predicate, ObjectId} = m_edge:get_triple(EdgeId, Context),

    case m_rsc:update(ObjectId, Props1, Context) of
        {ok, _} ->
            Vars = [
                    {subject_id, SubjectId},
                    {predicate, Predicate},
                    {object_id, ObjectId},
                    {edge_id, EdgeId}
                   ],
            Html  = z_template:render(case proplists:get_value(edge_template, Args) of undefined -> "_rsc_edge.tpl"; X -> X end,
                                      Vars,
                                      Context),
            Context1 = z_render:replace(proplists:get_value(update_element, Args), Html, Context),

            z_render:wire({dialog_close, []}, Context1);
        {error, _Reason} ->
            z_render:growl_error("Something went wrong. Sorry.", Context)
    end.
