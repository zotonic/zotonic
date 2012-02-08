%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-26
%% @doc Add an edge between two resources

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

-module(action_admin_link).
-author("Marc Worrell <marc@worrell.nl").
-include("zotonic.hrl").

%% interface functions
-export([
    render_action/4,
    event/2,
    do_link/5
]).

render_action(TriggerId, TargetId, Args, Context) ->
    SubjectId = z_convert:to_integer(proplists:get_value(subject_id, Args)),
    ObjectId = z_convert:to_integer(proplists:get_value(object_id, Args)),
    Predicate = proplists:get_value(predicate, Args),
    ElementId = proplists:get_value(element_id, Args),
    EdgeTemplate = proplists:get_value(edge_template, Args),
    Action = proplists:get_all_values(action, Args),
    
    Postback = {link, SubjectId, Predicate, ObjectId, ElementId, EdgeTemplate, Action},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Unlink the edge, on success show an undo message in the element with id "unlink-message"
%% @spec event(Event, Context1) -> Context2
event(#postback{message={link, SubjectId, Predicate, ObjectId, ElementId, EdgeTemplate, Action}}, Context) ->
    do_link(SubjectId, Predicate, ObjectId, ElementId, EdgeTemplate, Action, Context).


do_link(SubjectId, Predicate, ObjectId, EdgeTemplate, Context) ->
    do_link(SubjectId, Predicate, ObjectId, undefined, EdgeTemplate, [], Context).

do_link(SubjectId, Predicate, ObjectId, ElementId, EdgeTemplate, Action, Context) ->
    case z_acl:rsc_editable(SubjectId, Context) of
        true ->
            case m_edge:get_id(SubjectId, Predicate, ObjectId, Context) of
                undefined ->
                    {ok, EdgeId} = m_edge:insert(SubjectId, Predicate, ObjectId, Context),
                    Vars = [
                            {subject_id, SubjectId},
                            {predicate, Predicate},
                            {object_id, ObjectId},
                            {edge_id, EdgeId}
                           ],
                    Html  = z_template:render(case EdgeTemplate of undefined -> "_rsc_edge.tpl"; _ -> EdgeTemplate end,
                                              Vars,
                                              Context),
                    Title = z_html:strip(?__(m_rsc:p(ObjectId, title, Context), Context)),
                    ElementId1 = case ElementId of
                                     undefined -> "links-"++z_convert:to_list(SubjectId)++"-"++z_convert:to_list(Predicate);
                                     _ -> ElementId
                                 end,
                    Context1 = z_render:insert_bottom(ElementId1, Html, Context),
                    z_render:wire([{growl, [{text, "Added the connection to “"++z_convert:to_list(Title)++"”."}]} | Action], Context1);
                _ ->
                    z_render:growl_error("This connection does already exist.", Context)
            end;
        false ->
            z_render:growl_error("Sorry, you have no permission to add the connection.", Context)
    end.
    
