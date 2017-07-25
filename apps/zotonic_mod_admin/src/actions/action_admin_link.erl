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

-include_lib("zotonic_core/include/zotonic.hrl").

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
event(#postback{message={link, SubjectId, Predicate, ObjectId, ElementId, EdgeTemplate, Actions}}, Context) ->
    do_link(SubjectId, Predicate, ObjectId, ElementId, EdgeTemplate, Actions, Context).


do_link(SubjectId, Predicate, ObjectId, EdgeTemplate, Context) ->
    do_link(SubjectId, Predicate, ObjectId, undefined, EdgeTemplate, [], Context).

do_link(SubjectId, Predicate, ObjectId, ElementId, EdgeTemplate, Actions, Context) ->
    case z_acl:rsc_linkable(SubjectId, Context) of
        true ->
            case m_edge:get_id(SubjectId, Predicate, ObjectId, Context) of
                undefined ->
                    {ok, EdgeId} = m_edge:insert(SubjectId, Predicate, ObjectId, Context),
                    Context1 = case EdgeTemplate of
                        undefined ->
                            Context;
                        _ ->
                            Vars = [
                                    {subject_id, SubjectId},
                                    {predicate, Predicate},
                                    {object_id, ObjectId},
                                    {edge_id, EdgeId}
                                   ],
                            Html  = z_template:render(EdgeTemplate,
                                                      Vars,
                                                      Context),
                            ElementId1 = case ElementId of
                                             undefined -> "links-"++z_convert:to_list(SubjectId)++"-"++z_convert:to_list(Predicate);
                                             _ -> ElementId
                                         end,
                            z_render:insert_bottom(ElementId1, Html, Context)
                    end,
                    Title = m_rsc:p(ObjectId, title, Context),
                    z_render:wire([{growl, [{text, [?__("Added the connection to", Context), <<" \"">>, Title, <<"\".">>]}]}
                                            | Actions], Context1);
                _ ->
                    z_render:growl_error(?__("This connection already exists.", Context), Context)
            end;
        false ->
            z_render:growl_error(?__("Sorry, you don't have permission to add the connection.", Context), Context)
    end.

