%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-26
%% @doc Adds typeahead with a searchresult to an input box

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

-module(action_base_typeselect).
-author("Marc Worrell <marc@worrell.nl").
-include("zotonic.hrl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

render_action(TriggerId, TargetId, Args, Context) ->
    Actions = proplists:get_all_values(action, Args),
    ActionsWithId = proplists:get_all_values(action_with_id, Args),
    Cats = proplists:get_all_values(cat, Args),
    Template = proplists:get_value(template, Args, "_action_typeselect_result.tpl"),
    OtherArgs = proplists:delete(action, 
                    proplists:delete(action_with_id, 
                        proplists:delete(cat, 
                            proplists:delete(template, Args)))),
    Postback = {typeselect, Cats, Template, Actions, ActionsWithId, OtherArgs},
    {_PostbackMsgJS, PickledPostback} = z_render:make_postback(Postback, key, TriggerId, TargetId, ?MODULE, Context),
    JS = [
        <<"z_typeselect(\"">>, TriggerId, $",$,,$", PickledPostback, <<"\");">>
    ],
    {JS, Context}.


%% @doc Show possible completions of the search text using a template.
%% @spec event(Event, Context1) -> Context2
event(#postback{message={typeselect, Cats, Template, Actions, ActionsWithId, OtherArgs}, target=TargetId}, Context) ->
    Text = z_context:get_q("triggervalue", Context),
    Props = [{cat,Cats}, {text, Text}],
    Result = z_search:search({autocomplete, Props}, {1,20}, Context),
    M = #m{
        model=m_search,
        value=#m_search_result{result=Result, total=20, search_name=autocomplete, search_props=Props}
    },
    Vars = [
        {result, M},
        {action, Actions},
        {action_with_id, ActionsWithId}
        | OtherArgs
    ],
    Html = z_template:render(Template, Vars, Context),
    z_render:update(TargetId, Html, Context).
