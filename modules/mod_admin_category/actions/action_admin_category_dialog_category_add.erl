%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-07-13
%% @doc Open a dialog to let the user add a new category. Allows to select the parent category, refreshes the category overview.

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

-module(action_admin_category_dialog_category_add).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    OnSuccess = proplists:get_all_values(on_success, Args),
    Postback = {dialog_category_add, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the delete confirmation template. The next step will ask to delete the category.
%% @spec event(Event, Context1) -> Context2
event(#postback{message={dialog_category_add, OnSuccess}}, Context) ->
    case z_acl:is_allowed(insert, #acl_rsc{category=category}, Context) of
        true ->
            Vars = [ {on_success, OnSuccess} ],
            z_render:dialog("Add category", "_action_dialog_category_add.tpl", Vars, Context);
        false ->
            z_render:growl_error("You are not allowed to add categories.", Context)
    end;

%% @doc Handle the form postback. Optionally renaming existing categories.
event(#submit{message={category_add, Options}}, Context) ->
    case z_acl:is_allowed(insert, #acl_rsc{category=category}, Context) of
        true ->
            Title    = z_context:get_q_validated("title", Context),
            Name     = z_context:get_q_validated("name", Context),
            ParentId = z_convert:to_integer(z_context:get_q("category_id", Context, undefined)),
            
            Props = [
                {is_published, true},
                {category, category},
                {name, Name},
                {title, Title}
            ],
            
            case m_rsc:insert(Props, Context) of
                {ok, Id} ->
                    case ParentId of
                        PId when is_integer(PId) ->  m_category:move_below(Id, PId, Context);
                        undefined -> nop
                    end,
                    z_render:wire(proplists:get_all_values(on_success, Options), Context);
                {error, duplicate_name} ->
                    z_render:growl_error("This category exists already. Please use another name.", Context);
                {error, Reason} ->
                    Error = io_lib:format("Could not insert the categorie (~p)", [Reason]),
                    z_render:growl_error(Error, Context)
            end;
        false ->
            z_render:growl_error("You are not allowed to add categories.", Context)
    end.
