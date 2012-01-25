%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-07-10
%% @doc Open a dialog that asks confirmation to delete a category. Optionaly move pages to another category.

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

-module(action_admin_category_dialog_category_delete).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Id = z_convert:to_integer(proplists:get_value(id, Args)),
    OnSuccess = proplists:get_all_values(on_success, Args),
    Postback = {delete_category_dialog, Id, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the delete confirmation template. The next step will ask to delete the category.
%% @spec event(Event, Context1) -> Context2
event(#postback{message={delete_category_dialog, Id, OnSuccess}}, Context) ->
    case z_acl:is_allowed(delete, Id, Context) of
        true ->
            Count = m_category:get_page_count(Id, Context),
            Vars = [ {on_success, OnSuccess}, {id, Id}, {page_count, Count} ],
            z_render:dialog("Confirm delete", "_action_dialog_category_delete.tpl", Vars, Context);
        false ->
            z_render:growl_error("You are not allowed to delete this category.", Context)
    end;

%% @doc Handle the form postback. Optionally renaming existing categories.
event(#submit{message={delete_category, _Props}}, Context) ->
    Id = z_convert:to_integer(z_context:get_q("id", Context)),
    case z_acl:is_allowed(delete, Id, Context) of
        true ->
            TransferId = case z_context:get_q("transfer_id", Context) of
                [] -> undefined;
                undefined -> undefined;
                T -> list_to_integer(T)
            end,
            case m_category:delete(Id, TransferId, Context) of
                ok ->
                    %% Page refresh, good enough for the current usage.
                    z_render:wire({reload, []}, Context);
                {error, Reason} ->
                    Error = io_lib:format("Could not delete the categorie (~p)", [Reason]),
                    z_render:growl_error(Error, Context)
            end;
        false ->
            z_render:growl_error("You are not allowed to delete this category.", Context)
    end.
