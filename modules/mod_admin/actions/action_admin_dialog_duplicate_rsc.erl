%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-07-21
%% @doc Duplicate a resource, replace the title with the one entered in a dialog.

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

-module(action_admin_dialog_duplicate_rsc).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Id = z_convert:to_integer(proplists:get_value(id, Args)),
    Postback = {duplicate_rsc_dialog, Id},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the duplicate page form. The form will be posted back to this module.
%% @spec event(Event, Context1) -> Context2
event(#postback{message={duplicate_rsc_dialog, Id}}, Context) ->
    Vars = [
        {delegate, atom_to_list(?MODULE)},
        {id, Id}
    ],
    z_render:dialog("Duplicate page.", "_action_dialog_duplicate_rsc.tpl", Vars, Context);


event(#submit{message={duplicate_page, ActionProps}}, Context) ->
    Id = proplists:get_value(id, ActionProps),
    Title   = z_context:get_q("new_rsc_title", Context),
    IsPublished = z_context:get_q("is_published", Context),

    Props = [
        {title, Title},
        {is_published, IsPublished}
    ],
    {ok, NewId} = m_rsc:duplicate(Id, Props, Context),

    % Close the dialog and redirect to the edit page of the new resource
    Context1 = z_render:wire({dialog_close, []}, Context),
    Location = z_dispatcher:url_for(admin_edit_rsc, [{id, NewId}], Context1),
    z_render:wire({redirect, [{location, Location}]}, Context1).
