%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-05-12
%% @doc Open a dialog with some fields to make a new predicate.

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

-module(action_admin_predicate_dialog_predicate_new).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Title = proplists:get_value(title, Args),
    Redirect = proplists:get_value(redirect, Args, true),
    Postback = {predicate_new_dialog, Title, Redirect},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the new page form. The form will be posted back to this module.
%% @spec event(Event, Context1) -> Context2
event(#postback{message={predicate_new_dialog, Title, Redirect}}, Context) ->
    Vars = [
        {delegate, atom_to_list(?MODULE)},
        {redirect, Redirect },
        {title, Title}
    ],
    z_render:dialog("Make a new predicate", "_action_dialog_predicate_new.tpl", Vars, Context);


event(#submit{message=predicate_new}, Context) ->
    Title    = z_context:get_q("new_predicate_title", Context),
    Redirect = z_context:get_q("redirect", Context),
    {ok, Id} = m_predicate:insert(Title, Context),

    % Close the dialog and optionally redirect to the edit page of the new resource
    Context2 = z_render:wire({dialog_close, []}, Context),
    case z_convert:to_bool(Redirect) of
        false ->
            Context2;
        true ->
            Location = z_dispatcher:url_for(admin_edit_rsc, [{id, Id}], Context2),
            z_render:wire({redirect, [{location, Location}]}, Context2)
    end.

