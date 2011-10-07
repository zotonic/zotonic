%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc List all mailing lists, enable adding and deleting mailing lists.

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

-module(resource_admin_mailinglist).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_authorized/2,
	event/2
]).

-include_lib("resource_html.hrl").

is_authorized(ReqData, Context) ->
    z_acl:wm_is_authorized(use, mod_mailinglist, ReqData, Context).


html(Context) ->
    Vars = [
        {page_admin_mailinglist, true}
    ],
	Html = z_template:render("admin_mailinglist.tpl", Vars, Context),
	z_context:output(Html, Context).

event({postback, {mailinglist_delete_confirm, [{id,Id}]}, _TriggerId, _TargetId}, Context) ->
	Vars = [
		{id, Id}
	],
	z_render:dialog("Delete mailing list.", "_dialog_mailinglist_delete_confirm.tpl", Vars, Context);

event({postback, {mailinglist_delete, [{id,Id}]}, _TriggerId, _TargetId}, Context) ->
	case m_rsc:delete(Id, Context) of
		ok ->
			z_render:wire([	{growl, [{text, "Deleted the mailing list."}]}, 
							{slide_fade_out, [{target,"mailinglist-"++z_convert:to_list(Id)}]},
							{dialog_close, []}], Context);
		{error, eacces} ->
			z_render:wire([	{growl, [{text, "You are not allowed to delete the mailing list."}, {type, "error"}]}, 
							{dialog_close, []}], Context);
		{error, _Reason} ->
			z_render:wire([	{growl, [{text, "Could not delete the mailing list."}, {type, "error"}]}, 
							{dialog_close, []}], Context)
	end.
