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

-module(controller_admin_mailinglist).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_authorized/1,
	event/2
]).

-include_lib("zotonic_core/include/controller_html_helper.hrl").

is_authorized(Context) ->
    z_admin_controller_helper:is_authorized(mod_mailinglist, Context).


html(Context) ->
    Vars = [
        {page_admin_mailinglist, true}
    ],
	Html = z_template:render("admin_mailinglist.tpl", Vars, Context),
	z_context:output(Html, Context).

event(#postback{message={mailinglist_delete_confirm, [{id,Id}]}}, Context) ->
	Vars = [
		{id, Id}
	],
	z_render:dialog("Delete mailing list.", "_dialog_mailinglist_delete_confirm.tpl", Vars, Context);

event(#postback{message={mailinglist_delete, [{id,Id}]}}, Context) ->
	case m_rsc:delete(Id, Context) of
		ok ->
			z_render:wire([	{growl, [{text, ?__("Deleted the mailing list.", Context)}]},
							{slide_fade_out, [{target,"mailinglist-"++z_convert:to_list(Id)}]},
							{dialog_close, []}], Context);
		{error, eacces} ->
			z_render:wire([	{growl, [{text, ?__("You are not allowed to delete the mailing list.", Context)}, {type, "error"}]},
							{dialog_close, []}], Context);
		{error, _Reason} ->
			z_render:wire([	{growl, [{text, ?__("Could not delete the mailing list.", Context)}, {type, "error"}]},
							{dialog_close, []}], Context)
	end.
