%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2023 Marc Worrell
%% @doc List all mailing lists, enable adding and deleting mailing lists.
%% @end

%% Copyright 2009-2023 Marc Worrell
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
-moduledoc("
This controller shows the mailing lists that are available in the system.

For each list, it shows the number of recipients and the title. Clicking a list shows the
[recipients](/id/doc_controller_controller_admin_mailinglist_recipients) of the mailing list.

Todo

Extend documentation
").
-author("Marc Worrell <marc@worrell.nl>").

-export([
    service_available/1,
    is_authorized/1,
    process/4,
	event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

service_available(Context) ->
    Context1 = z_context:set_noindex_header(Context),
    Context2 = z_context:set_nocache_headers(Context1),
    {true, Context2}.

is_authorized(Context) ->
    z_controller_helper:is_authorized([ {use, z_context:get(acl_module, Context, mod_mailinglist)} ], Context).


process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
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
