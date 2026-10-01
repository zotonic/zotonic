%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2023 Marc Worrell
%% @doc List all mailing lists, enable adding and deleting mailing lists.
%% @end

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

-module(controller_admin_mailinglist_recipients).
-moduledoc(#{
    zotonic_keywords => ["reference", "backend_developer", "controller", "mailing_lists", "query", "edit"]
}).
-moduledoc("
Shows the recipients of the current mailing list. The recipients are listed in three columns, and have a checkbox next
to them to deactivate them.

Clicking a recipient shows a popup with information about the recipient, where you can edit the e-mail address and the
recipient’s name details.

The page also offers buttons for importing and exporting lists of email addresses.

Handled events
--------------

* `dialog_recipient_add` opens the add-recipient dialog.
* `dialog_recipient_edit` opens the edit-recipient dialog.
* `recipient_is_enabled_toggle` activates or deactivates a recipient.
* `recipient_change_email` updates a recipient's email address.
* `recipient_delete` deletes a recipient and removes it from the list.
* `recipients_clear` removes all recipients from the mailing list.
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
        {page_admin_mailinglist, true},
		{id, m_rsc:rid(z_context:get_q(<<"id">>, Context), Context)}
    ],
	Html = z_template:render("admin_mailinglist_recipients.tpl", Vars, Context),
	z_context:output(Html, Context).

event(#postback{message={dialog_recipient_add, [{id,Id}]}}, Context) when is_integer(Id) ->
    case is_allowed(Id, Context) of
        true ->
        	Vars = [
        		{id, Id},
                {in_admin, true}
        	],
        	z_render:dialog(?__("Add recipient", Context), "_dialog_mailinglist_recipient.tpl", Vars, Context);
        false ->
            z_render:growl(?__("You are not allowed to change recipients", Context), Context)
    end;
event(#postback{message={dialog_recipient_edit, Args}}, Context) ->
    {id, Id} = proplists:lookup(id, Args),
    {recipient_id, RcptId} = proplists:lookup(recipient_id, Args),
    case is_allowed(Id, Context) of
        true ->
        	Vars = [
                {id, Id},
                {recipient_id, RcptId},
                {in_admin, true}
        	],
        	z_render:dialog(?__("Edit recipient", Context), "_dialog_mailinglist_recipient.tpl", Vars, Context);
        false ->
            z_render:growl(?__("You are not allowed to change recipients", Context), Context)
    end;
event(#postback{message={recipient_is_enabled_toggle, [{recipient_id, RcptId}]}, target=Target}, Context) ->
    case is_allowed(Context) of
        true ->
            Recipient = m_mailinglist:recipient_get(RcptId, Context),
            ok = m_mailinglist:recipient_is_enabled_toggle(RcptId, Context),
            Context1 = z_render:wire({toggle_class, [{target, Target}, {class, "unpublished"}]}, Context),
            update_recipient_counts(Recipient, Context1);
        false ->
            z_render:growl(?__("You are not allowed to change recipients", Context), Context)
    end;
event(#postback{message={recipient_change_email, [{recipient_id, RcptId}]}}, Context) ->
    case is_allowed(Context) of
        true ->
            Email = z_context:get_q(<<"triggervalue">>, Context),
            m_mailinglist:update_recipient(RcptId, [{email, Email}], Context),
            z_render:growl(?__("E-mail address updated", Context), Context);
        false ->
            z_render:growl(?__("You are not allowed to change recipients", Context), Context)
    end;
event(#postback{message={recipient_delete, [{recipient_id, RcptId}, {target, Target}]}}, Context) ->
    case is_allowed(Context) of
        true ->
            Recipient = m_mailinglist:recipient_get(RcptId, Context),
            m_mailinglist:recipient_delete_quiet(RcptId, Context),
            Context1 = z_render:wire([
                {growl, [{text, ?__("Recipient deleted.", Context)}]},
                {slide_fade_out, [{target, Target}]}
            ], Context),
            update_recipient_counts(Recipient, Context1);
        false ->
            z_render:growl(?__("You are not allowed to change recipients", Context), Context)
    end;
event(#postback{message={recipients_clear, [{id, Id}]}}, Context) when is_integer(Id) ->
    case is_allowed(Id, Context) of
        true ->
        	m_mailinglist:recipients_clear(Id, Context),
        	z_render:wire([{reload, []}], Context);
        false ->
            z_render:growl(?__("You are not allowed to change recipients", Context), Context)
    end.

is_allowed(Context) ->
    z_acl:is_allowed(use, mod_mailinglist, Context).

is_allowed(Id, Context) when is_integer(Id) ->
    z_acl:rsc_editable(Id, Context)
    orelse z_acl:is_allowed(use, mod_mailinglist, Context);
is_allowed(_Id, _Context) ->
    false.

%% Refresh the summary without disturbing the recipient list or its scroll position.
-spec update_recipient_counts(Recipient, Context) -> z:context() when
    Recipient :: proplists:proplist(),
    Context :: z:context().
update_recipient_counts(Recipient, Context) ->
    ListId = proplists:get_value(mailinglist_id, Recipient),
    z_render:update("mailinglist-recipient-counts", #render{
        template = "_admin_mailinglist_recipient_counts.tpl",
        vars = [{id, ListId}]
    }, Context).
