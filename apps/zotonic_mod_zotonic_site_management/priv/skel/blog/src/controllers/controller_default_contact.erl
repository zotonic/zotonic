%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example contact-form handler.

-module(controller_default_contact).
-moduledoc("
Example contact-form controller from the blog site scaffold.

Handled events
--------------

* `{contact, []}` submits the contact form, emails its contents to the site
  administrator, and replaces the form with a confirmation message.
").
-export([event/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

event(#submit{message={contact, []}}, Context) ->
    Vars = [{mail, z_context:get_q("mail", Context)},
            {name, z_context:get_q("name", Context)},
            {message, z_context:get_q("message", Context)}],
    z_email:send_render(z_email:get_admin_email(Context), "_email_contact.tpl", Vars, Context),
    z_render:update("contact-form", "<p>The form has been submitted! Thank you, we'll get in touch soon.</p>", Context).
