%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example contact-form handler.

-module(resource_default_contact).
-export([event/2]).

-include_lib("zotonic.hrl").

event(#submit{message={contact, []}}, Context) ->
    Vars = [{mail, z_context:get_q("mail", Context)},
            {name, z_context:get_q("name", Context)},
            {message, z_context:get_q("message", Context)}],
    z_email:send_render(z_email:get_admin_email(Context), "_email_contact.tpl", Vars, Context),
    z_render:update("contact-form", "<p>The form has been submitted! Thank you, we'll get in touch soon.</p>", Context).
