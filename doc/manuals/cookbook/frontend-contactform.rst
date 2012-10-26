.. _manual-cookbook-frontend-contactform:

Implementing a simple contact form
----------------------------------

This tutorial teaches you to create a form, validate it, submit it
over Ajax and e-mail the results back to you.

Why
...

Making a simple contact form might seem difficult, but with the smart
application of different Zotonic techniques you'll see that it's
actually very easy.﻿

1. Create the contact page URL dispatcher and template
2. Create the contact form
3. Create the contact-form handler Erlang file.

Assumptions
...........   

Readers are assumed to be comfortable both in developing Zotonic
templates and in writing Erlang modules.

How
...

Create the contact page URL dispatcher and template

The URL dispatcher is placed in ``priv/sites/yoursite/dispatch/dispatch``. Add this line::

  {contact_url, ["contact"], controller_template, [ {template, "contact.tpl"} ]},

This says that the page at "/contact" will use the "contact.tpl" template. Let's create this template, at ``priv/sites/yoursite/templates/contact.tpl``::

  {% extends "base.tpl" %}

  {% block content %}
  <h1>Contact page</h1>
  {% endblock %}

Now we have this, let's try to see if it loads. Flush the Zotonic
cache (to refresh the URL dispatchers) by going to "modules" ->
"rescan modules" in the admin. Now, point your browser to
http://yoursite:8000/contact. This should show the contact page with the
template you just made.

Create the contact form
^^^^^^^^^^^^^^^^^^^^^^^

Now you should write the acual contact form. You should decide what
fields you want in the form, so for now, just put a name, e-mail and
comment field::

  {% wire id="contact-form" type="submit" postback={contact} delegate="resource_default_contact" %}
  <form id="contact-form" method="post" action="postback">

    <label for="name">Name</label>
    <input type="text" name="name" id="name" />

    <label for="email">E-mail</label>
    <input type="text" name="mail" id="mail" />
    {% validate id="mail" type={presence} type={email} %}

    <label for="message">Message</label>
    <textarea name="message" id="message" cols="60" rows="8"></textarea>
    {% validate id="message" type={presence} %}

    <input type="submit" value="Send" />
  </form>

This form has 3 fields, of which the message and the e-mail are required, and the e-mail input has to contain a valid e-mail address. The name field is optional.
  
Create the contact-form handler Erlang file
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

As you see in the :ref:`scomp-wire` statement in the contact form, the
`delegate` argument is set to ``resource_default_contact``, which is
the name of an erlang module which we still have to create. When the
form submits, this module's event/2 function gets called.Create a file
``priv/sites/default/resources/resource_default_contact.erl`` with the
following contents::

  -module(resource_default_contact).
  -export([event/2]).

  -include_lib("zotonic.hrl").

  event({submit, {contact, []}, _TriggerId, _TargetId}, Context) ->
    ?DEBUG(z_context:get_q_all(Context)),
    Context.

This is the most simple version of a Zotonic form-handling function:
it just dumps all form input it gets to the Zotonic console (using the
?DEBUG macro). To compile this Erlang module, enter the following on
the zotonic console::

  z:m().

You'll see a notice that the file is recompiled, which should end with
a ``ok`` message to indicate the success. This compiling is actually
very important: Whenever you change the .erl file, you'll need to
recompile it using this command.

E-mail the contents of the contact form to somebody
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Using Zotonic's email module, you can very easily send somebody an
e-mail. Let's create a simple template to send the contents of the
form to the site administrator.

Create the file ``priv/sites/default/templates/_email_contact.tpl``::

  <html>
    <head>
      <title>Contact form</title>
    </head>
    <body>
      <p>Hello, the contact form of the site has been submitted.</p>
      <p>Name: {{ name|escape }}</p>
      <p>E-mail: {{ mail|escape }}</p>
      <p>The contents of the message was this:</p>
      <pre>{{ message|escape }}</pre>
      <p>Regards, your website.</p>
    </body>
  </html>

This template will function as the message body that will be
sent. Note: this template gets scanned for the <title> tag, which will
double as the e-mail's subject, so be sure to include it!

Now we have to change our ``event/2`` function to render this template and
e-mail it using mod_emailer. Change the event function to the
following::

  event({submit, {contact, []}, _TriggerId, _TargetId}, Context) ->
    Vars = [{mail, z_context:get_q("mail", Context)},
            {name, z_context:get_q("name", Context)},
            {message, z_context:get_q("message", Context)}],
    z_email:send_render(z_email:get_admin_email(Context), "_email_contact.tpl", Vars, Context),
    z_render:update("contact-form", "<p>The form has been submitted! Thank you, we'll get in touch soon.</p>", Context).

This loads the relevant values from the form, puts them in the Vars
variable, and then calls the z_email module to mail the given template
to the e-mail address of the site admin (which is defined in your
site's config file). For more information on sending mails from
Zotonic, please see the mod_emailer documentation.

Finally, this contact-form handler replaces the contact form with a
``<p>`` tag with a success message, using the ``z_render:update``
function.
