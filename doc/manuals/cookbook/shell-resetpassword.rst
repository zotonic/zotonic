Reset a user's password
=======================

Emergency password reset when you can't get into the admin interface.

Why
---

Sometimes it happens that you want to reset an user's password from
the Erlang shell.

Assumptions
-----------

Readers are expected to be familiar with the EShell for running Erlang code interactively.

How
---

You can do this from the Erlang shell without using the /admin or the reset-password mail/dialog.

First go to the Erlang shell::

  marc$ ./bin/zotonic shell 

And then from the Erlang command prompt::

  (zotonic@host)1> m_identity:set_username_pw(1234, "username", "password", z:c(yoursitename)). 

Where `1234` is the id of your user (this must be an integer), ``yoursitename`` is the name of your site.

Troubleshooting
---------------
If you get the error::

  {error, admin_password_cannot_be_set}

That means you are trying to change the password for the admin (user
1). The admin password is not set in the database: you need to define
your admin password in the site's config file, use the property
``admin_password``. For more info on this, see
:ref:`tutorial-site-anatomy`.

