
.. include:: meta-mod_authentication.rst

This module contains the main Zotonic authentication mechanism. It
contains the logon and logoff controllers, and implements the various
hooks as described in the :ref:`guide-auth` manual.

Configuration keys:

mod_authentication.password_min_length
  The minimumum length of passwords. Defaults to 8, set this to an integer value.

mod_authentication.is_rememberme
  Set this to ``1`` to check the *remember me* checkbox per default

mod_authentication.is_one_step_logon
  Normally a two-step logon is used, first the username is requested, then the
  password is requested. In between the server checks the username and is able
  to show alternative authentication methods based on the username.
  Set this to ``1`` to show the username and password field at once, and disable
  the display of alternative authentication methods.

mod_authentication.is_signup_confirm
  Set to ``1`` to force user confirmation of new accounts. This is useful when
  using 3rd party authentication services. If a new identity is found then
  a new account is automatically added. With this option set the user will be
  asked if they want to make a new account. This prevents duplicate accounts
  when using multiple authentication methods.

mod_authentication.reset_token_maxage
  The maximum age of the emailed reset token in seconds. Defaults to 48 hours
  (172800 seconds). This must be an integer value.

mod_authentication.email_reminder_if_nomatch
  On the password reset form, an user can enter their email address for receiving
  an email to reset their password. If an user enters an email address that is not
  connected to an active account then we do not send an email.
  If this option is set to ``1`` then an email is sent. This prevents the user waiting
  for an email, but enables sending emails to arbitrary addresses.

mod_authentication.auth_secret
  The secret used to sign authentication cookies. This secret is automatically generated.
  Changing this secret will invalidate all authentication cookies.

mod_authentication.auth_anon_secret
  The secret to sign authentication cookies for the anonymous user. This secret is automatically
  generated. Changing this secret will invalidate all authentication cookies for anonymous users.

mod_authentication.auth_user_secret
  The secret to sign authentication cookies for the identified users if there is no database
  to store individual secrets.

mod_authentication.auth_autologon_secret
  The secret to sign *remember me* cookies. This secret is automatically generated. Changing this
  secret will invalidate all *remember me* cookies for all users.


Related configurations:

site.password_force_different
  Set to ``1`` to force an user picking a different password if they reset their password.

site.ip_allowlist
  If the admin password is set to ``admin`` then logon is only allowed from local IP addresses.
  This configuration overrules the ``ip_allowlist`` global configuration and enables other IP
  addresses to login as ``admin`` if the password is set to ``admin``.

