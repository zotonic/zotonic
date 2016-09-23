
.. include:: meta-mod_signup.rst

This module presents an interface for letting users register
themselves.

Notifications
-------------

``signup_form_fields``
  Fold for determining which signup fields to validate. This is an
  array of ``{Fieldname, Validate}`` tuples, defaulting to ``[{email,
  true}, {name_first, true}, {name_surname_prefix, false},
  {name_surname, true}]``. Observers can add / remove fields using the
  accumulator value that is passed into the notification.

``identify_verification{user_id=UserId, identity=Ident}``
  Sent verification requests to non verified identities.

``signup_check``
  Fold for the signup preflight check. Allows to add extra user properties or abort the signup.

  If no ``{ok, _Props1, SignupProps}`` is returned, but ``{error,
  Reason}``, the signup is aborted.

``signup_done{id=Id, is_verified=IsVerified, props=Props, signup_props=SignupProps}``
  Fired when a signup procedure is done and a user has been created.

``signup_confirm{id=UserId}``
  Fired when a user has signed up and confirmed his identity (e.g. over email)

``signup_confirm_redirect{id=UserId}``
  Decide to which page a user gets redirected to after signup


Config: Disabling confirmation email
------------------------------------

Set the configuration value ``mod_signup.request_confirm`` to
``false`` to disable the signup confirmation process.


Config: Using the user’s e-mail address as username
---------------------------------------------------

By setting a configuration value, it is possible to use the entered
email address as the username.

Set the configuration value ``mod_signup.username_equals_email`` to ``true``.

This makes the username equal to the email address, so that the user
can log in using his email address instead of a separate user
name. Note that when you allow a user to change his email, take care
to update the ``{username_pw, {Username, Password}}`` identity as
well, otherwise the username remains equal to the old email address.


Config: setting the category for new users
------------------------------------------

By default, users created through the signup process will become
:term:`resources <resource>` of the category `person`. This can be
changed by setting the configuration value
``mod_signup.member_category`` to the name of a different category.

.. _mod-signup-new-users-content-group:

Config: setting the content group for new users
-----------------------------------------------

By default, users created through the signup process will become
:term:`resources <resource>` in the content group `default_content_group `. This can be
changed by setting the configuration value
``mod_signup.content_group`` to the name of a different content group.


.. todo:: Add more documentation



