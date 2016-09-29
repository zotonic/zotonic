
.. include:: meta-mod_signup.rst

This module presents an interface for letting users register
themselves.

Configuration
-------------

You can adjust this module’s behaviour with the following
:ref:`dev-configuration-parameters`:

.. attribute:: mod_signup.request_confirm

    true (default)
        send a signup confirmation e-mail to new users

    false
        disable the signup confirmation e-mail

.. attribute:: mod_signup.username_equals_email

    false (default)
        users have a username separate from their e-mail address and use that
        username for logging in

    true
        the user’s e-mail address is also the user’s username, so users can
        log in with their e-mail address.

.. attribute:: mod_signup.member_category

    Name of the category that users created through sign up will be placed in.

    Defaults to ``person``.

.. _mod-signup-new-users-content-group:

.. attribute:: mod_signup.content_group

    Name of the content group that users created through sign up will be placed
    in.

    Defaults to ``default_content_group``.



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


Notifications
-------------

``signup_form_fields``
^^^^^^^^^^^^^^^^^^^^^^

Fold for determining which signup fields to validate. This is an
array of ``{Fieldname, Validate}`` tuples, defaulting to::

    [
        {email, true},
        {name_first, true},
        {name_surname_prefix, false},
        {name_surname, true}
    ]

Observers can add / remove fields using the accumulator value that is passed
into the notification.


``identify_verification{user_id=UserId, identity=Ident}``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Send verification requests to unverified identities.

``signup_check``
^^^^^^^^^^^^^^^^

Fold for the signup preflight check. Allows to add extra user properties or
abort the signup.

If no ``{ok, _Props1, SignupProps}`` is returned, but ``{error, Reason}``, the
signup is aborted.

``signup_done{id=Id, is_verified=IsVerified, props=Props, signup_props=SignupProps}``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Fired when a signup procedure is done and a user has been created.

``signup_confirm{id=UserId}``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Fired when a users have signed up and confirmed their identity (e.g. via e-mail).

``signup_confirm_redirect{id=UserId}``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Decide to which page a user gets redirected to after signup.

.. todo:: Add more documentation
